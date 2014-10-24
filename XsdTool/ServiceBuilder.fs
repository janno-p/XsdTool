module XsdTool.ServiceBuilder

open System.CodeDom
open System.Xml
open System.Xml.Schema
open XsdTool.Code
open XsdTool.Xsd

type Element =
    | Choice of typeName: string * elements: Element list
    | ChoiceEntity of typeName: string * elements: Element list
    | Primitive of name: string * sysType: System.Type * methodName: string
    | Array of name: string * item: Element
    | Entity of name: string * sysType: System.Type * elements: Element list

type ServiceDetails =
    { Parameters: Element list
      Result: Element }

let private getRequestResponse serviceName (xsd: XsdDetails) =
    (xsd.GetSchemaType(XmlQualifiedName(sprintf "%sParing" serviceName, xsd.TargetNamespace)) :?> XmlSchemaComplexType,
     xsd.GetSchemaType(XmlQualifiedName(sprintf "%sVastus" serviceName, xsd.TargetNamespace)) :?> XmlSchemaComplexType)

let rec private getParameterRuntimeType (p: Element) =
    match p with
    | Choice (tpName, _) -> CodeTypeReference(tpName)
    | ChoiceEntity (tpName, _) -> CodeTypeReference(tpName)
    | Primitive (_, tp, _) -> CodeTypeReference(tp)
    | Array (_, item) -> getParameterRuntimeType(item)
    | Entity (_, tp, _) -> CodeTypeReference(tp)

let private getElementSysType (e: Element) =
    match e with
    | Primitive (_, tp, _) -> tp
    | Entity (_, tp, _) -> tp
    | _ -> failwithf "Unable to retrieve system runtime type for element %O" e

let ParseServiceDetails serviceName (xsd: XsdDetails) (assembly: AssemblyDetails) =
    let requestType, responseType = xsd |> getRequestResponse serviceName
    let rec getContainedElements (o: XmlSchemaObject) = seq {
        let convertElement (element: XmlSchemaElement) =
            match mapType xsd element.SchemaTypeName with
            | SimpleType (tp, writeMethod) -> Primitive(element.Name, tp, writeMethod)
            | ComplexType schemaType ->
                match getArrayElementType schemaType xsd with
                | Some (arrTp, itemName) -> Array(element.Name, Entity(itemName, assembly.GetRuntimeType(arrTp), getContainedElements arrTp.Particle |> Seq.toList))
                | _ -> Entity(element.Name, assembly.GetRuntimeType(schemaType), getContainedElements schemaType.Particle |> Seq.toList)
        match o with
        | :? XmlSchemaSequence as sequence ->
            for item in sequence.Items do
                match item with
                | :? XmlSchemaElement as element ->
                    yield convertElement element
                | :? XmlSchemaChoice as choice ->
                    let elements =
                        [for item in choice.Items -> item] |> List.map (fun item ->
                            match item with
                            | :? XmlSchemaElement as element -> convertElement element
                            | :? XmlSchemaSequence as sequence ->
                                ChoiceEntity(assembly |> getClassName sequence, getContainedElements sequence |> Seq.toList)
                            | _ -> failwithf "Unhandled choice item: %O!" item
                        )
                    yield Choice(assembly |> getClassName choice, elements)
                | _ -> failwithf "Unhandled request sequence item %O" item
        | :? XmlSchemaGroupRef as groupRef -> yield! xsd.GetGroup(groupRef.RefName).Particle |> getContainedElements
        | _ -> failwithf "Unhandled request particle type %O" requestType.Particle
    }
    let responseSysType = assembly.GetRuntimeType(responseType)
    { Parameters = getContainedElements requestType.Particle |> Seq.toList
      Result = Entity("keha", responseSysType, getContainedElements responseType.Particle |> Seq.toList) }

let buildVariableNameGenerator () =
    let variableCounter = ref 1
    let generateNextName () =
        let result = sprintf "value%d" !variableCounter
        variableCounter := !variableCounter + 1
        result
    generateNextName

module Deserialization =
    type private ExpType = 
        | Top of string
        | Node of CodeExpression

    let BuildMethods (serviceDetails: ServiceDetails) =
        let nextVariableName = buildVariableNameGenerator()

        let methodName sfx = sprintf "Read%s" sfx

        let rec buildStatements (expType: ExpType) (arg: Element) = seq<CodeStatement> {
            match arg with
            | Array(name, element) ->
                let variableName = nextVariableName()
                let itemVariableName = nextVariableName()
                let listType = typedefof<System.Collections.Generic.List<_>>.MakeGenericType(getElementSysType element)
                let varExp = match expType with | Top varName -> variable varName | Node exp -> prop exp name
                let statements = [|
                    createObject listType [] |> declareVariable listType variableName
                    CodeIterationStatement(CodeSnippetStatement(),
                                           CodeSnippetExpression(),
                                           CodeSnippetStatement(),
                                           [ invoke (variable variableName) "Add" [variable itemVariableName] |> asStatement ]
                                                |> Seq.append (buildStatements (Top itemVariableName) element)
                                                |> Seq.toArray
                                          ) :> CodeStatement
                    invoke (variable variableName) "ToArray" [] |> assign varExp
                |]
                yield upcast CodeConditionStatement(invoke (variable "reader") "IsNilElementExt" [] |> equals (primitive false), statements)
            | Choice(typeName, elements) ->
                match expType with
                | Top varName ->
                    yield primitive null |> declareVariableTypeName typeName varName
                    let rec elementsToStatements (names: string list) (elements: Element list) : CodeStatement =
                        match elements with
                        | [] ->
                            let nameList = names |> List.rev |> List.fold (fun acc str -> match acc with | "" -> sprintf "`%s`" str | x -> sprintf "%s; `%s`" x str) ""
                            let messageFormat = sprintf "Expected element with name from list [ %s ] but `{0}` was found." nameList
                            let messageStatement = invoke (typeOf typeof<string>) "Format" [primitive messageFormat; prop (variable "reader") "LocalName"]
                            throwException typeof<System.Exception> [messageStatement]
                        | e::es ->
                            match e with
                            | Primitive(name, sysType, suffix) ->
                                let createChoice = invoke (typeOfName typeName) (sprintf "New%s" name) [invoke (variable "reader") (methodName suffix) []]
                                upcast CodeConditionStatement(equals (prop (variable "reader") "LocalName") (primitive name),
                                                              [| assign (variable varName) createChoice |],
                                                              [| elementsToStatements (name::names) es |])
                            | ChoiceEntity(className, elements) ->
                                let variableName = nextVariableName()
                                let declareVariable = CodeObjectCreateExpression(className) |> declareVariableTypeName className variableName
                                let createChoice = invoke (typeOfName typeName) (sprintf "New%s" className) [variable variableName] |> assign (variable varName)
                                match elements |> List.head with
                                | Primitive(firstName,_,firstSuffix) ->
                                    upcast CodeConditionStatement(equals (prop (variable "reader") "LocalName") (primitive firstName),
                                                                  elements
                                                                  |> List.tail
                                                                  |> Seq.collect (buildStatements (Node(variable variableName)))
                                                                  |> Seq.toArray
                                                                  |> Array.append [| declareVariable; createChoice; assign (prop (variable variableName) firstName) (invoke (variable "reader") (methodName firstSuffix) []) |],
                                                                  [| elementsToStatements (firstName::names) es |])
                                | e -> failwithf "Only primitives allowed. %O" e
                            | _ -> failwithf "Unhandled choice element %O" e
                    yield elementsToStatements [] elements
                | _ -> failwithf "Not implemented %O" expType
            | Entity(name, sysType, elements) ->
                match expType with
                | Top varName ->
                    yield primitive null |> declareVariable sysType varName
                    let createStatement = createObject sysType [] |> assign (variable varName)
                    yield upcast CodeConditionStatement(invoke (variable "reader") "IsNilElementExt" [] |> equals (primitive false),
                                                        elements |> Seq.collect (buildStatements (Node(variable varName))) |> Seq.append [createStatement] |> Seq.toArray)
                | Node exp ->
                    let variableName = nextVariableName()
                    yield primitive null |> declareVariable sysType variableName
                    let createStatement = createObject sysType [] |> assign (prop exp name)
                    yield upcast CodeConditionStatement(invoke (variable "reader") "IsNilElementExt" [] |> equals (primitive false),
                                                        elements |> Seq.collect (buildStatements (Node(variable variableName))) |> Seq.append [createStatement] |> Seq.toArray)
                    yield assign (prop exp name) (variable variableName)
            | Primitive(name, sysType, suffix) ->
                let messageFormat = sprintf "Expected element with name `%s` but `{0}` was found." name
                let messageStatement = invoke (typeOf typeof<string>) "Format" [primitive messageFormat; prop (variable "reader") "LocalName"]
                yield upcast CodeConditionStatement(inequals (prop (variable "reader") "LocalName") (primitive name),
                                                    throwException typeof<System.Exception> [messageStatement])
                let invokeStatement = invoke (variable "reader") (methodName suffix) []
                match expType with
                | Top varName -> yield (invokeStatement |> declareVariable sysType varName)
                | Node exp -> yield (invokeStatement |> assign (prop exp name))
            | _ -> failwithf "Unexpected type tree element %O" arg
        }

        let methodDesReq =
            let meth = CodeMemberMethod(Name="DeserializeRequest", Attributes=(MemberAttributes.Public ||| MemberAttributes.Static))
                       |> addParameter "reader" typeof<XmlReader>
            meth.ReturnType <- CodeTypeReference(typeof<obj[]>)
            let result = System.Collections.Generic.List<_>()
            serviceDetails.Parameters |> List.iteri (fun i p ->
                let argName = sprintf "arg%d" i
                result.Add(argName)
                buildStatements (Top argName) p
                |> Seq.iter (fun e -> meth |> addStatement e |> ignore)
            )
            meth |> addStatement (Some (createArray typeof<obj> (result |> Seq.map (variable) |> Seq.toList))
                                  |> returns)
                 |> ignore
            meth

        let methodDesRes =
            let meth = CodeMemberMethod(Name="DeserializeResponse", Attributes=(MemberAttributes.Public ||| MemberAttributes.Static))
                       |> addParameter "reader" typeof<XmlReader>
            meth.ReturnType <- getParameterRuntimeType serviceDetails.Result
            buildStatements (Top "vastus") serviceDetails.Result
            |> Seq.iter (fun e -> meth |> addStatement e |> ignore)
            meth |> addStatement (Some (variable "vastus") |> returns)

        [ methodDesReq; methodDesRes ]

module Serialization =
    type private ExpType = 
        | Top of CodeExpression
        | Node of CodeExpression

    let BuildMethods (serviceDetails: ServiceDetails) =
        let nextVariableName = buildVariableNameGenerator()

        let rec buildStatements (argExp: ExpType) (arg: Element) = seq<CodeStatement> {
            match arg with
            | Choice (typeName, elements) ->
                // TODO: Fix when nested choices are used
                let exp = match argExp with | Top exp -> exp | Node exp -> exp

                let rec elementsToStatements (elements: Element list) =
                    match elements with
                    | [] -> [||]
                    | e :: es ->
                        match e with
                        | Primitive (name, sysType, writeMethod) ->
                            let variableName = nextVariableName()
                            [| CodeVariableDeclarationStatement(sysType, variableName) :> CodeStatement
                               CodeConditionStatement(invoke exp (sprintf "TryGet%s" name) [variable variableName |> asOutParam],
                                                      [| invoke (variable "writer") writeMethod [primitive name; variable variableName] |> asStatement |],
                                                      elementsToStatements es) :> CodeStatement |]
                        | ChoiceEntity (typeName, elements) ->
                            let variableName = nextVariableName()
                            [| CodeVariableDeclarationStatement(CodeTypeReference(typeName), variableName) :> CodeStatement
                               CodeConditionStatement(invoke exp (sprintf "TryGet%s" typeName) [variable variableName |> asOutParam],
                                                      elements |> Seq.collect (buildStatements (Node(variable variableName))) |> Seq.toArray,
                                                      elementsToStatements es) :> CodeStatement |]
                        | _ -> failwithf "Unhandled choice element %O" e
                yield upcast CodeConditionStatement(equals exp (primitive null),
                                                    [| throwException typeof<System.ArgumentNullException> [primitive typeName] |],
                                                    elementsToStatements elements)
            | Primitive (name, sysType, writeMethod) ->
                let exp =  match argExp with | Top exp -> exp | Node exp -> prop exp name
                yield invoke (variable "writer") writeMethod [primitive name; exp] |> asStatement
            | Array (name, element) ->
                yield invoke (variable "writer") "WriteStartElement" [primitive name] |> asStatement
                let makeCondition exp =
                    let sysType = getElementSysType(element)
                    let enumType = typedefof<System.Collections.Generic.IEnumerator<_>>.MakeGenericType(sysType)
                    let enumerableType = typedefof<System.Collections.Generic.IEnumerable<_>>.MakeGenericType(sysType)
                    let enumName = nextVariableName()
                    let declareEnumStatement = invoke (exp |> castVariable enumerableType) "GetEnumerator" [] |> declareVariable enumType enumName
                    let varExp = variable enumName
                    let iterationStatement = CodeIterationStatement(CodeSnippetStatement(),
                                                                    invoke varExp "MoveNext" [],
                                                                    CodeSnippetStatement(),
                                                                    element |> buildStatements (Top (prop varExp "Current")) |> Seq.toArray)
                    CodeConditionStatement(equals exp (primitive null),
                                           [| invoke (variable "writer") "WriteNilAttributeExt" [] |> asStatement |],
                                           [| declareEnumStatement
                                              iterationStatement :> CodeStatement |])
                match argExp with
                | Top exp ->
                    yield upcast makeCondition exp
                | Node exp ->
                    yield upcast makeCondition (prop exp name)
                yield invoke (variable "writer") "WriteEndElement" [] |> asStatement
            | Entity (name, sysType, elements) ->
                yield invoke (variable "writer") "WriteStartElement" [primitive name] |> asStatement
                let makeCondition exp = CodeConditionStatement(equals exp (primitive null),
                                                               [| invoke (variable "writer") "WriteNilAttributeExt" [] |> asStatement |],
                                                               elements |> Seq.collect (buildStatements (Node exp)) |> Seq.toArray)
                match argExp with
                | Top exp ->
                    yield upcast makeCondition exp
                | Node exp ->
                    let variableName = nextVariableName()
                    yield (prop exp name |> castVariable sysType |> declareVariable sysType variableName)
                    yield upcast makeCondition (variable variableName)
                yield invoke (variable "writer") "WriteEndElement" [] |> asStatement
            | _ -> failwithf "Unexpected type tree element %O" arg
        }

        let addStatement (s: CodeStatement) (m: CodeMemberMethod) = m.Statements.Add(s) |> ignore

        let methodSerReq =
            let meth = CodeMemberMethod(Name="SerializeRequest", Attributes=(MemberAttributes.Public ||| MemberAttributes.Static))
                       |> addParameter "writer" typeof<XmlWriter>
            meth |> addStatement (invoke (variable "writer") "WriteStartElement" [primitive "keha"] |> asStatement)
            serviceDetails.Parameters |> List.iteri (fun i p ->
                let argName = sprintf "arg%d" (i + 1)
                meth.Parameters.Add(CodeParameterDeclarationExpression(getParameterRuntimeType p, argName)) |> ignore
                buildStatements (Node(variable argName)) p
                |> Seq.iter (fun s -> meth |> addStatement s)
            )
            meth |> addStatement (invoke (variable "writer") "WriteEndElement" [] |> asStatement)
            meth

        let methodSerRes =
            let meth = CodeMemberMethod(Name="SerializeResponse", Attributes=(MemberAttributes.Public ||| MemberAttributes.Static))
                       |> addParameter "writer" typeof<XmlWriter>
            meth.Parameters.Add(CodeParameterDeclarationExpression(getParameterRuntimeType serviceDetails.Result, "vastus")) |> ignore
            serviceDetails.Result |> buildStatements (Top(variable "vastus")) |> Seq.iter (meth.Statements.Add >> ignore)
            meth

        [ methodSerReq; methodSerRes ]
