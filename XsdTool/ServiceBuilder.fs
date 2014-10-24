module XsdTool.ServiceBuilder

open System.CodeDom
open System.Reflection
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
            | SimpleType (tp, suffix) -> Primitive(element.Name, tp, suffix)
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

        let methodName sfx = sprintf "Write%s" sfx

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
                        | Primitive (name, sysType, suffix) ->
                            let variableName = nextVariableName()
                            [| CodeVariableDeclarationStatement(sysType, variableName) :> CodeStatement
                               CodeConditionStatement(invoke exp (sprintf "TryGet%s" name) [variable variableName |> asOutParam],
                                                      [| invoke (variable "writer") (methodName suffix) [primitive name; variable variableName] |> asStatement |],
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
            | Primitive (name, sysType, suffix) ->
                let exp =  match argExp with | Top exp -> exp | Node exp -> prop exp name
                yield invoke (variable "writer") (methodName suffix) [primitive name; exp] |> asStatement
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

module Types =
    type private ElementType =
        | SimpleType of System.Type
        | ComplexType of string

    let BuildTypes (serviceDetails: ServiceDetails) =
        let rec collectAllTypes (element: Element) = seq<CodeTypeDeclaration> {
            match element with
            | Choice(typeName, elements) ->
                let choiceType = CodeTypeDeclaration(typeName, IsClass=true)

                let tagEnum = CodeTypeDeclaration("Tag", IsEnum=true, TypeAttributes=TypeAttributes.NestedPrivate)
                choiceType.Members.Add(tagEnum) |> ignore
                choiceType.Members.Add(CodeMemberField(CodeTypeReference("Tag"), "tag", Attributes=MemberAttributes.Private)) |> ignore

                let members =
                    let fieldTypes = System.Collections.Generic.HashSet<_>()
                    let getIndex et =
                        fieldTypes.Add(et) |> ignore
                        fieldTypes |> Seq.findIndex (fun x -> x = et)
                    let collectedMembers =
                        elements |> List.map (fun el ->
                            match el with
                            | Primitive(name, sysType, _) -> (name, getIndex (SimpleType sysType), CodeTypeReference(sysType))
                            | ChoiceEntity(typeName, elements) -> (typeName, getIndex (ComplexType typeName), CodeTypeReference(typeName))
                            | _ -> failwith "Unsupported %O" el)
                    fieldTypes |> Seq.iteri (fun i ft ->
                        let name = sprintf "value%d" i
                        let ctr = CodeConstructor(Attributes=MemberAttributes.Private)
                        ctr.Parameters.Add(CodeParameterDeclarationExpression(CodeTypeReference("Tag"), "tag")) |> ignore
                        let tp = match ft with | SimpleType tp -> CodeTypeReference(tp) | ComplexType name -> CodeTypeReference(name)
                        ctr.Parameters.Add(CodeParameterDeclarationExpression(tp, "value")) |> ignore
                        ctr.Statements.Add(CodeAssignStatement(CodeFieldReferenceExpression(CodeThisReferenceExpression(), "tag"), CodeVariableReferenceExpression("tag"))) |> ignore
                        ctr.Statements.Add(CodeAssignStatement(CodeFieldReferenceExpression(CodeThisReferenceExpression(), name), CodeVariableReferenceExpression("value"))) |> ignore
                        choiceType.Members.Add(ctr) |> ignore
                        choiceType.Members.Add(CodeMemberField(tp, name, Attributes=MemberAttributes.Private)) |> ignore)
                    collectedMembers

                let matchMethod = CodeMemberMethod(Name="Match", Attributes=MemberAttributes.Public)
                matchMethod.TypeParameters.Add(CodeTypeParameter("T")) |> ignore
                matchMethod.ReturnType <- CodeTypeReference(CodeTypeParameter("T"))
                choiceType.Members.Add(matchMethod) |> ignore

                members |> List.iter (fun (name, index, tp) ->
                    let initMethod = CodeMemberMethod(Name=(sprintf "New%s" name), Attributes=(MemberAttributes.Static ||| MemberAttributes.Public))
                    initMethod.ReturnType <- CodeTypeReference(choiceType.Name)
                    initMethod.Parameters.Add(CodeParameterDeclarationExpression(tp, "value")) |> ignore
                    initMethod.Statements.Add(CodeMethodReturnStatement(CodeObjectCreateExpression(choiceType.Name, CodeFieldReferenceExpression(CodeTypeReferenceExpression("Tag"), name), CodeVariableReferenceExpression("value")))) |> ignore
                    choiceType.Members.Add(initMethod) |> ignore

                    let tryMethod = CodeMemberMethod(Name=(sprintf "TryGet%s" name), Attributes=MemberAttributes.Public)
                    tryMethod.ReturnType <- CodeTypeReference(typeof<bool>)
                    tryMethod.Parameters.Add(CodeParameterDeclarationExpression(tp, "value", Direction=FieldDirection.Out)) |> ignore
                    tryMethod.Statements.Add(CodeAssignStatement(CodeVariableReferenceExpression("value"), CodePrimitiveExpression(null))) |> ignore
                    tryMethod.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(CodeFieldReferenceExpression(CodeThisReferenceExpression(), "tag"),
                                                                                                 CodeBinaryOperatorType.IdentityEquality,
                                                                                                 CodePropertyReferenceExpression(CodeTypeReferenceExpression("Tag"), name)),
                                                                    CodeAssignStatement(CodeVariableReferenceExpression("value"), CodeFieldReferenceExpression(CodeThisReferenceExpression(), sprintf "value%d" index)),
                                                                    CodeMethodReturnStatement(CodePrimitiveExpression(true)))) |> ignore
                    tryMethod.Statements.Add(CodeMethodReturnStatement(CodePrimitiveExpression(false))) |> ignore
                    choiceType.Members.Add(tryMethod) |> ignore

                    tagEnum.Members.Add(CodeMemberField("Tag", name)) |> ignore

                    matchMethod.Parameters.Add(CodeParameterDeclarationExpression(CodeTypeReference("System.Func", tp, CodeTypeReference(CodeTypeParameter("T"))), sprintf "f%s" name)) |> ignore
                    matchMethod.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(CodeFieldReferenceExpression(CodeThisReferenceExpression(), "tag"),
                                                                                                   CodeBinaryOperatorType.IdentityEquality,
                                                                                                   CodePropertyReferenceExpression(CodeTypeReferenceExpression("Tag"), name)),
                                                                      CodeMethodReturnStatement(CodeMethodInvokeExpression(null, sprintf "f%s" name, CodeFieldReferenceExpression(CodeThisReferenceExpression(), sprintf "value%d" index))))) |> ignore
                )

                matchMethod.Statements.Add(CodeThrowExceptionStatement(CodeObjectCreateExpression(typeof<System.InvalidOperationException>))) |> ignore

                yield! elements |> Seq.collect (fun el -> match el with | ChoiceEntity _ -> collectAllTypes el | _ -> Seq.empty)

                yield choiceType
            | ChoiceEntity(typeName, elements) ->
                let entityType = CodeTypeDeclaration(typeName, IsClass=true)
                elements |> List.iter (fun el ->
                    match el with
                    | Primitive(name, sysType, _) ->
                        entityType.Members.Add(CodeMemberField(sysType, name, Attributes=MemberAttributes.Public)) |> ignore
                    | _ -> failwithf "Unsupported element type %O" el
                )
                yield entityType
            | _ -> ()
        }

        serviceDetails.Result :: serviceDetails.Parameters
        |> Seq.collect (collectAllTypes)
        |> Seq.toList
