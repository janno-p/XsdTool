module XsdTool.ServiceBuilder

open System.CodeDom
open System.Xml
open System.Xml.Schema
open XsdTool.Code
open XsdTool.Xsd

type Element =
    | Choice of typeName: string * elements: Element list
    | ChoiceEntity of typeName: string * elements: Element list
    | Primitive of name: string * sysType: System.Type * writeMethod: string
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

type ExpType = 
    | Top of CodeExpression
    | Node of CodeExpression

module Deserialization =
    let BuildMethods (serviceDetails: ServiceDetails) =
        []

module Serialization =
    let BuildMethods (serviceDetails: ServiceDetails) =
        let nextVariableName =
            let variableCounter = ref 1
            let generateNextName () =
                let result = sprintf "value%d" !variableCounter
                variableCounter := !variableCounter + 1
                result
            generateNextName

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
                let exp =  match argExp with | Top exp -> exp | Node exp -> upcast prop exp name
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
                                           [| declareEnumStatement :> CodeStatement
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
                    yield upcast (prop exp name |> castVariable sysType |> declareVariable sysType variableName)
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
