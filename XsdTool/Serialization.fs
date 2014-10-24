module XsdTool.Serialization

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

let ParseServiceDetails serviceName (xsd: XsdDetails) (assembly: AssemblyDetails) =
    let requestType, responseType = xsd |> getRequestResponse serviceName
    let rec getContainedElements (o: XmlSchemaObject) = seq {
        let convertElement (element: XmlSchemaElement) =
            match mapType xsd element.SchemaTypeName with
            | SimpleType (tp, writeMethod) ->
                Primitive(element.Name, tp, writeMethod)
            | ComplexType schemaType ->
                match getArrayElementType schemaType xsd with
                | Some (arrTp, itemName) ->
                    Array(element.Name, Entity(itemName, assembly.GetRuntimeType(arrTp), []))
                | _ ->
                    Entity(element.Name, assembly.GetRuntimeType(schemaType), [])
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
                            | :? XmlSchemaElement as element ->
                                convertElement element
                            | :? XmlSchemaSequence as sequence ->
                                ChoiceEntity(assembly |> getClassName sequence, getContainedElements sequence |> Seq.toList)
                            | _ -> failwithf "Unhandled choice item: %O!" item
                        )
                    yield Choice(assembly |> getClassName choice, elements)
                | _ -> failwithf "Unhandled request sequence item %O" item
        | _ -> failwithf "Unhandled request particle type %O" requestType.Particle
    }
    let responseSysType = assembly.GetRuntimeType(responseType)
    { Parameters = getContainedElements requestType.Particle |> Seq.toList
      Result = Entity("keha", responseSysType, getContainedElements responseType.Particle |> Seq.toList) }

type ExpType = 
    | Top of CodeExpression
    | Node of CodeExpression

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
            match argExp with
            | Top exp -> ()
            | Node exp ->
                let variableName = nextVariableName()
                // TODO :!!!

                yield upcast CodeConditionStatement(equals exp (primitive null),
                                                    [| invoke (variable "writer") "WriteNilAttributeExt" [] |> asStatement |],
                                                    buildStatements argExp element |> Seq.toArray)
            yield invoke (variable "writer") "WriteEndElement" [] |> asStatement
        | Entity (name, sysType, elements) ->
            yield invoke (variable "writer") "WriteStartElement" [primitive name] |> asStatement
            match argExp with
            | Top exp ->
                yield upcast CodeConditionStatement(equals exp (primitive null),
                                                    [| invoke (variable "writer") "WriteNilAttributeExt" [] |> asStatement |],
                                                    elements |> Seq.collect (fun e -> buildStatements (Node(exp)) e) |> Seq.toArray)
            | Node exp ->
                let variableName = nextVariableName()
                yield upcast (prop exp name |> castVariable sysType |> declareVariable sysType variableName)
                yield upcast CodeConditionStatement(equals (variable variableName) (primitive null),
                                                    [| invoke (variable "writer") "WriteNilAttributeExt" [] |> asStatement |],
                                                    elements |> Seq.collect (buildStatements (Node(variable variableName))) |> Seq.toArray)
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


(*

type ComplexTypeContext =
    | SimpleContext of XmlSchemaComplexType
    | ArrayContext of XmlSchemaComplexType * System.Type * string * string

let CreateResponseMethod (xsd: XsdDetails) (assembly: AssemblyDetails) (response: XmlSchemaElement) =
    let responseType = xsd.GetSchemaType(response.SchemaTypeName) :?> XmlSchemaComplexType
    let codeType = assembly.GetRuntimeType(responseType)

    let meth = CodeMemberMethod(Name="Serialize", Attributes=(MemberAttributes.Public ||| MemberAttributes.Static))
               |> addParameter "writer" typeof<XmlWriter>
               |> addParameter "name" typeof<string>
               |> addParameter "obj" codeType

    let addStatement (s: CodeStatement) = meth.Statements.Add(s) |> ignore
    let mapType' = mapType xsd

    let nextVariableName =
        let variableCounter = ref 1
        let generateNextName () =
            let result = sprintf "value%d" !variableCounter
            variableCounter := !variableCounter + 1
            result
        generateNextName

    let rec buildComplexTypeStatements (varExp: CodeExpression) (nameExp: CodeExpression) (typeContext: ComplexTypeContext) = seq<CodeStatement> {
        yield invoke (variable "writer") "WriteStartElement" [nameExp] |> asStatement

        let buildElements (sequence: XmlSchemaSequence) = seq<CodeStatement> {
            for item in sequence.Items do
                match item with
                | :? XmlSchemaElement as element ->
                    match mapType' element.SchemaTypeName with
                    | ComplexType schemaType ->
                        let name = nextVariableName()
                        match getArrayElementType schemaType xsd with
                        | Some (arrTp, itemName) ->
                            let tp = assembly.GetRuntimeType(arrTp)
                            yield upcast (prop varExp element.Name |> declareVariable (tp.MakeArrayType()) name)
                            yield! buildComplexTypeStatements (variable name) (primitive element.Name) (ArrayContext(arrTp, tp, name, itemName))
                        | _ ->
                            let tp = assembly.GetRuntimeType(schemaType)
                            yield upcast (prop varExp element.Name |> castVariable tp |> declareVariable tp name)
                            yield! buildComplexTypeStatements (variable name) (primitive element.Name) (SimpleContext schemaType)
                    | SimpleType writeMethod -> yield invoke (variable "writer") writeMethod [primitive element.Name; prop varExp element.Name] |> asStatement
                | _ -> failwithf "Handling sequence item of type %O is not implemented" (item.GetType())
        }

        let statements =
            seq<CodeStatement> {
                match typeContext with
                | ArrayContext (complexType, runtimeType, name, itemName) ->
                    let etp = typedefof<System.Collections.Generic.IEnumerator<_>>.MakeGenericType(runtimeType)
                    let ename = sprintf "%s_enum" name
                    let vname = nextVariableName()
                    let castStatement = (prop (variable ename) "Current" |> declareVariable runtimeType vname) :> CodeStatement
                    let statements = buildComplexTypeStatements (variable vname) (primitive itemName) (SimpleContext complexType) |> Seq.toArray
                    let getp = typedefof<System.Collections.Generic.IEnumerable<_>>.MakeGenericType(runtimeType)
                    yield upcast (invoke (variable name |> castVariable getp) "GetEnumerator" [] |> declareVariable etp ename)
                    yield upcast CodeIterationStatement(CodeSnippetStatement(),
                                                        invoke (variable ename) "MoveNext" [],
                                                        CodeSnippetStatement(),
                                                        Array.append [| castStatement |] statements)
                | SimpleContext complexType ->
                    match complexType.Particle with
                    | :? XmlSchemaSequence as sequence -> yield! buildElements sequence
                    | :? XmlSchemaGroupRef as groupRef ->
                        let group = xsd.GetGroup(groupRef.RefName)
                        match group.Particle with
                        | :? XmlSchemaSequence as sequence -> yield! buildElements sequence
                        | _ -> failwithf "Only sequences are supported for serialization. Given: %O." group.Particle
                    | _ -> failwithf "Handling particle of type %O is not implemented" (complexType.Particle.GetType())
            } |> Seq.toArray

        yield upcast CodeConditionStatement(CodeBinaryOperatorExpression(varExp,
                                                                         CodeBinaryOperatorType.IdentityEquality,
                                                                         CodePrimitiveExpression(null)),
                                            [| invoke (variable "writer") "WriteNilAttributeExt" [] |> asStatement |],
                                            statements)
        yield invoke (variable "writer") "WriteEndElement" [] |> asStatement
    }

    buildComplexTypeStatements (variable "obj") (variable "name") (SimpleContext responseType)
    |> Seq.iter addStatement

    meth
*)
