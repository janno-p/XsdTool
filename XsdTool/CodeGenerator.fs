module XsdTool.CodeGenerator

open System.CodeDom
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Schema

let settings = XmlReaderSettings()
settings.ValidationEventHandler.Add(fun e -> eprintfn "%s" e.Message)

let openSchema schemaFile =
    use reader = XmlReader.Create(File.OpenRead(schemaFile), settings)
    XmlSchema.Read(reader, fun _ e -> eprintfn "%s" e.Message)

let findComplexType (qn: XmlQualifiedName) (schema: XmlSchema) =
    if schema.TargetNamespace = qn.Namespace then
        [ for e in schema.Items -> e]
        |> Seq.choose (fun e -> match e with | :? XmlSchemaComplexType as ct -> Some ct | _ -> None)
        |> Seq.find (fun e -> e.Name = qn.Name)
    else failwith "Unknown type %O" qn

let getRequestResponse (schema: XmlSchema) =
    let elements = [for e in schema.Items -> e]
                   |> Seq.choose (fun e -> match e with | :? XmlSchemaElement as e -> Some e | _ -> None)
                   |> Seq.fold (fun (acc: Dictionary<string, XmlSchemaElement>) e -> acc.Add(e.Name, e); acc) (Dictionary<_, _>())
    (elements.["request"], elements.["response"])

type ElementType =
    | SimpleType of System.Type
    | ComplexType of string

let qn ns n = XmlQualifiedName(n, ns)

let (|XmlSchema|_|) (n: XmlQualifiedName): string option =
    if n.Namespace = "http://www.w3.org/2001/XMLSchema" then
        Some n.Name
    else None

let matchType : XmlQualifiedName -> ElementType = function
    | XmlSchema "string" -> SimpleType typeof<string>
    | qn -> failwith <| sprintf "Unable to match type %O!" qn

let createDeserializationMethod (request: XmlSchemaElement) (schema: XmlSchema) =
    let variableIndex = ref 1
    let requestType = schema |> findComplexType request.SchemaTypeName

    let deserializeXmlReaderParameter = CodeParameterDeclarationExpression(typeof<XmlReader>, "reader")

    let deserializeMethod = CodeMemberMethod(Name="Deserialize")
    deserializeMethod.Parameters.Add(deserializeXmlReaderParameter) |> ignore
    deserializeMethod.ReturnType <- CodeTypeReference(typeof<obj[]>)
    deserializeMethod.Attributes <- MemberAttributes.Public ||| MemberAttributes.Static

    let newMembers = List<CodeTypeMember>()
    newMembers.Add(deserializeMethod)

    let varReader = CodeVariableReferenceExpression("reader")

    let createChoiceType (choiceElement : XmlSchemaChoice) =
        let choiceTypeName = match choiceElement.Id with | null | "" -> failwith "Choice type should have `id` declared" | id -> id
        let choiceType = CodeTypeDeclaration(choiceTypeName, IsClass=true)

        let tagEnum = CodeTypeDeclaration("Tag", IsEnum=true, TypeAttributes=TypeAttributes.NestedPrivate)
        choiceType.Members.Add(tagEnum) |> ignore
        choiceType.Members.Add(CodeMemberField(CodeTypeReference("Tag"), "tag", Attributes=MemberAttributes.Private)) |> ignore

        let addedMembers = List<CodeTypeMember>()
        addedMembers.Add(choiceType)

        let choiceTypeItems = [ for item in choiceElement.Items -> item ]
        let fieldTypes = choiceTypeItems
                         |> List.fold (fun (acc: HashSet<_>) item ->
                                match item with
                                | :? XmlSchemaElement as element -> acc.Add(matchType element.SchemaTypeName) |> ignore
                                | :? XmlSchemaSequence as sequence ->
                                    if sequence.Id |> System.String.IsNullOrEmpty then failwith "Sequence type in choice should have `id` declared"
                                    acc.Add(ComplexType sequence.Id) |> ignore
                                | _ -> failwith <| sprintf "Oh no: %O!" item
                                acc)
                            (HashSet<_>())
        fieldTypes
        |> Seq.iteri (fun i et ->
            let ctr = CodeConstructor(Attributes=MemberAttributes.Private)
            ctr.Parameters.Add(CodeParameterDeclarationExpression(CodeTypeReference("Tag"), "tag")) |> ignore
            let name = sprintf "value%d" i
            let tpRef = match et with
                        | SimpleType tp -> CodeTypeReference(tp)
                        | ComplexType name -> CodeTypeReference(name)
            ctr.Parameters.Add(CodeParameterDeclarationExpression(tpRef, "value")) |> ignore
            ctr.Statements.Add(CodeAssignStatement(CodeFieldReferenceExpression(CodeThisReferenceExpression(), "tag"), CodeVariableReferenceExpression("tag"))) |> ignore
            ctr.Statements.Add(CodeAssignStatement(CodeFieldReferenceExpression(CodeThisReferenceExpression(), name), CodeVariableReferenceExpression("value"))) |> ignore
            choiceType.Members.Add(ctr) |> ignore
            choiceType.Members.Add(CodeMemberField(tpRef, name, Attributes=MemberAttributes.Private)) |> ignore)

        let createNewMethod name (tpRef: CodeTypeReference) =
            let createMethod = CodeMemberMethod(Name=(sprintf "New%s" name))
            createMethod.ReturnType <- CodeTypeReference(choiceType.Name)
            createMethod.Parameters.Add(CodeParameterDeclarationExpression(tpRef, "value")) |> ignore
            createMethod.Attributes <- MemberAttributes.Static ||| MemberAttributes.FamilyAndAssembly
            createMethod.Statements.Add(CodeMethodReturnStatement(CodeObjectCreateExpression(choiceType.Name, CodeFieldReferenceExpression(CodeTypeReferenceExpression("Tag"), name), CodeVariableReferenceExpression("value")))) |> ignore
            createMethod

        choiceTypeItems |> List.iter (fun item ->
            match item with
            | :? XmlSchemaElement as element ->
                let tpRef = match matchType element.SchemaTypeName with
                            | SimpleType tp -> CodeTypeReference(tp)
                            | ComplexType name -> CodeTypeReference(name)
                tagEnum.Members.Add(CodeMemberField("Tag", element.Name)) |> ignore
                choiceType.Members.Add(createNewMethod element.Name tpRef) |> ignore
            | :? XmlSchemaSequence as sequence ->
                tagEnum.Members.Add(CodeMemberField("Tag", sequence.Id)) |> ignore
                let sequenceType = CodeTypeDeclaration(sequence.Id, IsClass=true)
                addedMembers.Add(sequenceType)
                choiceType.Members.Add(createNewMethod sequence.Id (CodeTypeReference(sequence.Id))) |> ignore
            | _ -> failwith <| sprintf "Not implemented %O" item)

        let expReaderLocalName = CodePropertyReferenceExpression(varReader, "LocalName")

        let choiceVarName = sprintf "v%d" !variableIndex
        variableIndex := !variableIndex + 1
        deserializeMethod.Statements.Add(CodeVariableDeclarationStatement(choiceType.Name, choiceVarName)) |> ignore
        let rec buildDeserializeStatements (items: XmlSchemaObject list): CodeStatement =
            match items with
            | [] -> upcast CodeThrowExceptionStatement(CodeObjectCreateExpression(CodeTypeReference(typeof<System.Exception>), CodePrimitiveExpression("bla")))
            | x::xs -> match x with
                       | :? XmlSchemaElement as element ->
                            let createChoice = CodeMethodInvokeExpression(CodeTypeReferenceExpression(choiceType.Name), sprintf "New%s" element.Name, CodeMethodInvokeExpression(varReader, "ReadString"))
                            upcast CodeConditionStatement(CodeBinaryOperatorExpression(expReaderLocalName, CodeBinaryOperatorType.IdentityEquality, CodePrimitiveExpression(element.Name)),
                                                          [| CodeAssignStatement(CodeVariableReferenceExpression(choiceVarName), createChoice) :> CodeStatement |],
                                                          [| buildDeserializeStatements xs |])
                       | :? XmlSchemaSequence as sequence ->
                            let firstElement = sequence.Items.[0] :?> XmlSchemaElement
                            upcast CodeConditionStatement(CodeBinaryOperatorExpression(expReaderLocalName, CodeBinaryOperatorType.IdentityEquality, CodePrimitiveExpression(firstElement.Name)),
                                                          [| |],
                                                          [| buildDeserializeStatements xs |])
                       | _ -> failwith <| sprintf "Not implemented %O" x
        deserializeMethod.Statements.Add([ for i in choiceElement.Items -> i] |> List.ofSeq |> buildDeserializeStatements) |> ignore
        addedMembers

    match requestType.Particle with
    | :? XmlSchemaSequence as sequence ->
        for item in sequence.Items do
            match item with
            | :? XmlSchemaChoice as choice -> newMembers.AddRange(createChoiceType choice)
            | _ -> failwith <| sprintf "Not implemented %O" item
    | _ -> failwith <| sprintf "Not implemented %O" requestType.Particle

    deserializeMethod.Statements.Add(CodeMethodReturnStatement(CodeArrayCreateExpression(typeof<obj>, [for i in 1..(!variableIndex - 1) -> CodeVariableReferenceExpression(sprintf "v%d" i) :> CodeExpression] |> List.toArray))) |> ignore

    newMembers

let createSerializationMethod (response: XmlSchemaElement) (schema: XmlSchema) =
    let responseType = schema |> findComplexType response.SchemaTypeName

    let serializeXmlWriterParameter = CodeParameterDeclarationExpression(typeof<XmlWriter>, "writer")
    let serializeObjParameter = CodeParameterDeclarationExpression(typeof<obj>, "obj")

    let serializeMethod = CodeMemberMethod(Name="Serialize")
    serializeMethod.Parameters.Add(serializeXmlWriterParameter) |> ignore
    serializeMethod.Parameters.Add(serializeObjParameter) |> ignore
    serializeMethod.Attributes <- MemberAttributes.Public ||| MemberAttributes.Static

    serializeMethod

let BuildCodeUnit assemblyNamespace schemaFile =
    let schema = openSchema schemaFile
    let request, response = getRequestResponse schema

    let targetClass = CodeTypeDeclaration(schema.Id, IsClass=true)
    targetClass.Members.Add(new CodeConstructor(Attributes=MemberAttributes.Private)) |> ignore;
    createDeserializationMethod request schema |> Seq.iter (targetClass.Members.Add >> ignore)
    targetClass.Members.Add(createSerializationMethod response schema) |> ignore
    targetClass.Attributes <- MemberAttributes.Public ||| MemberAttributes.Static
    targetClass.TypeAttributes <- TypeAttributes.Public ||| TypeAttributes.Sealed

    let codeNamespace = CodeNamespace(assemblyNamespace)
    codeNamespace.Types.Add(targetClass) |> ignore

    let codeCompileUnit = CodeCompileUnit()
    codeCompileUnit.Namespaces.Add(codeNamespace) |> ignore
    codeCompileUnit.ReferencedAssemblies.Add("System.Xml.dll") |> ignore

    codeCompileUnit
