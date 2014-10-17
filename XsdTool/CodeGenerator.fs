module XsdTool.CodeGenerator

open System.CodeDom
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Schema
open System.Xml.Serialization

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
    if elements.ContainsKey("request") && elements.ContainsKey("response") then
        Some (elements.["request"], elements.["response"])
    else None

let dtoAssembly = Assembly.Load(@"Etoimik.Web")

type ElementType =
    | SimpleType of System.Type
    | ComplexType of string

let qn ns n = XmlQualifiedName(n, ns)
let nsET = "http://producers.etoimik.xtee.riik.ee/producer/etoimik"

let (|XmlSchema|_|) (n: XmlQualifiedName): string option =
    if n.Namespace = "http://www.w3.org/2001/XMLSchema" then
        Some n.Name
    else None

let matchType = function
    | XmlSchema "string" -> SimpleType typeof<string>
    | XmlSchema "date" -> SimpleType typeof<System.Nullable<System.DateTime>>
    | XmlSchema "long" -> SimpleType typeof<System.Nullable<int64>>
    | qn -> failwith <| sprintf "Unable to match type %O!" qn

let mapMethod = function
    | XmlSchema "string" -> "ReadStringExt"
    | XmlSchema "date" -> "ReadDateTimeExt"
    | qn -> failwith <| sprintf "Unable to match method for type %O!" qn

let mapWriteMethod = function
    | XmlSchema "string" -> "WriteStringExt"
    | XmlSchema "date" -> "WriteDateExt"
    | XmlSchema "dateTime" -> "WriteDateTimeExt"
    | XmlSchema "long" -> "WriteLongExt"
    | qn -> failwith <| sprintf "Unsupported type for serialization %O!" qn

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

        let createPropertyMethods name (tpRef: CodeTypeReference) i =
            let initMethod = CodeMemberMethod(Name=(sprintf "New%s" name))
            initMethod.ReturnType <- CodeTypeReference(choiceType.Name)
            initMethod.Parameters.Add(CodeParameterDeclarationExpression(tpRef, "value")) |> ignore
            initMethod.Attributes <- MemberAttributes.Static ||| MemberAttributes.FamilyAndAssembly
            initMethod.Statements.Add(CodeMethodReturnStatement(CodeObjectCreateExpression(choiceType.Name, CodeFieldReferenceExpression(CodeTypeReferenceExpression("Tag"), name), CodeVariableReferenceExpression("value")))) |> ignore
            let tryMethod = CodeMemberMethod(Name=(sprintf "TryGet%s" name), Attributes=MemberAttributes.Public)
            tryMethod.ReturnType <- CodeTypeReference(typeof<bool>)
            tryMethod.Parameters.Add(CodeParameterDeclarationExpression(tpRef, "value", Direction=FieldDirection.Out)) |> ignore
            tryMethod.Statements.Add(CodeAssignStatement(CodeVariableReferenceExpression("value"), CodePrimitiveExpression(null))) |> ignore
            tryMethod.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(CodeFieldReferenceExpression(CodeThisReferenceExpression(), "tag"),
                                                                                         CodeBinaryOperatorType.IdentityEquality,
                                                                                         CodePropertyReferenceExpression(CodeTypeReferenceExpression("Tag"), name)),
                                                            CodeAssignStatement(CodeVariableReferenceExpression("value"), CodeFieldReferenceExpression(CodeThisReferenceExpression(), sprintf "value%d" i)),
                                                            CodeMethodReturnStatement(CodePrimitiveExpression(true)))) |> ignore
            tryMethod.Statements.Add(CodeMethodReturnStatement(CodePrimitiveExpression(false))) |> ignore
            [ initMethod; tryMethod ]

        let matchMethod = CodeMemberMethod(Name="Match", Attributes=MemberAttributes.Public)
        matchMethod.TypeParameters.Add(CodeTypeParameter("T")) |> ignore
        matchMethod.ReturnType <- CodeTypeReference(CodeTypeParameter("T"))
        choiceType.Members.Add(matchMethod) |> ignore

        choiceTypeItems |> List.iter (fun item ->
            match item with
            | :? XmlSchemaElement as element ->
                let et = matchType element.SchemaTypeName
                let tpRef = match et with
                            | SimpleType tp -> CodeTypeReference(tp)
                            | ComplexType name -> CodeTypeReference(name)
                tagEnum.Members.Add(CodeMemberField("Tag", element.Name)) |> ignore
                let i = (fieldTypes |> Seq.findIndex (fun x -> x = et))
                matchMethod.Parameters.Add(CodeParameterDeclarationExpression(CodeTypeReference("System.Func", tpRef, CodeTypeReference(CodeTypeParameter("T"))), sprintf "f%s" element.Name)) |> ignore
                matchMethod.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(CodeFieldReferenceExpression(CodeThisReferenceExpression(), "tag"),
                                                                                               CodeBinaryOperatorType.IdentityEquality,
                                                                                               CodePropertyReferenceExpression(CodeTypeReferenceExpression("Tag"), element.Name)),
                                                                  CodeMethodReturnStatement(CodeMethodInvokeExpression(null, sprintf "f%s" element.Name, CodeFieldReferenceExpression(CodeThisReferenceExpression(), sprintf "value%d" i))))) |> ignore
                createPropertyMethods element.Name tpRef i |> List.iter (choiceType.Members.Add >> ignore)
            | :? XmlSchemaSequence as sequence ->
                tagEnum.Members.Add(CodeMemberField("Tag", sequence.Id)) |> ignore
                let sequenceType = CodeTypeDeclaration(sequence.Id, IsClass=true)
                addedMembers.Add(sequenceType)
                for item in sequence.Items do
                    match item with
                    | :? XmlSchemaElement as element ->
                        let tpRef = match matchType element.SchemaTypeName with
                                    | SimpleType tp -> CodeTypeReference(tp)
                                    | ComplexType name -> CodeTypeReference(name)
                        sequenceType.Members.Add(CodeMemberField(tpRef, element.Name, Attributes=MemberAttributes.Public)) |> ignore
                    | _ -> failwith <| sprintf "Oh noes again: %O" item
                let i = (fieldTypes |> Seq.findIndex (fun x -> x = ComplexType sequence.Id))
                matchMethod.Parameters.Add(CodeParameterDeclarationExpression(CodeTypeReference("System.Func", CodeTypeReference(sequence.Id), CodeTypeReference(CodeTypeParameter("T"))), sprintf "f%s" sequence.Id)) |> ignore
                matchMethod.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(CodeFieldReferenceExpression(CodeThisReferenceExpression(), "tag"),
                                                                                               CodeBinaryOperatorType.IdentityEquality,
                                                                                               CodePropertyReferenceExpression(CodeTypeReferenceExpression("Tag"), sequence.Id)),
                                                                  CodeMethodReturnStatement(CodeMethodInvokeExpression(null, sprintf "f%s" sequence.Id, CodeFieldReferenceExpression(CodeThisReferenceExpression(), sprintf "value%d" i))))) |> ignore
                createPropertyMethods sequence.Id (CodeTypeReference(sequence.Id)) i
                |> List.iter (choiceType.Members.Add >> ignore)
            | _ -> failwith <| sprintf "Not implemented %O" item)

        matchMethod.Statements.Add(CodeThrowExceptionStatement(CodeObjectCreateExpression(typeof<System.InvalidOperationException>))) |> ignore

        let expReaderLocalName = CodePropertyReferenceExpression(varReader, "LocalName")

        let choiceVarName = sprintf "v%d" !variableIndex
        variableIndex := !variableIndex + 1
        deserializeMethod.Statements.Add(CodeVariableDeclarationStatement(choiceType.Name, choiceVarName)) |> ignore
        let rec buildDeserializeStatements (items: XmlSchemaObject list): CodeStatement =
            match items with
            | [] -> upcast CodeThrowExceptionStatement(CodeObjectCreateExpression(CodeTypeReference(typeof<System.Exception>), CodePrimitiveExpression("bla")))
            | x::xs -> match x with
                       | :? XmlSchemaElement as element ->
                            let createChoice = CodeMethodInvokeExpression(CodeTypeReferenceExpression(choiceType.Name), sprintf "New%s" element.Name, CodeMethodInvokeExpression(varReader, mapMethod element.SchemaTypeName))
                            upcast CodeConditionStatement(CodeBinaryOperatorExpression(expReaderLocalName, CodeBinaryOperatorType.IdentityEquality, CodePrimitiveExpression(element.Name)),
                                                          [| CodeAssignStatement(CodeVariableReferenceExpression(choiceVarName), createChoice) :> CodeStatement |],
                                                          [| buildDeserializeStatements xs |])
                       | :? XmlSchemaSequence as sequence ->
                            let firstElement = sequence.Items.[0] :?> XmlSchemaElement
                            let statements: seq<CodeStatement> = seq {
                                let seqType = CodeTypeReference(sequence.Id)
                                yield upcast CodeVariableDeclarationStatement(seqType, "x", CodeObjectCreateExpression(seqType))
                                for item in sequence.Items do
                                    match item with
                                    | :? XmlSchemaElement as element ->
                                        yield upcast CodeAssignStatement(CodePropertyReferenceExpression(CodeVariableReferenceExpression("x"), element.Name), CodeMethodInvokeExpression(varReader, mapMethod element.SchemaTypeName))
                                    | _ -> failwith <| sprintf "Oh noes: %O" item
                                let createChoice = CodeMethodInvokeExpression(CodeTypeReferenceExpression(choiceType.Name), sprintf "New%s" sequence.Id, CodeVariableReferenceExpression("x"))
                                yield upcast CodeAssignStatement(CodeVariableReferenceExpression(choiceVarName), createChoice)
                            }
                            upcast CodeConditionStatement(CodeBinaryOperatorExpression(expReaderLocalName, CodeBinaryOperatorType.IdentityEquality, CodePrimitiveExpression(firstElement.Name)),
                                                          statements |> Seq.toArray,
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

    let dtoType =
        match responseType.UnhandledAttributes |> Array.tryFind (fun a -> a.NamespaceURI = nsET && a.LocalName = "type") with
        | Some typeName -> typeName.Value
        | _ -> failwith "Unable to resolve dto type"

    let dtoTypePrefix, dtoTypeName = match dtoType.Split(':') with
                                     | [| prefix; name |] -> prefix, name
                                     | _ -> failwith "Invalid name"

    let realType = dtoAssembly.GetTypes()
                   |> Array.find (fun tp -> if tp.Name = dtoTypeName then
                                                tp.GetCustomAttributes(typeof<XmlTypeAttribute>, false)
                                                |> Array.choose (fun a -> match a with | :? XmlTypeAttribute as x -> Some x | _ -> None)
                                                |> Array.filter (fun a -> a.Namespace = nsET)
                                                |> Array.length = 1
                                            else false)
    let codeType = CodeTypeReference(realType)

    let serializeMethod = CodeMemberMethod(Name="Serialize")
    serializeMethod.Parameters.Add(serializeXmlWriterParameter) |> ignore
    serializeMethod.Parameters.Add(serializeObjParameter) |> ignore
    serializeMethod.Attributes <- MemberAttributes.Public ||| MemberAttributes.Static

    serializeMethod.Statements.Add(CodeVariableDeclarationStatement(codeType, "value", CodeCastExpression(codeType, CodeVariableReferenceExpression("obj")))) |> ignore

    let schemaTypes = [for i in schema.Items -> i]
                      |> Seq.choose (fun i -> match i with
                                              | :? XmlSchemaComplexType as ct ->
                                                    Some(XmlQualifiedName(ct.Name, schema.TargetNamespace), ct)
                                              | _ -> None)
                      |> Seq.fold (fun (acc: Dictionary<_,_>) (k, v) -> acc.Add(k, v); acc) (Dictionary<_,_>())

    let schemaGroups = [for i in schema.Items -> i]
                       |> Seq.choose (fun i -> match i with
                                               | :? XmlSchemaGroup as g ->
                                                     Some(XmlQualifiedName(g.Name, schema.TargetNamespace), g)
                                               | _ -> None)
                       |> Seq.fold (fun (acc: Dictionary<_,_>) (k, v) -> acc.Add(k, v); acc) (Dictionary<_,_>())

    let varIndex = ref 1

    let rec buildElementStatements (el: XmlSchemaElement) (exp: CodeExpression) = seq {
        let exp = CodePropertyReferenceExpression(exp, el.Name)
        match schemaTypes.TryGetValue(el.SchemaTypeName) with
        | true, complexType ->
            match complexType.Particle with
            | :? XmlSchemaSequence as sequence ->
                for item in sequence.Items do
                    match item with
                    | :? XmlSchemaElement as element -> yield! buildElementStatements element exp
                    | _ -> failwith <| sprintf "Only elements are supported for serialization. Given: %O." item
            | :? XmlSchemaGroupRef as groupRef ->
                match schemaGroups.TryGetValue(groupRef.RefName) with
                | true, group ->
                    match group.Particle with
                    | :? XmlSchemaSequence as sequence ->
                        for item in sequence.Items do
                            match item with
                            | :? XmlSchemaElement as element -> yield! buildElementStatements element exp
                            | _ -> failwith <| sprintf "Only elements are supported for serialization. Given: %O." item
                    | _ -> failwith <| sprintf "Only sequences are supported for serialization. Given: %O." group.Particle
                | false, _ -> failwith <| sprintf "Invalid group reference %O." groupRef.RefName
            | _ -> failwith <| sprintf "Only sequences or group refs are supported for serialization. Given: %O." complexType.Particle
        | false, _ ->
            yield CodeMethodInvokeExpression(CodeVariableReferenceExpression("writer"), mapWriteMethod el.SchemaTypeName, exp)
    }

    match responseType.Particle with
    | :? XmlSchemaSequence as sequence ->
        for item in sequence.Items do
            match item with
            | :? XmlSchemaElement as element ->
                buildElementStatements element (CodeVariableReferenceExpression "v0")
                |> Seq.iter (serializeMethod.Statements.Add >> ignore)
            | _ -> failwith <| sprintf "Not implemented %O" (item.GetType())
    | _ -> failwith <| sprintf "Not implemented %O" (responseType.Particle.GetType())

    serializeMethod

let BuildCodeUnit assemblyNamespace schemaFile =
    let schema = openSchema schemaFile
    match getRequestResponse schema with
    | Some(request, response) ->
        let targetClass = CodeTypeDeclaration(schema.Id, IsClass=true)
        targetClass.Members.Add(new CodeConstructor(Attributes=MemberAttributes.Private)) |> ignore;
        createDeserializationMethod request schema |> Seq.iter (targetClass.Members.Add >> ignore)
        targetClass.Members.Add(createSerializationMethod response schema) |> ignore
        targetClass.Attributes <- MemberAttributes.Public ||| MemberAttributes.Static
        targetClass.TypeAttributes <- TypeAttributes.Public ||| TypeAttributes.Sealed

        let codeNamespace = CodeNamespace(assemblyNamespace)
        codeNamespace.Types.Add(targetClass) |> ignore
        codeNamespace.Types.Add(XsdTool.CreateXmlReaderExtensions.createClass()) |> ignore

        let codeCompileUnit = CodeCompileUnit()
        codeCompileUnit.Namespaces.Add(codeNamespace) |> ignore
        codeCompileUnit.ReferencedAssemblies.Add("System.Xml.dll") |> ignore

        Some codeCompileUnit
    | _ -> None
