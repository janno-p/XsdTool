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

let getRequestResponse (schema: XmlSchema) =
    let elements = [for e in schema.Items -> e]
                   |> Seq.choose (fun e -> match e with | :? XmlSchemaElement as e -> Some e | _ -> None)
                   |> Seq.fold (fun (acc: Dictionary<string, XmlSchemaElement>) e -> acc.Add(e.Name, e); acc) (Dictionary<_, _>())
    (elements.["request"], elements.["response"])

let BuildCodeUnit assemblyNamespace schemaFile =
    let schema = openSchema schemaFile
    let request, response = getRequestResponse schema

    let deserializeXmlReaderParameter = CodeParameterDeclarationExpression(typeof<XmlReader>, "reader")

    let deserializeMethod = CodeMemberMethod(Name="Deserialize")
    deserializeMethod.Parameters.Add(deserializeXmlReaderParameter) |> ignore
    deserializeMethod.ReturnType <- CodeTypeReference(typeof<obj[]>)
    deserializeMethod.Statements.Add(CodeMethodReturnStatement(CodePrimitiveExpression(null))) |> ignore
    deserializeMethod.Attributes <- MemberAttributes.Public ||| MemberAttributes.Static

    let serializeXmlWriterParameter = CodeParameterDeclarationExpression(typeof<XmlWriter>, "writer")
    let serializeObjParameter = CodeParameterDeclarationExpression(typeof<obj>, "obj")

    let serializeMethod = CodeMemberMethod(Name="Serialize")
    serializeMethod.Parameters.Add(serializeXmlWriterParameter) |> ignore
    serializeMethod.Parameters.Add(serializeObjParameter) |> ignore
    serializeMethod.Attributes <- MemberAttributes.Public ||| MemberAttributes.Static

    let targetClass = CodeTypeDeclaration(schema.Id, IsClass=true)
    targetClass.Members.Add(new CodeConstructor(Attributes=MemberAttributes.Private)) |> ignore;
    targetClass.Members.Add(deserializeMethod) |> ignore
    targetClass.Members.Add(serializeMethod) |> ignore
    targetClass.Attributes <- MemberAttributes.Public ||| MemberAttributes.Static
    targetClass.TypeAttributes <- TypeAttributes.Public ||| TypeAttributes.Sealed

    let codeNamespace = CodeNamespace(assemblyNamespace)
    codeNamespace.Types.Add(targetClass) |> ignore

    let codeCompileUnit = CodeCompileUnit()
    codeCompileUnit.Namespaces.Add(codeNamespace) |> ignore

    codeCompileUnit
