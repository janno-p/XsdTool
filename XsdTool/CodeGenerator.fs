module XsdTool.CodeGenerator

open System.CodeDom
open System.Collections.Generic
open System.IO
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

    let deserializeMethod = CodeMemberMethod(Name="Deserialize")
    let serializeMethod = CodeMemberMethod(Name="Serialize")

    let targetClass = CodeTypeDeclaration(schema.Id, IsClass=true)
    targetClass.Members.Add(deserializeMethod) |> ignore
    targetClass.Members.Add(serializeMethod) |> ignore

    let codeNamespace = CodeNamespace(assemblyNamespace)
    codeNamespace.Types.Add(targetClass) |> ignore

    let codeCompileUnit = CodeCompileUnit()
    codeCompileUnit.Namespaces.Add(codeNamespace) |> ignore

    codeCompileUnit
