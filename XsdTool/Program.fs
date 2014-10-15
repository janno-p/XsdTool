module XsdTool.Program

open FSharp.Configuration
open Microsoft.CSharp
open System.CodeDom
open System.CodeDom.Compiler
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Xml
open System.Xml.Schema

type Settings = AppSettings<"App.config">

module Generate =
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

    let BuildCodeUnit schemaFile =
        let schema = openSchema schemaFile
        CodeCompileUnit()

[<EntryPoint>]
let main argv =
    let path = Path.GetFullPath(match Settings.XsdSearchPath with
                                | null | "" -> "."
                                | path -> path)

    let codeUnits = Directory.GetFiles(path, "*.xsd") |> Array.map (Generate.BuildCodeUnit)

    use codeProvider = new CSharpCodeProvider()
    let parameters = CompilerParameters(GenerateExecutable=false, OutputAssembly=Settings.AssemblyName)
    let results = codeProvider.CompileAssemblyFromDom(parameters, codeUnits)

    [for str in results.Output -> str] |> Seq.iter (printfn "%s")
    0
