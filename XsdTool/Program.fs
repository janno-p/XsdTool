module XsdTool.Program

open Microsoft.CSharp
open System.CodeDom.Compiler
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Xml
open System.Xml.Schema

module Generate =
    let settings = XmlReaderSettings()
    settings.ValidationEventHandler.Add(fun e -> printfn "%s" e.Message)

    let reader = XmlReader.Create(File.OpenRead("AnnaIsikuKvalifikatsioonid.xsd"), settings)

    let doc = XmlSchema.Read(reader, fun _ e -> printfn "%s" e.Message)
    printfn "%s" doc.TargetNamespace

    let elements = [for e in doc.Items -> e]
                   |> Seq.choose (fun e -> match e with
                                           | :? XmlSchemaElement as e -> Some e
                                           | _ -> None)
                   |> Seq.fold (fun (acc : Dictionary<string, XmlSchemaElement>) e ->
                                    acc.Add(e.Name, e)
                                    acc) (Dictionary<_, _>())

    let qn name = XmlQualifiedName(name, doc.TargetNamespace)
    let request = elements.["request"]
    let response = elements.["response"]

[<EntryPoint>]
let main argv =
    printfn "%O" Generate.request
    printfn "%O" Generate.response

    use codeProvider = new CSharpCodeProvider()
    let parameters = CompilerParameters(GenerateExecutable=false, OutputAssembly="AutoGen.dll")
    let results = codeProvider.CompileAssemblyFromSource(parameters, "")
    printfn "%A" ([for str in results.Output -> str] |> Seq.toArray)
    0
