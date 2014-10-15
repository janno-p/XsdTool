module XsdTool.Program

open FSharp.Configuration
open Microsoft.CSharp
open System.CodeDom.Compiler
open System.IO
open XsdTool.CodeGenerator

type Settings = AppSettings<"App.config">

[<EntryPoint>]
let main argv =
    let path = Path.GetFullPath(match Settings.XsdSearchPath with | null | "" -> "." | path -> path)
    let codeUnits = Directory.GetFiles(path, "*.xsd") |> Array.map (BuildCodeUnit)

    use codeProvider = new CSharpCodeProvider()
    let parameters = CompilerParameters(GenerateExecutable=false, OutputAssembly=Settings.AssemblyName)
    let results = codeProvider.CompileAssemblyFromDom(parameters, codeUnits)

    [for str in results.Output -> str] |> Seq.iter (printfn "%s")
    0
