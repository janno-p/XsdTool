﻿module XsdTool.Program

open Microsoft.CSharp
open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Configuration
open System.IO
open System.Reflection
open System.Text
open XsdTool.CodeGenerator
open XsdTool.Configuration
open XsdTool.Xsd

type Settings = FSharp.Configuration.AppSettings<"App.config">

let assembliesConfig = unbox<DtoAssemblies>(ConfigurationManager.GetSection "DtoAssemblies")

AppDomain.CurrentDomain.add_AssemblyResolve(fun _ args ->
    let assemblyName = AssemblyName(args.Name)
    let expectedName = sprintf "%s.dll" assemblyName.Name
    let expectedLocation = Path.Combine(assembliesConfig.probingPath, expectedName)
    match File.Exists(expectedLocation) with
    | true -> Assembly.LoadFrom(expectedLocation)
    | _ -> null)

let printCode (codeUnits: CodeCompileUnit []) (codeProvider: #CodeDomProvider) =
    codeUnits
    |> Array.iter (fun u -> let sb = StringBuilder()
                            use writer = new StringWriter(sb)
                            codeProvider.GenerateCodeFromCompileUnit(u, writer, CodeGeneratorOptions())
                            printfn "%s" (sb.ToString()))

let compileAssembly (codeUnits: CodeCompileUnit []) (codeProvider: #CodeDomProvider) =
    let parameters = CompilerParameters(GenerateExecutable=false, OutputAssembly=Settings.AssemblyName)
    let results = codeProvider.CompileAssemblyFromDom(parameters, codeUnits)
    [for str in results.Output -> str] |> Seq.iter (printfn "%s")

[<EntryPoint>]
let main _ =
    let path = Path.GetFullPath(match Settings.XsdSearchPath with | null | "" -> "." | path -> path)

    let assembly = AssemblyDetails.FromConfig(assembliesConfig)

    let codeUnits = Directory.GetFiles(path, "*.xsd")
                    |> Array.choose (BuildCodeUnit Settings.AssemblyNamespace assembly)

    use codeProvider = new CSharpCodeProvider()

    let execute = printCode codeUnits
    //let execute = compileAssembly codeUnits

    codeProvider |> execute

    0
