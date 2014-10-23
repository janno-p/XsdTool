module XsdTool.Program

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
open XsdTool.ExtensionsBuilder
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

let printCode (codeUnit: CodeCompileUnit) (codeProvider: #CodeDomProvider) =
    let sb = StringBuilder()
    use writer = new StringWriter(sb)
    codeProvider.GenerateCodeFromCompileUnit(codeUnit, writer, CodeGeneratorOptions())
    printfn "%s" (sb.ToString())

let compileAssembly (codeUnit: CodeCompileUnit) (codeProvider: #CodeDomProvider) =
    let parameters = CompilerParameters(GenerateExecutable=false, OutputAssembly=Settings.AssemblyName)
    let results = codeProvider.CompileAssemblyFromDom(parameters, codeUnit)
    [for str in results.Output -> str] |> Seq.iter (printfn "%s")

[<EntryPoint>]
let main _ =
    let path = Path.GetFullPath(match Settings.XsdSearchPath with | null | "" -> "." | path -> path)

    let assembly = AssemblyDetails.FromConfig(assembliesConfig)

    let codeCompileUnit = CodeCompileUnit()
    codeCompileUnit.ReferencedAssemblies.Add("System.Xml.dll") |> ignore
    codeCompileUnit.ReferencedAssemblies.Add(sprintf "%s\%s.dll" assembliesConfig.probingPath "Etoimik.Xtee") |> ignore
    codeCompileUnit.ReferencedAssemblies.Add(sprintf "%s\%s.dll" assembliesConfig.probingPath assembliesConfig.assembly) |> ignore

    let extensionsNamespace = CodeNamespace(sprintf "%s.Ext" Settings.AssemblyNamespace)
    extensionsNamespace.Types.Add(XmlReader.CreateExtensionClass()) |> ignore
    extensionsNamespace.Types.Add(XmlWriter.CreateExtensionClass()) |> ignore
    codeCompileUnit.Namespaces.Add(extensionsNamespace) |> ignore

    Directory.GetFiles(path, "*.xsd")
    |> Array.choose (BuildCodeNamespace Settings.AssemblyNamespace assembly)
    |> Array.iter (codeCompileUnit.Namespaces.Add >> ignore)

    use codeProvider = new CSharpCodeProvider()

    let execute = printCode codeCompileUnit
    //let execute = compileAssembly codeCompileUnit

    codeProvider |> execute

    0
