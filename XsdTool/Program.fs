module XsdTool.Program

open Microsoft.CSharp
open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Configuration
open System.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Schema
open XsdTool.Configuration
open XsdTool.ExtensionsBuilder
open XsdTool.ServiceBuilder
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

let openSchema schemaFile =
    let settings = XmlReaderSettings()
    settings.ValidationEventHandler.Add(fun e -> eprintfn "%s" e.Message)
    use reader = XmlReader.Create(File.OpenRead(schemaFile), settings)
    XmlSchema.Read(reader, fun _ e -> eprintfn "%s" e.Message)

let BuildCodeNamespace assemblyNamespace assembly schemaFile =
    let schema = openSchema schemaFile
    let schemaTargetNamespace = match schema.TargetNamespace with | null -> "" | x -> x
    match Regex.Match(schemaTargetNamespace, sprintf "^%s/(?<serviceName>\\w+)$" assembly.TargetNamespace) with
    | m when m.Success ->
        let serviceName = m.Groups.["serviceName"].Value
        let xsd = XsdDetails.FromSchema(schema)

        let targetClass = CodeTypeDeclaration(schema.Id, IsClass=true)
        targetClass.Members.Add(new CodeConstructor(Attributes=MemberAttributes.Private)) |> ignore;

        let serviceDetails = ParseServiceDetails serviceName xsd assembly
        serviceDetails |> Deserialization.BuildMethods
                       //|> List.append (serviceDetails |> Serialization.BuildMethods)
                       |> List.iter (targetClass.Members.Add >> ignore)

        targetClass.Attributes <- MemberAttributes.Public ||| MemberAttributes.Static
        targetClass.TypeAttributes <- TypeAttributes.Public ||| TypeAttributes.Sealed

        let codeNamespace = CodeNamespace(assemblyNamespace)
        codeNamespace.Types.Add(targetClass) |> ignore
        codeNamespace.Imports.Add(CodeNamespaceImport(sprintf "%s.Ext" assemblyNamespace))

        Some codeNamespace
    | _ ->
        printfn "Unable to extract service name from targetNamespace `%s` using base namespace `%s`." schemaTargetNamespace assembly.TargetNamespace
        None

[<EntryPoint>]
let main _ =
    let path = Path.GetFullPath(match Settings.XsdSearchPath with | null | "" -> "." | path -> path)

    let assembly = AssemblyDetails.FromConfig(assembliesConfig)

    let codeCompileUnit = CodeCompileUnit()
    codeCompileUnit.ReferencedAssemblies.Add("System.Xml.dll") |> ignore
    codeCompileUnit.ReferencedAssemblies.Add(Path.Combine(assembliesConfig.probingPath, "Etoimik.Xtee.dll")) |> ignore
    codeCompileUnit.ReferencedAssemblies.Add(Path.Combine(assembliesConfig.probingPath, sprintf "%s.dll"  assembliesConfig.assembly)) |> ignore

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
