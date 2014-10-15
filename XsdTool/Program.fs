module XsdTool.Program

open System.CodeDom.Compiler
open System.Diagnostics
open Microsoft.CSharp

[<EntryPoint>]
let main argv =
    use codeProvider = new CSharpCodeProvider()
    let parameters = CompilerParameters(GenerateExecutable=false, OutputAssembly="AutoGen.dll")
    let results = codeProvider.CompileAssemblyFromSource(parameters, "")
    printfn "%A" ([for str in results.Output -> str] |> Seq.toArray)
    0
