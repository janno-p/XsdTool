module XsdTool.Code

open System
open System.CodeDom

let addParameter name (tp: Type) (m: CodeMemberMethod) =
    m.Parameters.Add(CodeParameterDeclarationExpression(tp, name)) |> ignore
    m

let declareVariable (tp: Type) (nm: string) (exp: CodeExpression) = CodeVariableDeclarationStatement(tp, nm, exp)
let variable nm = CodeVariableReferenceExpression(nm)
let castVariable (tp: Type) (v: CodeVariableReferenceExpression) = CodeCastExpression(tp, v)
