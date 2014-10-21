﻿module XsdTool.Code

open System
open System.CodeDom

let addParameter name (tp: Type) (m: CodeMemberMethod) =
    m.Parameters.Add(CodeParameterDeclarationExpression(tp, name)) |> ignore
    m

let declareVariable (tp: Type) (nm: string) (exp: CodeExpression) = CodeVariableDeclarationStatement(tp, nm, exp)
let variable nm = CodeVariableReferenceExpression(nm)
let castVariable (tp: Type) (v: CodeExpression) = CodeCastExpression(tp, v)
let returns (exp: CodeExpression option) = match exp with | Some e -> CodeMethodReturnStatement(e) | _ -> CodeMethodReturnStatement()
let invoke (target: CodeExpression) methodName (args: CodeExpression list) = CodeMethodInvokeExpression(target, methodName, args |> List.toArray)
let asStatement exp = CodeExpressionStatement(exp) :> CodeStatement
let prop target name = CodePropertyReferenceExpression(target, name)
let primitive value = CodePrimitiveExpression(value)

let extensionsClass name =
    let targetClass = CodeTypeDeclaration(name, IsClass=true, Attributes=MemberAttributes.FamilyAndAssembly)
    targetClass.StartDirectives.Add(CodeRegionDirective(CodeRegionMode.Start, sprintf "%s    static" System.Environment.NewLine)) |> ignore
    targetClass.EndDirectives.Add(CodeRegionDirective(CodeRegionMode.End, "")) |> ignore
    targetClass

let extensionMethod name (returnType: Type option) (paramType: Type, paramName) =
    let meth = CodeMemberMethod(Name=name, Attributes=(MemberAttributes.Public ||| MemberAttributes.Static))
    if returnType.IsSome then meth.ReturnType <- CodeTypeReference(returnType.Value)
    meth.Parameters.Add(CodeParameterDeclarationExpression(sprintf "this %s" paramType.FullName, paramName)) |> ignore
    meth
