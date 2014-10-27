module XsdTool.Code

open System
open System.CodeDom
open System.Reflection

let addParameter name (tp: Type) (m: CodeMemberMethod) =
    m.Parameters.Add(CodeParameterDeclarationExpression(tp, name)) |> ignore
    m

let addParameterTypeName name (tpName: string) (m: CodeMemberMethod) =
    m.Parameters.Add(CodeParameterDeclarationExpression(CodeTypeReference(tpName), name)) |> ignore
    m

let declareVariable (tp: Type) (nm: string) (exp: CodeExpression) : CodeStatement =
    upcast CodeVariableDeclarationStatement(tp, nm, exp)

let declareVariableTypeName (tpName: string) (nm: string) (exp: CodeExpression) : CodeStatement =
    upcast CodeVariableDeclarationStatement(tpName, nm, exp)

let variable nm : CodeExpression =
    upcast CodeVariableReferenceExpression(nm)

let castVariable (tp: Type) (v: CodeExpression) = CodeCastExpression(tp, v)
let returns (exp: CodeExpression option) = (match exp with | Some e -> CodeMethodReturnStatement(e) | _ -> CodeMethodReturnStatement()) :> CodeStatement
let invoke (target: CodeExpression) methodName (args: CodeExpression list) = CodeMethodInvokeExpression(target, methodName, args |> List.toArray) :> CodeExpression
let asStatement exp = CodeExpressionStatement(exp) :> CodeStatement

let prop target name : CodeExpression =
    upcast CodePropertyReferenceExpression(target, name)

let primitive value : CodeExpression =
    upcast CodePrimitiveExpression(value)

let asOutParam exp = CodeDirectionExpression(FieldDirection.Out, exp)

let extensionsClass name =
    let targetClass = CodeTypeDeclaration(name, IsClass=true, TypeAttributes=TypeAttributes.NestedFamANDAssem)
    targetClass.StartDirectives.Add(CodeRegionDirective(CodeRegionMode.Start, sprintf "%s    static" System.Environment.NewLine)) |> ignore
    targetClass.EndDirectives.Add(CodeRegionDirective(CodeRegionMode.End, "")) |> ignore
    targetClass

let extensionMethod name (returnType: Type option) (paramType: Type, paramName) =
    let meth = CodeMemberMethod(Name=name, Attributes=(MemberAttributes.Public ||| MemberAttributes.Static))
    if returnType.IsSome then meth.ReturnType <- CodeTypeReference(returnType.Value)
    meth.Parameters.Add(CodeParameterDeclarationExpression(sprintf "this %s" paramType.FullName, paramName)) |> ignore
    meth

let createObject (tp: Type) (args: CodeExpression list) : CodeExpression =
    upcast CodeObjectCreateExpression(tp, args |> List.toArray)

let createArray (tp: Type) (args: CodeExpression list) : CodeExpression =
    upcast CodeArrayCreateExpression(tp, args |> List.toArray)

let throwException (tp: Type) (args: CodeExpression list) : CodeStatement =
    upcast CodeThrowExceptionStatement(createObject tp args)

let equals (exp: CodeExpression) (otherExp: CodeExpression) =
    CodeBinaryOperatorExpression(exp, CodeBinaryOperatorType.IdentityEquality, otherExp)

let inequals (exp: CodeExpression) (otherExp: CodeExpression) =
    CodeBinaryOperatorExpression(exp, CodeBinaryOperatorType.IdentityInequality, otherExp)

let addStatement (s: CodeStatement) (meth: CodeMemberMethod) =
    meth.Statements.Add(s) |> ignore
    meth

let assign left right : CodeStatement = upcast CodeAssignStatement(left, right)
let typeOf (tp: Type) : CodeExpression = upcast CodeTypeReferenceExpression(tp)
let typeOfName (tpName: string) : CodeExpression = upcast CodeTypeReferenceExpression(tpName)
