module XsdTool.CreateXmlReaderExtensions

open System
open System.CodeDom
open System.Xml

let private createXmlReaderExtensionMethod name (tp: Type) =
    let meth = CodeMemberMethod(Name=name, Attributes=(MemberAttributes.Public ||| MemberAttributes.Static))
    meth.ReturnType <- CodeTypeReference(tp)
    meth.Parameters.Add(CodeParameterDeclarationExpression(sprintf "this %s" typeof<XmlReader>.FullName, "reader")) |> ignore
    meth

let private createReadStringExt () =
    let meth = createXmlReaderExtensionMethod "ReadStringExt" typeof<string>
    meth.Statements.Add(CodeVariableDeclarationStatement(typeof<string>, "result", CodePrimitiveExpression(null))) |> ignore
    meth.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(CodePropertyReferenceExpression(CodeVariableReferenceExpression("reader"), "IsEmptyElement"), CodeBinaryOperatorType.IdentityEquality, CodePrimitiveExpression(false)),
                                                         [| CodeVariableDeclarationStatement(typeof<string>, "value", CodeMethodInvokeExpression(CodeVariableReferenceExpression("reader"), "ReadString")) :> CodeStatement
                                                            CodeConditionStatement(CodeMethodInvokeExpression(CodeTypeReferenceExpression(typeof<string>), "IsNullOrEmpty", CodeVariableReferenceExpression("value")),
                                                                                   [| CodeAssignStatement(CodeVariableReferenceExpression("result"), CodePrimitiveExpression(null)) :> CodeStatement |],
                                                                                   [| CodeAssignStatement(CodeVariableReferenceExpression("result"), CodeVariableReferenceExpression("value")) :> CodeStatement |]) :> CodeStatement |])) |> ignore
    meth.Statements.Add(CodeExpressionStatement(CodeMethodInvokeExpression(CodeVariableReferenceExpression("reader"), "MoveToNextElement"))) |> ignore
    meth.Statements.Add(CodeMethodReturnStatement(CodeVariableReferenceExpression("result"))) |> ignore
    meth

let private createReadDateTimeExt () =
    let meth = createXmlReaderExtensionMethod "ReadDateTimeExt" typeof<Nullable<DateTime>>
    meth.Statements.Add(CodeVariableDeclarationStatement(typeof<System.Nullable<System.DateTime>>, "result", CodePrimitiveExpression(null))) |> ignore
    meth.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(CodePropertyReferenceExpression(CodeVariableReferenceExpression("reader"), "IsEmptyElement"), CodeBinaryOperatorType.IdentityEquality, CodePrimitiveExpression(false)),
                                                         [| CodeVariableDeclarationStatement(typeof<string>, "value", CodeMethodInvokeExpression(CodeVariableReferenceExpression("reader"), "ReadString")) :> CodeStatement
                                                            CodeConditionStatement(CodeMethodInvokeExpression(CodeTypeReferenceExpression(typeof<string>), "IsNullOrEmpty", CodeVariableReferenceExpression("value")),
                                                                                   [| CodeAssignStatement(CodeVariableReferenceExpression("result"), CodePrimitiveExpression(null)) :> CodeStatement |],
                                                                                   [| CodeAssignStatement(CodeVariableReferenceExpression("result"), CodePropertyReferenceExpression(CodeMethodInvokeExpression(CodeTypeReferenceExpression("System.Xml.XmlConvert"), "ToDateTimeOffset", CodeVariableReferenceExpression("value")), "DateTime")) :> CodeStatement |]) :> CodeStatement |])) |> ignore
    meth.Statements.Add(CodeExpressionStatement(CodeMethodInvokeExpression(CodeVariableReferenceExpression("reader"), "MoveToNextElement"))) |> ignore
    meth.Statements.Add(CodeMethodReturnStatement(CodeVariableReferenceExpression("result"))) |> ignore
    meth

let private createMoveToNextElementMethod () =
    let meth = createXmlReaderExtensionMethod "MoveToNextElement" typeof<bool>
    let condition = CodeBinaryOperatorExpression(CodePropertyReferenceExpression(CodeVariableReferenceExpression("reader"), "NodeType"),
                                                 CodeBinaryOperatorType.IdentityInequality,
                                                 CodePropertyReferenceExpression(CodeTypeReferenceExpression(typeof<XmlNodeType>), "Element"))
    let whileLoop = CodeIterationStatement(CodeExpressionStatement(CodeMethodInvokeExpression(CodeVariableReferenceExpression("reader"), "Read")),
                                           condition,
                                           CodeSnippetStatement())
    whileLoop.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(CodeMethodInvokeExpression(CodeVariableReferenceExpression("reader"), "Read"),
                                                                                 CodeBinaryOperatorType.IdentityEquality,
                                                                                 CodePrimitiveExpression(false)),
                                                    CodeMethodReturnStatement(CodePrimitiveExpression(false)))) |> ignore
    meth.Statements.Add(whileLoop) |> ignore
    meth.Statements.Add(CodeMethodReturnStatement(CodePrimitiveExpression(true))) |> ignore
    meth

let createClass () =
    let targetClass = CodeTypeDeclaration("XmlReaderExtensions", IsClass=true, Attributes=MemberAttributes.Public)
    targetClass.StartDirectives.Add(CodeRegionDirective(CodeRegionMode.Start, sprintf "%s    static" System.Environment.NewLine)) |> ignore
    targetClass.EndDirectives.Add(CodeRegionDirective(CodeRegionMode.End, "")) |> ignore
    targetClass.Members.Add(createMoveToNextElementMethod()) |> ignore
    targetClass.Members.Add(createReadStringExt()) |> ignore
    targetClass.Members.Add(createReadDateTimeExt()) |> ignore
    targetClass
