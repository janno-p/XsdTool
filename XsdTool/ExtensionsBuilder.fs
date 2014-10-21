module XsdTool.ExtensionsBuilder

open System
open System.CodeDom
open XsdTool.Code

module XmlReader =
    let private createXmlReaderExtensionMethod name (tp: Type) = extensionMethod name (Some tp) (typeof<Xml.XmlReader>, "reader")

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
        meth.Statements.Add(CodeVariableDeclarationStatement(typeof<Nullable<DateTime>>, "result", CodePrimitiveExpression(null))) |> ignore
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
                                                     CodePropertyReferenceExpression(CodeTypeReferenceExpression(typeof<Xml.XmlNodeType>), "Element"))
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

    let CreateExtensionClass () =
        let targetClass = extensionsClass "XmlReaderExtensions"
        targetClass.Members.Add(createMoveToNextElementMethod()) |> ignore
        targetClass.Members.Add(createReadStringExt()) |> ignore
        targetClass.Members.Add(createReadDateTimeExt()) |> ignore
        targetClass


module XmlWriter =
    let private createXmlWriterExtensionMethod name = extensionMethod name None (typeof<Xml.XmlWriter>, "writer")

    let private createWriteNilAttributeExtMethod () =
        let meth = createXmlWriterExtensionMethod "WriteNilAttributeExt"
        meth.Statements.Add(invoke (variable "writer") "WriteAttributeString" [primitive "nil"
                                                                               primitive "http://www.w3.org/2001/XMLSchema-instance"
                                                                               primitive "1"]) |> ignore
        meth

    let private createWriteStringExtMethod () =
        let meth = createXmlWriterExtensionMethod "WriteStringExt"
        meth.Parameters.Add(CodeParameterDeclarationExpression(typeof<string>, "name")) |> ignore
        meth.Parameters.Add(CodeParameterDeclarationExpression(typeof<obj>, "value")) |> ignore
        meth

    let private createWriteLongExtMethod () =
        let meth = createXmlWriterExtensionMethod "WriteLongExt"
        meth.Parameters.Add(CodeParameterDeclarationExpression(typeof<string>, "name")) |> ignore
        meth.Parameters.Add(CodeParameterDeclarationExpression(typeof<obj>, "value")) |> ignore
        meth

    let private createWriteDateExtMethod () =
        let meth = createXmlWriterExtensionMethod "WriteDateExt"
        meth.Parameters.Add(CodeParameterDeclarationExpression(typeof<string>, "name")) |> ignore
        meth.Parameters.Add(CodeParameterDeclarationExpression(typeof<obj>, "value")) |> ignore
        meth

    let private createWriteDateTimeExtMethod () =
        let meth = createXmlWriterExtensionMethod "WriteDateTimeExt"
        meth.Parameters.Add(CodeParameterDeclarationExpression(typeof<string>, "name")) |> ignore
        meth.Parameters.Add(CodeParameterDeclarationExpression(typeof<obj>, "value")) |> ignore
        meth

    let CreateExtensionClass () =
        let targetClass = extensionsClass "XmlWriterExtensions"
        targetClass.Members.Add(createWriteNilAttributeExtMethod()) |> ignore
        targetClass.Members.Add(createWriteStringExtMethod()) |> ignore
        targetClass.Members.Add(createWriteLongExtMethod()) |> ignore
        targetClass.Members.Add(createWriteDateExtMethod()) |> ignore
        targetClass.Members.Add(createWriteDateTimeExtMethod()) |> ignore
        targetClass
