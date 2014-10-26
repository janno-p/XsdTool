module XsdTool.ExtensionsBuilder

open System
open System.CodeDom
open XsdTool.Code

module XmlReader =
    let private createXmlReaderExtensionMethod name (tp: Type) =
        extensionMethod name (Some tp) (typeof<Xml.XmlReader>, "reader")

    let private xsiNamespace = "http://www.w3.org/2001/XMLSchema-instance"

    let private createIsNilElementExt () =
        createXmlReaderExtensionMethod "IsNilElementExt" typeof<bool>
        |> addStatement (invoke (variable "reader") "GetAttribute" [primitive "nil"; primitive xsiNamespace]
                         |> declareVariable typeof<string> "value")
        |> addStatement ((Some (CodeBinaryOperatorExpression(inequals (variable "value") (primitive null),
                                                     CodeBinaryOperatorType.BooleanAnd,
                                                     invoke (typeOf typeof<System.Xml.XmlConvert>) "ToBoolean" [variable "value"]) :> CodeExpression))
                         |> returns)

    let private createReadLongExt () =
        createXmlReaderExtensionMethod "ReadLongExt" typeof<Nullable<int64>>
        |> addStatement (primitive null |> declareVariable typeof<Nullable<int64>> "result")
        |> addStatement (CodeConditionStatement(equals (prop (variable "reader") "IsEmptyElement") (primitive false),
                                                [| invoke (variable "reader") "ReadString" [] |> declareVariable typeof<string> "value"
                                                   CodeConditionStatement(invoke (typeOf typeof<string>) "IsNullOrEmpty" [variable "value"],
                                                                          [| primitive null |> assign (variable "result") |],
                                                                          [| CodeAssignStatement(CodeVariableReferenceExpression("result"), CodeMethodInvokeExpression(CodeTypeReferenceExpression("System.Xml.XmlConvert"), "ToInt64", CodeVariableReferenceExpression("value"))) :> CodeStatement |]) :> CodeStatement |]))
        |> addStatement (invoke (variable "reader") "MoveToNextElement" [] |> asStatement)
        |> addStatement (Some(variable "result") |> returns)

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

    let private createReadDateExt () =
        let meth = createXmlReaderExtensionMethod "ReadDateExt" typeof<Nullable<DateTime>>
        meth.Statements.Add(CodeVariableDeclarationStatement(typeof<Nullable<DateTime>>, "result", CodePrimitiveExpression(null))) |> ignore
        meth.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(CodePropertyReferenceExpression(CodeVariableReferenceExpression("reader"), "IsEmptyElement"), CodeBinaryOperatorType.IdentityEquality, CodePrimitiveExpression(false)),
                                                             [| CodeVariableDeclarationStatement(typeof<string>, "value", CodeMethodInvokeExpression(CodeVariableReferenceExpression("reader"), "ReadString")) :> CodeStatement
                                                                CodeConditionStatement(CodeMethodInvokeExpression(CodeTypeReferenceExpression(typeof<string>), "IsNullOrEmpty", CodeVariableReferenceExpression("value")),
                                                                                       [| CodeAssignStatement(CodeVariableReferenceExpression("result"), CodePrimitiveExpression(null)) :> CodeStatement |],
                                                                                       [| CodeAssignStatement(CodeVariableReferenceExpression("result"), CodePropertyReferenceExpression(CodeMethodInvokeExpression(CodeTypeReferenceExpression("System.Xml.XmlConvert"), "ToDateTimeOffset", CodeVariableReferenceExpression("value")), "Date")) :> CodeStatement |]) :> CodeStatement |])) |> ignore
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
        targetClass.Members.Add(createIsNilElementExt()) |> ignore
        targetClass.Members.Add(createReadStringExt()) |> ignore
        targetClass.Members.Add(createReadDateTimeExt()) |> ignore
        targetClass.Members.Add(createReadDateExt()) |> ignore
        targetClass.Members.Add(createReadLongExt()) |> ignore
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
        meth |> addParameter "name" typeof<string>
             |> addParameter "value" typeof<obj>
             |> ignore
        let statements : CodeStatement[] = [|
            invoke (variable "value") "ToString" [] |> declareVariable typeof<string> "strValue"
            CodeConditionStatement(CodeBinaryOperatorExpression(CodeBinaryOperatorExpression(invoke (variable "strValue") "Contains" [primitive "&"],
                                                                                             CodeBinaryOperatorType.BooleanOr,
                                                                                             invoke (variable "strValue") "Contains" [primitive "<"]),
                                                                CodeBinaryOperatorType.BooleanOr,
                                                                invoke (variable "strValue") "Contains" [primitive ">"]),
                                   [| invoke (variable "writer") "WriteCData" [variable "strValue"] |> asStatement |],
                                   [| invoke (variable "writer") "WriteValue" [variable "strValue"] |> asStatement |])
        |]
        meth.Statements.Add(invoke (variable "writer") "WriteStartElement" [variable "name"]) |> ignore
        meth.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(variable "value",
                                                                                CodeBinaryOperatorType.IdentityEquality,
                                                                                primitive null),
                                                   [| invoke (variable "writer") "WriteNilAttributeExt" [] |> asStatement |],
                                                   statements)) |> ignore
        meth.Statements.Add(invoke (variable "writer") "WriteEndElement" []) |> ignore
        meth

    let private createWriteLongExtMethod () =
        let meth = createXmlWriterExtensionMethod "WriteLongExt"
        meth |> addParameter "name" typeof<string>
             |> addParameter "value" typeof<obj>
             |> ignore
        let statements : CodeStatement[] = [|
            [ invoke (CodeTypeReferenceExpression typeof<System.Convert>) "ToInt64" [variable "value"] ]
            |> invoke (variable "writer") "WriteValue" 
            |> asStatement
        |]
        meth.Statements.Add(invoke (variable "writer") "WriteStartElement" [variable "name"]) |> ignore
        meth.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(variable "value",
                                                                                CodeBinaryOperatorType.IdentityEquality,
                                                                                primitive null),
                                                   [| invoke (variable "writer") "WriteNilAttributeExt" [] |> asStatement |],
                                                   statements)) |> ignore
        meth.Statements.Add(invoke (variable "writer") "WriteEndElement" []) |> ignore
        meth

    let private createWriteDateExtMethod () =
        let meth = createXmlWriterExtensionMethod "WriteDateExt"
        meth |> addParameter "name" typeof<string>
             |> addParameter "value" typeof<obj>
             |> ignore
        let statements : CodeStatement[] = [|
            invoke (CodeTypeReferenceExpression(typeof<System.Convert>)) "ToDateTime" [variable "value"]
            |> declareVariable typeof<System.DateTime> "dateValue"

            [ invoke (CodeTypeReferenceExpression typeof<System.Xml.XmlConvert>) "ToString" [variable "dateValue"; primitive "yyyy-MM-dd"] ]
            |> invoke (variable "writer") "WriteValue" 
            |> asStatement
        |]
        meth.Statements.Add(invoke (variable "writer") "WriteStartElement" [variable "name"]) |> ignore
        meth.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(variable "value",
                                                                                CodeBinaryOperatorType.IdentityEquality,
                                                                                primitive null),
                                                   [| invoke (variable "writer") "WriteNilAttributeExt" [] |> asStatement |],
                                                   statements)) |> ignore
        meth.Statements.Add(invoke (variable "writer") "WriteEndElement" []) |> ignore
        meth

    let private createWriteDateTimeExtMethod () =
        let meth = createXmlWriterExtensionMethod "WriteDateTimeExt"
        meth |> addParameter "name" typeof<string>
             |> addParameter "value" typeof<obj>
             |> ignore
        let statements : CodeStatement[] = [|
            [ invoke (CodeTypeReferenceExpression typeof<System.Convert>) "ToDateTime" [variable "value"] ]
            |> invoke (variable "writer") "WriteValue" 
            |> asStatement
        |]
        meth.Statements.Add(invoke (variable "writer") "WriteStartElement" [variable "name"]) |> ignore
        meth.Statements.Add(CodeConditionStatement(CodeBinaryOperatorExpression(variable "value",
                                                                                CodeBinaryOperatorType.IdentityEquality,
                                                                                primitive null),
                                                   [| invoke (variable "writer") "WriteNilAttributeExt" [] |> asStatement |],
                                                   statements)) |> ignore
        meth.Statements.Add(invoke (variable "writer") "WriteEndElement" []) |> ignore
        meth

    let CreateExtensionClass () =
        let targetClass = extensionsClass "XmlWriterExtensions"
        targetClass.Members.Add(createWriteNilAttributeExtMethod()) |> ignore
        targetClass.Members.Add(createWriteStringExtMethod()) |> ignore
        targetClass.Members.Add(createWriteLongExtMethod()) |> ignore
        targetClass.Members.Add(createWriteDateExtMethod()) |> ignore
        targetClass.Members.Add(createWriteDateTimeExtMethod()) |> ignore
        targetClass
