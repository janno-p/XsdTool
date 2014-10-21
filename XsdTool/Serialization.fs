module XsdTool.Serialization

open System.CodeDom
open System.Xml
open System.Xml.Schema
open XsdTool.Code
open XsdTool.Xsd

type ComplexTypeContext =
    | SimpleContext of XmlSchemaComplexType
    | ArrayContext of XmlSchemaComplexType * System.Type * string * string

let CreateMethod (xsd: XsdDetails) (assembly: AssemblyDetails) (response: XmlSchemaElement) =
    let responseType = xsd.GetSchemaType(response.SchemaTypeName) :?> XmlSchemaComplexType

    let meth = CodeMemberMethod(Name="Serialize", Attributes=(MemberAttributes.Public ||| MemberAttributes.Static))

    meth |> addParameter "writer" typeof<XmlWriter>
         |> addParameter "name" typeof<string>
         |> addParameter "obj" typeof<obj>
         |> ignore

    let codeType = assembly.GetRuntimeType(responseType)

    let addStatement (s: CodeStatement) = meth.Statements.Add(s) |> ignore
    let mapType' = mapType xsd

    addStatement (variable "obj" |> castVariable codeType |> declareVariable codeType "value0")

    let nextVariableName =
        let variableCounter = ref 1
        let generateNextName () =
            let result = sprintf "value%d" !variableCounter
            variableCounter := !variableCounter + 1
            result
        generateNextName

    let rec buildComplexTypeStatements (varExp: CodeExpression) (nameExp: CodeExpression) (typeContext: ComplexTypeContext) = seq<CodeStatement> {
        yield invoke (variable "writer") "WriteStartElement" [nameExp] |> asStatement

        let buildElements (sequence: XmlSchemaSequence) = seq<CodeStatement> {
            for item in sequence.Items do
                match item with
                | :? XmlSchemaElement as element ->
                    match mapType' element.SchemaTypeName with
                    | ComplexType schemaType ->
                        let name = nextVariableName()
                        match getArrayElementType schemaType xsd with
                        | Some (arrTp, itemName) ->
                            let tp = assembly.GetRuntimeType(arrTp)
                            yield upcast (prop varExp element.Name |> declareVariable (tp.MakeArrayType()) name)
                            yield! buildComplexTypeStatements (variable name) (primitive element.Name) (ArrayContext(arrTp, tp, name, itemName))
                        | _ ->
                            let tp = assembly.GetRuntimeType(schemaType)
                            yield upcast (prop varExp element.Name |> castVariable tp |> declareVariable tp name)
                            yield! buildComplexTypeStatements (variable name) (primitive element.Name) (SimpleContext schemaType)
                    | SimpleType writeMethod -> yield invoke (variable "writer") writeMethod [primitive element.Name; prop varExp element.Name] |> asStatement
                | _ -> failwithf "Handling sequence item of type %O is not implemented" (item.GetType())
        }

        let statements =
            seq<CodeStatement> {
                match typeContext with
                | ArrayContext (complexType, runtimeType, name, itemName) ->
                    let etp = typedefof<System.Collections.Generic.IEnumerator<_>>.MakeGenericType(runtimeType)
                    let ename = sprintf "%s_enum" name
                    let vname = nextVariableName()
                    let castStatement = (prop (variable ename) "Current" |> declareVariable runtimeType vname) :> CodeStatement
                    let statements = buildComplexTypeStatements (variable vname) (primitive itemName) (SimpleContext complexType) |> Seq.toArray
                    let getp = typedefof<System.Collections.Generic.IEnumerable<_>>.MakeGenericType(runtimeType)
                    yield upcast (invoke (variable name |> castVariable getp) "GetEnumerator" [] |> declareVariable etp ename)
                    yield upcast CodeIterationStatement(CodeSnippetStatement(),
                                                        invoke (variable ename) "MoveNext" [],
                                                        CodeSnippetStatement(),
                                                        Array.append [| castStatement |] statements)
                | SimpleContext complexType ->
                    match complexType.Particle with
                    | :? XmlSchemaSequence as sequence -> yield! buildElements sequence
                    | :? XmlSchemaGroupRef as groupRef ->
                        let group = xsd.GetGroup(groupRef.RefName)
                        match group.Particle with
                        | :? XmlSchemaSequence as sequence -> yield! buildElements sequence
                        | _ -> failwithf "Only sequences are supported for serialization. Given: %O." group.Particle
                    | _ -> failwithf "Handling particle of type %O is not implemented" (complexType.Particle.GetType())
            } |> Seq.toArray

        yield upcast CodeConditionStatement(CodeBinaryOperatorExpression(varExp,
                                                                         CodeBinaryOperatorType.IdentityEquality,
                                                                         CodePrimitiveExpression(null)),
                                            [| invoke (variable "writer") "WriteNilAttributeExt" [] |> asStatement |],
                                            statements)
        yield invoke (variable "writer") "WriteEndElement" [] |> asStatement
    }

    buildComplexTypeStatements (variable "value0") (variable "name") (SimpleContext responseType)
    |> Seq.iter addStatement

    meth
