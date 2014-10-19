module XsdTool.Serialization

open System.CodeDom
open System.Xml
open System.Xml.Schema
open XsdTool.Code
open XsdTool.Xsd

let CreateMethod (response: XmlSchemaElement) (xsd: XsdDetails) =
    let responseType = xsd.GetSchemaType(response.SchemaTypeName)

    let meth = CodeMemberMethod(Name="Serialize", Attributes=(MemberAttributes.Public ||| MemberAttributes.Static))

    meth |> addParameter "writer" typeof<XmlWriter>
         |> addParameter "name" typeof<string>
         |> addParameter "obj" typeof<obj>
         |> ignore

    let codeType = typeof<obj> // Placeholder

    meth.Statements.Add(variable "obj" |> castVariable codeType |> declareVariable codeType "value") |> ignore

    meth
