module XsdTool.Xsd

open System.Collections.Generic
open System.Xml
open System.Xml.Schema

let inline private getQualifiedName (o: ^T) (schema: XmlSchema) =
    match schema.ElementFormDefault with
    | XmlSchemaForm.Unqualified ->
        let name = (^T: (member Name: string) o)
        XmlQualifiedName(name, schema.TargetNamespace)
    | _ -> (^T: (member QualifiedName: XmlQualifiedName) o)

let inline private getItemsOfType (schema: XmlSchema): IDictionary<XmlQualifiedName, ^T> =
    [for i in schema.Items -> i]
    |> List.choose (fun i -> match i with
                             | :? ^T as tp -> Some(getQualifiedName tp schema, tp)
                             | _ -> None)
    |> dict

type XsdDetails =
    { TargetNamespace: string
      Namespaces: IDictionary<string, string>
      Types: IDictionary<XmlQualifiedName, XmlSchemaType>
      Groups: IDictionary<XmlQualifiedName, XmlSchemaGroup> }
    member this.GetSchemaType qname =
        match this.Types.TryGetValue(qname) with
        | true, tp -> tp
        | _ -> failwith <| sprintf "Unable to find xml schema type %O from schema." qname
    static member FromSchema (schema: XmlSchema) =
        { TargetNamespace = schema.TargetNamespace
          Namespaces = schema.Namespaces.ToArray() |> Array.map (fun xqn -> (xqn.Name, xqn.Namespace)) |> dict
          Types = getItemsOfType schema
          Groups = getItemsOfType schema }
