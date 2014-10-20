module XsdTool.Xsd

open System
open System.Collections.Generic
open System.Reflection
open System.Xml
open System.Xml.Schema
open XsdTool.Configuration

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
        | _ -> failwithf "Unable to find xml schema type %O from schema." qname
    member this.GetGroup qname =
        match this.Groups.TryGetValue(qname) with
        | true, g -> g
        | _ -> failwithf "Unable to find xml schema group %O from schema." qname
    static member FromSchema (schema: XmlSchema) =
        { TargetNamespace = schema.TargetNamespace
          Namespaces = schema.Namespaces.ToArray() |> Array.map (fun xqn -> (xqn.Name, xqn.Namespace)) |> dict
          Types = getItemsOfType schema
          Groups = getItemsOfType schema }

type AssemblyDetails =
    { TargetNamespace: string
      AssemblyName: string
      ProbingPath: string
      Types: IDictionary<string, Type> }
    member this.GetRuntimeType (schemaType: XmlSchemaType) =
        let name = match schemaType.UnhandledAttributes |> Array.tryFind (fun attr -> attr.NamespaceURI = this.TargetNamespace && attr.LocalName = "type") with
                   | Some typeName -> match typeName.Value.Split(':') with
                                      | [| _; name |] -> name
                                      | _ -> failwithf "Invalid runtime type name %s" typeName.Value
                   | _ -> failwithf "Runtime type not specified for type %s" schemaType.Name
        match this.Types.TryGetValue name with
        | true, tp -> tp
        | _ -> failwithf "Unable to resolve runtime type %s" name
    static member FromConfig (config: DtoAssemblies) =
        { TargetNamespace = config.xmlNamespace
          AssemblyName = config.assembly
          ProbingPath = config.probingPath
          Types = Assembly.Load(config.assembly).GetTypes()
                  |> Array.filter (fun tp -> tp.GetCustomAttributes(typeof<SerializableAttribute>, false) |> Array.isEmpty |> not)
                  |> Array.filter (fun tp -> config.assemblyNamespaces |> Array.exists (fun ns -> ns = tp.Namespace))
                  |> Array.map (fun tp -> (tp.Name, tp))
                  |> dict }

type ElementType =
    | SimpleType of string
    | ComplexType of XmlSchemaComplexType

let (|XmlSchema|_|) (n: XmlQualifiedName): string option =
    if n.Namespace = "http://www.w3.org/2001/XMLSchema" then
        Some n.Name
    else None

let mapXmlSchemaTypeToMethod = function
    | "string" -> "WriteStringExt"
    | "date" -> "WriteDateExt"
    | "dateTime" -> "WriteDateTimeExt"
    | "long" -> "WriteLongExt"
    | name -> failwithf "Unmapped XML Schema type %s" name

let mapType (xsd: XsdDetails) (qname: XmlQualifiedName) =
    match qname with
    | XmlSchema name -> SimpleType (mapXmlSchemaTypeToMethod name)
    | _ -> ComplexType (xsd.GetSchemaType qname :?> XmlSchemaComplexType)

let getArrayElementType (schemaType: XmlSchemaComplexType) (xsd: XsdDetails) =
    match schemaType.Particle with
    | :? XmlSchemaSequence as sequence ->
        match sequence.Items.Count with
        | 1 ->
            match sequence.Items.[0] with
            | :? XmlSchemaElement as element ->
                let str = match element.MaxOccursString with | null -> "" | x -> x
                match str.ToLower() with
                | "unbounded" -> Some (xsd.GetSchemaType element.SchemaTypeName :?> XmlSchemaComplexType, element.Name)
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None
