// The DSL
[<AutoOpen>]
module LightXml =
    type XmlMarkup =
        | Element of XmlElement
        | Attribute of XmlAttribute

    and XmlName =
        | Name of string
        | QualifiedName of string * string

    and XmlElement =
        {   Name:XmlName
            Attributes:XmlAttribute seq
            Content:XmlElementContent }

    and XmlElementContent =
        | Empty
        | Value of string
        | Content of XmlElement seq
            
    and XmlAttribute =
        {   Name:XmlName
            Value:string    }
            
    let name s = Name (s)
    let qname ns s = QualifiedName (ns, s)

    let (@=) name value = { Name=name; Value=value }
    let elem name = { Name=name; Attributes=[]; Content=Empty }
    let attribs a (el:XmlElement) = { el with Attributes=a }
    let value s (el:XmlElement) = { el with Content=Value (s) }
    let content items (el:XmlElement) = { el with Content=Content (items) }

#if INTERACTIVE
#r "System.Xml.Linq.dll"
#endif
open System.Xml.Linq
open LightXml

[<AutoOpen>]
module XElementExtension =
    let private mapName = function
        | Name n -> XName.Get (n)
        | QualifiedName (ns,n) -> XName.Get (n, ns)

    let private concatNames (parentName : XmlName) (nodeName : XmlName) = 
        match nodeName with
        | QualifiedName (ns, n) -> qname ns n
        | Name n -> match parentName with
                    | QualifiedName (pns, _) -> qname pns n
                    | Name n -> name n

    let private mapAttribs (attribs:XmlAttribute seq) =
        attribs |> Seq.map (fun a -> new XAttribute (mapName a.Name, a.Value))
                
    let rec private mapXmlElement (e:XmlElement) =
        match e.Content with
        | Empty -> new XElement (mapName e.Name, mapAttribs e.Attributes)
        | Value s -> 
            let content =
                mapAttribs e.Attributes
                |> Seq.map (fun a -> a :> obj)
                |> Seq.append ([s :> obj])

            new XElement (mapName e.Name, content)
        | Content c -> 
            let content =
                mapAttribs e.Attributes
                |> Seq.map (fun a -> a :> obj)
                |> Seq.append (c |> Seq.map (fun se -> mapXmlElement ({se with Name = (concatNames e.Name se.Name)}) :> obj)) 
            new XElement (mapName e.Name, content)

    module XElement =
        let fromXmlElement (xe:XmlElement) = mapXmlElement xe
        let ToString (xe:XmlElement) = (xe |> fromXmlElement).ToString()


