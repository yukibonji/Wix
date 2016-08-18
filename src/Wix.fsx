#if INTERACTIVE
#load "LightXml.fsx"
#endif
open LightXml


type Version = {
        Major : int
        Minor : int
        BugFix : int
    } with 
    override this.ToString() = sprintf "%i.%i.%i" this.Major this.Minor this.BugFix 

let createVersion maj min bf = {Major = maj; Minor = min; BugFix = bf}

type UniqueIdentifier = {
        Index : int64
        Name : string
    } with
    override this.ToString() = sprintf "%i_%s" this.Index this.Name

let mutable internal uid = (int64)0
let createUniqueIdentifier name = 
    uid <- uid + (int64)1
    {Index = uid; Name = name}

type File = {
    Id : UniqueIdentifier
    Source : System.IO.FileInfo
    } with
    member this.toXmlElement() =
        let xe = elem (name "File") 
                    |> attribs [name "Id" @= this.Id.ToString();
                                name "Name" @= this.Source.Name;
                                name "Source" @= this.Source.FullName]
        xe |> XElement.fromXmlElement
let createFile (fi:System.IO.FileInfo) = {Id = (createUniqueIdentifier fi.Name); Source=fi}

type Component = {
        Id : UniqueIdentifier
        Guid : System.Guid
        Files : File seq
    }
            
type Wix = 
    | Bundle of Bundle
    | Product of Product

and Product = {
        Name : string
        Code : System.Guid
        Version : Version
        Publisher : string
        UpgradeGuid : System.Guid
    }

and Bundle = {
        Setups : string seq
    }

let f = createFile (System.IO.FileInfo "test.txt")

f.toXmlElement().ToString()