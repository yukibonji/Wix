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

type Architecture =
    | X86
    | X64
    override archi.ToString() = 
        match archi with 
        | X86 -> "x86"
        | X64 -> "x64"


type File = {
    Id : UniqueIdentifier
    Source : System.IO.FileInfo
    } with
    member this.ToXmlElement() =
        elem (name "File") 
                    |> attribs [name "Id" @= this.Id.ToString();
                                name "Name" @= this.Source.Name;
                                name "Source" @= this.Source.FullName]
let createFile (fi : System.IO.FileInfo) = {Id = (createUniqueIdentifier fi.Name); Source=fi}

type Directory = {
    Id : UniqueIdentifier
    Source : System.IO.DirectoryInfo
    Files : File seq
    } with
    member this.ToXmlElement() =
        elem (name "Directory") 
                    |> attribs [name "Id" @= this.Id.ToString();
                                name "Name" @= this.Source.Name]
                    |> content (this.Files |> Seq.map (fun f -> f.ToXmlElement()))

let rec allFilesInDir (dirs : System.IO.DirectoryInfo seq) =
    if Seq.isEmpty dirs then Seq.empty 
    else seq { yield! dirs |> Seq.collect (fun d -> d.EnumerateFiles())
               yield! dirs |> Seq.collect (fun d -> d.EnumerateDirectories()) |> allFilesInDir }
let createDirectory (filter) (di : System.IO.DirectoryInfo) =
    { Id = createUniqueIdentifier di.Name; Source = di; Files =  [| di |] |> allFilesInDir |> Seq.where filter |> Seq.map createFile }

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

let fxe = f.ToXmlElement() |> XElement.fromXmlElement
fxe.ToString()

let d = createDirectory (fun f -> f.Extension.Equals(".png")) (System.IO.DirectoryInfo ".")
(d.ToXmlElement() |> XElement.fromXmlElement).ToString()