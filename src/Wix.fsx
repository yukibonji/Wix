#if INTERACTIVE
#load "LightXml.fsx"
#endif
open LightXml

[<AutoOpen>]
module Wix =
    type Version = {
            Major : int
            Minor : int
            Build : int
            Revision : int option
        } with 
        override this.ToString() =
            match this.Revision with
            | Some r -> sprintf "%i.%i.%i.%i" this.Major this.Minor this.Build this.Revision.Value
            | None ->  sprintf "%i.%i.%i" this.Major this.Minor this.Build
        member this.IncRevision() = 
            {this with 
                Revision = match this.Revision with 
                           | Some r -> Some (r + 1)
                           | None -> Some (1)}

    let createVersion maj min bf = {Major = maj; Minor = min; Build = bf; Revision = None}

    
    type Architecture =
        | X86
        | X64
        override archi.ToString() = 
            match archi with 
            | X86 -> "x86"
            | X64 -> "x64"


    type File = {
        Id : string
        Source : System.IO.FileInfo
        } with
        member this.ToXmlElement() =
            elem (name "File") 
                        |> attribs [name "Id" @= this.Id;
                                    name "Name" @= this.Source.Name;
                                    name "Source" @= this.Source.FullName]
    let IdFromFile (file : string) = System.IO.Path.GetFileNameWithoutExtension(file) + System.IO.Path.GetExtension(file).ToUpper().Replace(".", "")
    let createFile (fi : System.IO.FileInfo) = {Id = fi.Name |> IdFromFile ; Source = fi}

    type Directory = {
        Id : string
        Source : System.IO.DirectoryInfo
        Files : File seq
        } with
        member this.ToXmlElement() =
            elem (name "Directory") 
                        |> attribs [name "Id" @= this.Id.ToString();
                                    name "Name" @= this.Source.FullName]
                        |> content (this.Files |> Seq.map (fun f -> f.ToXmlElement()))
    let rec allFilesInDir (dirs : System.IO.DirectoryInfo seq) =
        if Seq.isEmpty dirs then Seq.empty 
        else seq { 
            yield! dirs |> Seq.collect (fun d -> d.EnumerateFiles()) 
            yield! dirs |> Seq.collect (fun d -> d.EnumerateDirectories()) |> allFilesInDir }
    let createDirectory (filter) (dir : System.IO.DirectoryInfo) =
        { Id = dir.Name |> IdFromFile; Source = dir; Files =  [| dir |] |> allFilesInDir |> Seq.where filter |> Seq.map createFile }

    type Component = {
            Id : string
            Guid : System.Guid
            Files : File seq
        }
                
    type Product = {
            Name : string
            Manufacturer : string
            Id : System.Guid
            UpgradeCode : System.Guid
            Version : Version
            Description : string
        } with
        member this.ToXmlElement() = 
            elem (name "Product")
                |> attribs [name "Name" @= this.Name;
                            name "Id" @= this.Id.ToString("D");
                            name "UpgradeCode" @= this.UpgradeCode.ToString("D");
                            name "Version" @= this.Version.ToString();
                            name "Manufacturer" @= this.Manufacturer.ToString();]
                |> content [
                    elem (name "Package")
                        |> attribs [name "Id" @= "*";
                                    name "Description" @= this.Description;
                                    name "Manufacturer" @= this.Manufacturer.ToString();
                                    name "InstallerVersion" @= "200";
                                    name "Compressed" @= "yes"]
                ]
    let DefaultProduct = {Name = System.String.Empty; Id = System.Guid.NewGuid(); Version = createVersion 0 0 1; UpgradeCode = System.Guid.NewGuid(); Manufacturer = System.String.Empty; Description = System.String.Empty}
    let createProduct (updateProduct: Product -> Product) = 
        DefaultProduct |> updateProduct


    type Bundle = {
            Setups : string seq
        } with
        member this.ToXmlElement() = 
            elem (name "Bundle")

    type Wix = 
        | Bundle of Bundle
        | Product of Product
        with 
        member this.ToXmlElement() =
            let xe = match this with
                     | Bundle b -> b.ToXmlElement()
                     | Product p -> p.ToXmlElement()
            elem (qname "http://schemas.microsoft.com/wix/2006/wi" "Wix") 
                |> content [ xe ]
