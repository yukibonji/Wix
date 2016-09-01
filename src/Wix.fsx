
#if INTERACTIVE
#load "LightXml.fsx"
#endif

open LightXml

[<AutoOpen>]
module Wix =
    type Version =
        { Major : int
          Minor : int
          Build : int
          Revision : int option }

        override this.ToString() =
            match this.Revision with
            | Some r -> sprintf "%i.%i.%i.%i" this.Major this.Minor this.Build this.Revision.Value
            | None -> sprintf "%i.%i.%i" this.Major this.Minor this.Build

        member this.IncRevision() =
            { this with Revision =
                            match this.Revision with
                            | Some r -> Some(r + 1)
                            | None -> Some(1) }

    let createVersion maj min bf =
        { Major = maj
          Minor = min
          Build = bf
          Revision = None }

    type Architecture =
        | X86
        | X64
        override archi.ToString() =
            match archi with
            | X86 -> "x86"
            | X64 -> "x64"

    type File =
        { Id : string
          Source : System.IO.FileInfo }
        member this.ToXmlElement() =
            elem (name "File") |> attribs [ name "Id" @= this.Id
                                            name "Name" @= this.Source.Name
                                            name "Source" @= this.Source.FullName ]

    let IdFromFile(file : string) =
        System.IO.Path.GetFileNameWithoutExtension(file) + System.IO.Path.GetExtension(file).ToUpper().Replace(".", "")

    let createFile (fi : System.IO.FileInfo) =
        { Id = fi.Name |> IdFromFile
          Source = fi }

    type Directory =
        { Id : string
          Source : System.IO.DirectoryInfo
          Files : File seq }
        member this.ToXmlElement() =
            elem (name "Directory")
            |> attribs [ name "Id" @= this.Id.ToString()
                         name "Name" @= this.Source.FullName ]
            |> content (this.Files |> Seq.map (fun f -> f.ToXmlElement()))

    let rec allFilesInDir (dirs : System.IO.DirectoryInfo seq) =
        if Seq.isEmpty dirs then Seq.empty
        else
            seq {
                yield! dirs |> Seq.collect (fun d -> d.EnumerateFiles())
                yield! dirs
                       |> Seq.collect (fun d -> d.EnumerateDirectories())
                       |> allFilesInDir
            }

    let createDirectory (filter) (dir : System.IO.DirectoryInfo) =
        { Id = dir.Name |> IdFromFile
          Source = dir
          Files =
              [| dir |]
              |> allFilesInDir
              |> Seq.where filter
              |> Seq.map createFile }

    type Component =
        { Id : string
          Guid : System.Guid
          Files : File seq }

    type YesNo = 
    | Yes
    | No
    with 
        override this.ToString() = 
            match this with
            | Yes -> sprintf "yes"
            | No -> sprintf "no"

    type Package = {
        Description : string
        Manufacturer : string
        InstallerVersion : int
        Compressed : YesNo
        } with 
            member this.ToXmlElement() =
                elem (name "Package") 
                |> attribs [ name "Id" @= "*"
                             name "Description" @= this.Description
                             name "Manufacturer" @= this.Manufacturer.ToString()
                             name "InstallerVersion" @= this.InstallerVersion.ToString()
                             name "Compressed" @= this.Compressed.ToString() ]
    let DefaultPackage = 
        {   Description = System.String.Empty
            Manufacturer = System.String.Empty
            InstallerVersion = 200
            Compressed = Yes 
        }
    let createPackage (updatePackage : Package -> Package) =
        DefaultPackage
        |> updatePackage

    type Product =
        { Id : System.Guid
          Language : string
          Manufacturer : string
          Name : string
          UpgradeCode : System.Guid
          Version : Version
          Package : Package }
        member this.ToXmlElement() =
            elem (name "Product")
            |> attribs [ name "Name" @= this.Name
                         name "Id" @= this.Id.ToString("D")
                         name "UpgradeCode" @= this.UpgradeCode.ToString("D")
                         name "Version" @= this.Version.ToString()
                         name "Manufacturer" @= this.Manufacturer.ToString() ]
            |> content [ this.Package.ToXmlElement() ]
    let consolidateProduct (product : Product) = {
            product with 
                Package = {product.Package with Manufacturer = product.Manufacturer}
            }
    let DefaultProduct =
        { Id = System.Guid.NewGuid()
          Language = System.String.Empty
          Manufacturer = System.String.Empty
          Name = System.String.Empty
          UpgradeCode = System.Guid.NewGuid()
          Version = createVersion 0 0 1
          Package = DefaultPackage }
          |> consolidateProduct
    
    let createProduct (updateProduct : Product -> Product) = 
        DefaultProduct 
        |> updateProduct
        |> consolidateProduct

    type Bundle =
        { Setups : string seq }
        member this.ToXmlElement() = elem (name "Bundle")

    type Wix =
        | Bundle of Bundle
        | Product of Product
        member this.ToXmlElement() =
            let xe =
                match this with
                | Bundle b -> b.ToXmlElement()
                | Product p -> p.ToXmlElement()
            elem (qname "http://schemas.microsoft.com/wix/2006/wi" "Wix")
            |> content [ xe ]
