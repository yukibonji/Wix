#if INTERACTIVE
#load "LightXml.fsx"
#endif

open LightXml

open System
open System.IO

[<AutoOpen>]
module Wix =
    type Version =
        {
            Major : int
            Minor : int
            Build : int
            Revision : int option
        }

        override this.ToString() =
            match this.Revision with
            | Some r -> sprintf "%i.%i.%i.%i" this.Major this.Minor this.Build this.Revision.Value
            | None -> sprintf "%i.%i.%i" this.Major this.Minor this.Build

        member this.IncRevision() =
            { this with Revision =
                            match this.Revision with
                            | Some r -> Some(r + 1)
                            | None -> Some(1) }

    type AutogenGuid =
        | Unic of Guid
        | Auto
        with override this.ToString() =
                match this with
                | Unic guid -> guid.ToString("D")
                | Auto -> "*"

    type Architecture =
        | X86
        | X64
        with override archi.ToString() =
                match archi with
                | X86 -> "x86"
                | X64 -> "x64"

    type YesOrNo =
        | Yes
        | No
        with
            override this.ToString() =
                match this with
                | Yes -> sprintf "yes"
                | No -> sprintf "no"

    
    type LocalizableInteger =
        | Integer of int
        | Localization of string
        with
            override this.ToString() =
                match this with
                | Integer v -> v.ToString()
                | Localization s -> s

    let private appendIfDefined items item =
        match item with
        | Some v -> Seq.append items [v]
        | None -> items

    type Permission = 
        {
            Read : YesOrNo option
            Write : YesOrNo option
        }
        with 
            member this.ToXmlElement() =
                elem (name "PermissionEx")
                // |> attribs (Seq.empty |> appendIfDefined this.Read)

    

    type WixFile =
        {
            Id : string
            Source : IO.FileInfo
            Permission : Permission option
        }
        member this.ToXmlElement() =
            elem (name "File")
            |> attribs [  name "Id" @= this.Id
                          name "Name" @= this.Source.Name
                          name "Source" @= this.Source.FullName ]

    type WixDirectory =
        {
            Id : string
            Source : IO.DirectoryInfo
            Files : WixFile seq
            Permission : Permission option
        }
        member this.ToXmlElement() =
            elem (name "Directory")
            |> attribs [ name "Id" @= this.Id.ToString()
                         name "Name" @= this.Source.FullName ]
            |> content (this.Files |> Seq.map (fun f -> f.ToXmlElement()))
            

    type WixComponent =
        {
            Id : string
            Guid : Guid
            Files : WixFile seq
        }

    type WixPackage = {
        Id : AutogenGuid
        Description : string
        Manufacturer : string
        InstallerVersion : int
        Compressed : YesOrNo
        } with
            member this.ToXmlElement() =
                elem (name "Package")
                |> attribs [ name "Id" @= this.Id.ToString()
                             name "Description" @= this.Description
                             name "Manufacturer" @= this.Manufacturer.ToString()
                             name "InstallerVersion" @= this.InstallerVersion.ToString()
                             name "Compressed" @= this.Compressed.ToString() ]

    type WixProduct =
        {
            Id : AutogenGuid
            Language : LocalizableInteger
            Manufacturer : string
            Name : string
            UpgradeCode : Guid
            Version : Version
            Package : WixPackage
        }
        member this.ToXmlElement() =
            elem (name "Product")
            |> attribs [ name "Name" @= this.Name
                         name "Id" @= this.Id.ToString()
                         name "Language" @= this.Language.ToString()
                         name "UpgradeCode" @= this.UpgradeCode.ToString("D")
                         name "Version" @= this.Version.ToString()
                         name "Manufacturer" @= this.Manufacturer.ToString() ]
            |> content [ this.Package.ToXmlElement() ]


    type WixBundle =
        { Setups : string seq }
        member this.ToXmlElement() = elem (name "Bundle")

    type Wix =
        | WixBundle of WixBundle
        | WixProduct of WixProduct
        member this.ToXmlElement() =
            let xe =
                match this with
                | WixBundle b -> b.ToXmlElement()
                | WixProduct p -> p.ToXmlElement()
            elem (qname "http://schemas.microsoft.com/wix/2006/wi" "Wix")
            |> content [ xe ]

open Wix

module Version =
    let create maj min bf : Version =
        {
            Major = maj
            Minor = min
            Build = bf
            Revision = None
        }

module WixFile =
    let IdFromFile(file : string) =
        Path.GetFileNameWithoutExtension(file) + Path.GetExtension(file).ToUpper().Replace(".", "")

    let create (fi : FileInfo) : WixFile =
        {
            Id = fi.Name |> IdFromFile
            Source = fi
            Permission = None
        }


module WixDirectory =
    let rec walk (dirs : DirectoryInfo seq) =
        if Seq.isEmpty dirs then Seq.empty
        else
            seq {
                yield! dirs
                    |> Seq.collect (fun d -> d.EnumerateFiles())
                yield! dirs
                    |> Seq.collect (fun d -> d.EnumerateDirectories())
                    |> walk
            }

    let create (filter) (dir : DirectoryInfo) : WixDirectory =
        {
            Id = dir.Name |> WixFile.IdFromFile
            Source = dir
            Files =
                [| dir |]
                |> walk
                |> Seq.where filter
                |> Seq.map WixFile.create
            Permission = None
        }

module WixPackage =
    let create (updatePackage : WixPackage -> WixPackage) : WixPackage =
        let defaultPackage : WixPackage =
            {
                Id = Auto
                Description = String.Empty
                Manufacturer = String.Empty
                InstallerVersion = 200
                Compressed = Yes
            }
        defaultPackage
        |> updatePackage

module WixProduct =
    let create (updateProduct : WixProduct -> WixProduct) : WixProduct =
        let consolidateProduct (product : WixProduct) = {
                product with
                    Package = {product.Package with Manufacturer = product.Manufacturer}
                }
        let defaultProduct : WixProduct =
          {
            Id = Auto
            Language = Integer 1033
            Manufacturer = String.Empty
            Name = String.Empty
            UpgradeCode = Guid.NewGuid()
            Version = Version.create 0 0 1
            Package = (WixPackage.create Operators.id)
          }
        defaultProduct
        |> updateProduct
        |> consolidateProduct
