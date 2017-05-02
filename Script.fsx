#if INTERACTIVE
#load "src/Wix.fsx"
#endif
open LightXml
open Wix

#if INTERACTIVE
#r "System.Xml.Linq.dll"
#r "packages/FSharp.Data.Xsd/lib/net45/FSharp.Data.Xsd.dll"
#endif

open FSharp.Data

let f = WixFile.create (System.IO.FileInfo "test.txt")
f.ToXmlElement() |> XElement.ToString

let d = WixDirectory.create (fun f -> f.Extension.Equals(".png")) (System.IO.DirectoryInfo ".")
d.ToXmlElement() |> XElement.ToString

let w = WixProduct (WixProduct.create (fun prod ->
  {
    prod with
      Name = "MyApp"
      Manufacturer = "Developer"
      Package = WixPackage.create (fun pkg ->
        {
          pkg with 
            Compressed = No
            Description = "Some wonderfull functionalities"
        } 
      )
  }))
let wx = w.ToXmlElement() |> XElement.ToString

type WixType = XmlProvider<Schema = "src/data/wix.xsd">
let project = WixType.Parse wx
project.Wix.Value.Product.Value.Package.Description