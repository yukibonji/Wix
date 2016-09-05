#if INTERACTIVE
#load "src/Wix.fsx"
#endif
open LightXml
open Wix


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
w.ToXmlElement() |> XElement.ToString
