#if INTERACTIVE
#load "src/Wix.fsx"
#endif
open LightXml
open Wix

let f = createFile (System.IO.FileInfo "test.txt")
f.ToXmlElement() |> XElement.ToString

let d = createDirectory (fun f -> f.Extension.Equals(".png")) (System.IO.DirectoryInfo ".")
d.ToXmlElement() |> XElement.ToString

let w = Product (createProduct (fun p ->
  {
    p with
      Name = "MyApp"
      Manufacturer = "Developer"
      Id = System.Guid.NewGuid()
      Package = createPackage (fun pack ->
        {pack with 
          Compressed = No
        } 
      )
  }))
w.ToXmlElement() |> XElement.ToString
