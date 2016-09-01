#if INTERACTIVE
#load "src/Wix.fsx"
#endif
open LightXml
open Wix

let f = createFile (System.IO.FileInfo "test.txt")
f.ToXmlElement() |> XElement.ToString

let d = createDirectory (fun f -> f.Extension.Equals(".png")) (System.IO.DirectoryInfo ".")
d.ToXmlElement() |> XElement.ToString

let w = Product (createProduct (fun prod ->
  {
    prod with
      Name = "MyApp"
      Manufacturer = "Developer"
      Package = createPackage (fun pkg ->
        {
          pkg with 
            Compressed = No
            Description = "Some wonderfull functionalities"
        } 
      )
  }))
w.ToXmlElement() |> XElement.ToString
