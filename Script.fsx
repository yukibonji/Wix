#if INTERACTIVE
#load "src/Wix.fsx"
#endif
open LightXml
open Wix

let f = createFile (System.IO.FileInfo "test.txt")
f.ToXmlElement() |> XElement.ToString

let d = createDirectory (fun f -> f.Extension.Equals(".png")) (System.IO.DirectoryInfo ".")
d.ToXmlElement() |> XElement.ToString

let w = Product (createProduct (fun p -> {p with 
    Name = "MyApp"
    Id = System.Guid.NewGuid()}))
w.ToXmlElement() |> XElement.ToString


let xe = elem (qname "youpi" "top")
            |> content [elem (name "sub") |> content [elem (name "subsub")]]

xe |> XElement.ToString