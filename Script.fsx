#if INTERACTIVE
#load "src/LightXml.fsx"
#load "src/Wix.fsx"
#endif
open LightXml
open Wix

let f = createFile (System.IO.FileInfo "test.txt")
f.ToXmlElement() |> XElement.ToString

let d = createDirectory (fun f -> f.Extension.Equals(".png")) (System.IO.DirectoryInfo ".")
d.ToXmlElement() |> XElement.ToString