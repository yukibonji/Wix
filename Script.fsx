#if INTERACTIVE
#load "src/LightXml.fsx"
#endif
open LightXml

// LightXml Usage
let xml = elem (name "Person")
            |> attribs [name "id" @= "js1"; name "alt" @= "super"]
            |> content [elem (name "FullName") |> value "John Smith" ]
        |> XElement.fromXmlElement

xml.ToString()