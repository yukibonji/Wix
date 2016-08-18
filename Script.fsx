#if INTERACTIVE
#load "src/LightXml.fsx"
#endif
open LightXml

// LightXml Usage
let xml = elem (qname "http://myschema" "root")
            |> content [
                elem (name "Person")
            |> attribs [name "id" @= "js1"]
            |> content [elem (name "FullName") |> value "John Smith" ]]
        |> XElement.fromXmlElement

