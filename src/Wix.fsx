[<StructuredFormatDisplay("{Major}.{Minor}.{BugFix}")>]
type Version = {
        Major : int
        Minor : int
        BugFix : int
    }
let version maj min bf = {Major = maj; Minor = min; BugFix = bf}

[<StructuredFormatDisplay("{Index}_{Name}")>]
type UniqueIdentifier = {
        Index : int64
        Name : string
    }
let mutable internal uid = (int64)0
let uniqueIdentifier name = 
    uid <- uid + (int64)1
    {Index = uid; Name = name}

type Component = {
        Id : UniqueIdentifier
        Guid : System.Guid
        Files : File seq
    }
and File = {
        Id : UniqueIdentifier
        Name : string
        Source : System.IO.FileInfo
    }


type Wix = 
    | Bundle of Bundle
    | Product of Product

and Product = {
        Name : string
        Code : System.Guid
        Version : Version
        Publisher : string
        UpgradeGuid : System.Guid
    }

and Bundle = {
        Setups : string seq
    }