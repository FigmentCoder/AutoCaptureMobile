namespace AutoCaptureMobile.Domain

open AutoCaptureMobile.Common

[<AutoOpen>]
[<RequireQualifiedAccessAttribute>]
module Name =
    
    type Name = Name of string

    let pattern = "^[a-zA-Z]{2,}$"

    let value (Name value) = value

    let validate name =
        value name |> 
        validateStringType name pattern