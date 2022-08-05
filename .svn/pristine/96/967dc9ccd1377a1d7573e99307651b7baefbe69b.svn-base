namespace AutoCaptureMobile.Domain

open AutoCaptureMobile.Common

[<AutoOpen>]
[<RequireQualifiedAccessAttribute>]
module Phone =

    type Phone = Phone of string

    let pattern = "^(\+\d{1,2}\s)?\(?\d{3}\)?[\s.-]\d{3}[\s.-]\d{4}$"

    let value (Phone value) = value

    let validate phone =
        value phone |> 
        validateStringType phone pattern

[<AutoOpen>]
[<RequireQualifiedAccessAttribute>]
module Email = 

    type Email = Email of string

    let pattern = "^([\w\.\-]+)@([\w\-]+)((\.(\w){2,3})+)$"

    let value (Email value) = value

    let validate email =
        value email |> 
        validateStringType email pattern

[<AutoOpen>]
[<RequireQualifiedAccessAttribute>]
module Customer =

    type Customer =
        {
            Name: Name
            Phone: Phone
            Email: Email
        }

    let validate customer = 
        result { 
            let! name = 
                Name.validate customer.Name
            let! phone = 
                Phone.validate customer.Phone
            let! email = 
                Email.validate customer.Email
            
            return { 
                Name = name
                Phone = phone
                Email = email
            }
        }