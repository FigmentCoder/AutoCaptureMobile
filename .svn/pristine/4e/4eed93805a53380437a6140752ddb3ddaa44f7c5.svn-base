namespace AutoCaptureMobile.Domain

open AutoCaptureMobile.Common
open AutoCaptureMobile.Domain

[<AutoOpen>]
[<RequireQualifiedAccessAttribute>]
module Vin = 
    
    type Vin = Vin of string

    let pattern = "[0-9A-Z[^IOQ]{17}"

    let value (Vin value) = value

    let validate vin =
        value vin |>
        validateStringType vin pattern

[<AutoOpen>]
[<RequireQualifiedAccessAttribute>]
module Make =
    
    type Make = Make of string
    
    let pattern = "^[a-zA-Z]{2,50}$"

    let value (Make value) = value

    let validate make =
        value make |>
        validateStringType make pattern

[<AutoOpen>]
[<RequireQualifiedAccessAttribute>]
module Modelv =

    type Modelv = Modelv of string
    
    let pattern = "^[a-zA-Z]{2,50}$"

    let value (Modelv value) = value
    
    let validate model =
        value model |>
        validateStringType model pattern

[<AutoOpen>]
[<RequireQualifiedAccessAttribute>]
module Year =

    type Year = Year of int
        
    let value (Year value) = value

    let validate year =
        let value = value year
        match value with
        | x when x > 0 -> ok year
        | _ -> error "is 0 or less"
 
[<AutoOpen>]
[<RequireQualifiedAccessAttribute>]
module Photo =
    
    type Photo = Photo of string 

    let pattern = ""

    let value (Photo value) = value

    let validate photo =
        value photo |>
        validateStringType photo pattern

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Note = 

    type Note = Note of string

    let pattern = ""

    let value (Note value) = value

    let validate note =
        value note |>
        validateStringType note pattern

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Vehicle =

    type Vehicle =
        {
            Vin : Vin
            Make : Make
            Modelv : Modelv
            Year : Year
            Customer : Customer
            Location : Location option
            Photos : Photo list
            Notes : Note list
            Prices : Price List
            Total : decimal
        }

    let validate vehicle =
        if vehicle.Vin = Vin "" then
            ok vehicle
        else error ""

    let validate' vehicle =
        result {
            let! vin =
                Vin.validate vehicle.Vin
            
            let! make =
                Make.validate vehicle.Make

            let! model =
                Modelv.validate vehicle.Modelv

            let! photos =
                vehicle.Photos 
                |> List.map Photo.validate
                |> Result.sequence

            let! notes =
                vehicle.Notes
                |> List.map Note.validate
                |> Result.sequence

            let! customer =
                Customer.validate vehicle.Customer

            return {
                Vin = vin
                Make = make
                Modelv = model
                Year = vehicle.Year
                Customer = customer
                Location = vehicle.Location
                Photos = photos
                Notes = notes
                Prices = vehicle.Prices
                Total = vehicle.Total
            }      
        }

 //make types private and parse rather than validate?
 //Event I wonder if that should come out and might evolve like Price
 //Validation functions(private ?) for these single case unions and use <*> ?
 //Make types private?
 //DU of two Customer Records?
 //How client consumes may determine API and whether you keep validate or use create and make types private 
 //Evaluate the use of AutoOpen and RequireQualifiedAccess
 //Create private lift in validate function use applicative
 //DI MVU?