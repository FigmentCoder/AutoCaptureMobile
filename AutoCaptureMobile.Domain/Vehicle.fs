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
module Model =

    type Model = Model of string
    
    let pattern = "^[a-zA-Z]{2,50}$"

    let value (Model value) = value
    
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
            Modelv : Model
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
                Model.validate vehicle.Modelv

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