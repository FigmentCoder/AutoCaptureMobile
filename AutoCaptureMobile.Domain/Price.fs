namespace AutoCaptureMobile.Domain

open AutoCaptureMobile.Common

[<AutoOpen>]
[<RequireQualifiedAccessAttribute>]
module Price =

    type Price =
        {
            Key : string
            Amount : decimal
        }

    let validate price =
        if price.Key = "" then
            ok price
        else error ""

    let calculate prices =
        prices |> List.sumBy(fun p -> p.Amount)

    //The App should allow multiple Price constructs for different scenarios
    //Export Prices
    //Import Prices
    
    //Validation functions for these single case unions and use <*> ?
    //Look at importing a Money library or re-create one in F#

    //Key - CalcType
        //Key - Category
            //Key - Component
            //Value - Amount

    //Start with this concrete type, you can always look at something more dynamic later