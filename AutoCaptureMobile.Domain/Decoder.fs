namespace AutoCaptureMobile.Domain

[<RequireQualifiedAccessAttribute>]
module Decoder =

       type VinPattern = VinPattern of string

       let decode (vinPatterns:seq<VinPattern>) (vin:Vin) =
           Make "", Model "", Year 0 

//make vinPattern unit -> seq<VinPattern> ?