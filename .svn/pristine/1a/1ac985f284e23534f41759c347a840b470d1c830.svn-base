namespace AutoCaptureMobile.Common

open System
open System.Text.RegularExpressions

[<AutoOpen>]
module System =

    let isNullOrEmpty string =
        String.IsNullOrEmpty(string)

    let isNotNullOrEmpty string =
        not (isNullOrEmpty string)

    let isMatch pattern input =
        isNotNullOrEmpty pattern &&
        isNotNullOrEmpty input &&
        Regex.IsMatch(input, pattern)

    let isNotMatch pattern input =
        not (isMatch pattern input)

    let newLine =
        System.Environment.NewLine
    
    let concat seq =
        String.concat ", " seq

    let space =
        " "