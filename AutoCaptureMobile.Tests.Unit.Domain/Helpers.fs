namespace AutoCaptureMobile.Tests.Unit.Domain

open AutoCaptureMobile.Common

[<AutoOpen>]
module Predicates =
    
    let mustMatchThePattern typeName pattern input =
        let message = sprintf "%s '%s' must match the pattern '%s'" typeName input pattern
        error message

    let mustNotBeNullOrEmpty typeName =
        let message = sprintf "%s must not be null or empty" typeName
        error  message

    let isNotValid name pattern value actual =
        (mustNotBeNullOrEmpty name = actual) ||
        (mustMatchThePattern name pattern value = actual)

    let areEqual left right =
        left = right

[<AutoOpen>]
module Letters =

    let lowerLetters =
        {'a' .. 'z'}

    let uppLetters =
        {'A' .. 'Z'}

    let letters =
        [lowerLetters; uppLetters] 
        |> Seq.concat

    let isLetter c = 
        letters
        |> Seq.contains c

    let isLetters (s:string) =
        s <> null && 
        s.Length > 1 &&
        Seq.forall isLetter s

[<AutoOpen>]
module Digits =

    let digits =
         {0..9} 

[<AutoOpen>]
module String =

    let isLessThan51 (s:string) =
        s <> null &&
        s.Length < 51

[<AutoOpen>]
module Random =

    open System

    let random xs = 
        let r = Random()
        xs |> Seq.sortBy (fun _ -> r.Next())

    let randomHead xs =
        random xs
        |> Seq.head