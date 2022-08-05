namespace AutoCaptureMobile.Tests.Unit.Domain

open Xunit
open FsCheck
open FsCheck.Xunit

open AutoCaptureMobile.Common
open AutoCaptureMobile.Domain
open AutoCaptureMobile.Tests.Unit.Domain.Predicates

module NameGenerator =

    type Names = 
        static member Names =
            Arb.Default.String()
            |> Arb.filter isLetters
            |> Arb.convert Name string

module NameTests = 

    [<Property(Arbitrary = [| typeof<NameGenerator.Names> |])>]
    let ``name ok`` name =

        let expected =
            name

        let actual =
            Name.validate expected

        Assert.Equal(ok expected, actual)

    [<Property>]
    let ``name error`` value =
        let expected =
            Name value

        let actual =
            Name.validate expected

        (isNotMatch Name.pattern value) ==>
            lazy isNotValid "Name" Name.pattern value actual