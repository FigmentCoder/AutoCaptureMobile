namespace AutoCaptureMobile.Tests.Unit.Domain

open Xunit
open FsCheck
open FsCheck.Xunit

open AutoCaptureMobile.Common
open AutoCaptureMobile.Domain
open AutoCaptureMobile.Tests.Unit.Domain.Predicates

module VinGenerator =
    
    let first = 
        seq {"1";"2";"4";"5"}
    
    let others = 
        let ds = 
            digits 
            |> Seq.map string
        let ls =
            uppLetters
            |> Seq.except ['I';'O';'Q']
            |> Seq.map string
        Seq.concat[ds;ls]
    
    let generator =
        gen {
            let! one = 
               first 
               |> Gen.elements
            
            let! two = 
                others 
                |> Gen.elements
            
            let! three = 
                others 
                |> Gen.elements      

            let fourteen = 
                Gen.elements others 
                |> Gen.sample 0 14 
                |> String.concat ""

            return Vin (one + two + three + fourteen)
        }

    type Vins =
        static member Vins() =
            { new Arbitrary<Vin>() with
                override x.Generator = generator }

module VinTests =
    
    [<Property(Arbitrary = [| typeof<VinGenerator.Vins> |])>]
    let ``vin ok`` vin =
        let expected =
            vin

        let actual =
            Vin.validate expected

        Assert.Equal(ok expected, actual)

    [<Property>]
    let ``vin error`` value =
        let expected =
            Vin value

        let actual =
            Vin.validate expected

        (isNotMatch Vin.pattern value) ==>
            lazy isNotValid "Vin" Vin.pattern value actual

module MakeGenerator =

    type Makes = 
        static member Makes =
            Arb.Default.NonEmptyString()
            |> Arb.convert string NonEmptyString
            |> Arb.filter isLessThan51
            |> Arb.filter isLetters
            |> Arb.convert Make string

module MakeTests =

    [<Property(Arbitrary = [| typeof<MakeGenerator.Makes> |])>]
    let ``make ok`` make =

        let expected =
            make

        let actual =
            Make.validate expected

        Assert.Equal(ok make, actual)

    [<Property(MaxTest = 500)>]
    let ``make error`` value =

        let expected =
            Make value

        let actual =
            Make.validate expected

        (isNotMatch Make.pattern value) ==>
            lazy isNotValid "Make" Make.pattern value actual

module ModelGenerator =
    
    type Models = 
        static member Models =
            Arb.Default.NonEmptyString()
            |> Arb.convert string NonEmptyString
            |> Arb.filter isLessThan51
            |> Arb.filter isLetters
            |> Arb.convert Modelv string


module ModelTests =
    
    [<Property(Arbitrary = [| typeof<ModelGenerator.Models> |])>]
    let ``model ok`` model =
        
        let expected =
            model

        let actual =
            Modelv.validate expected

        Assert.Equal(ok model, actual)


    [<Property(MaxTest = 500)>]
    let ``model error`` value =

        let expected =
            Modelv value

        let actual =
            Modelv.validate expected

        (isNotMatch Modelv.pattern value) ==>
            lazy isNotValid "Model" Modelv.pattern value actual

module YearGenerator =

    type Years =
        static member Years =
            Arb.Default.PositiveInt()
            |> Arb.convert int PositiveInt
            |> Arb.convert Year Year.value
    
module VehicleTests = 

    [<Fact>]
    let ``vehicle ok``() =
        ()