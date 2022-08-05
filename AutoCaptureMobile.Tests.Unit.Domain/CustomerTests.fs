namespace AutoCaptureMobile.Tests.Unit.Domain

open Xunit
open FsCheck
open FsCheck.Xunit

open AutoCaptureMobile.Common
open AutoCaptureMobile.Domain
open AutoCaptureMobile.Tests.Unit.Domain.Predicates

module PhoneGenerator =
    
    open FsCheck.Gen

    let format i =
        let rec loop (s:string) =
            match s with
            | s when s.Length >= 4 -> s
            | _ -> loop("0" + s)
        loop (sprintf "%i" i)

    let phone a p s =
        let s = format s
        Phone (sprintf "%i-%i-%s" a p s)

    let generator =
        gen {
            let! a = choose (100, 999)
            let! p = choose (100, 999)
            let! s = choose (0000, 9999)
            
            return phone a p s
        }

    type Phones =
        static member Phones() =
            { new Arbitrary<Phone>() with
                override x.Generator = generator }

module PhoneTests = 

    [<Property(Arbitrary = [| typeof<PhoneGenerator.Phones> |])>]
    let ``phone ok`` phone =
        
        let expected =
            phone

        let actual =
            Phone.validate expected

        Assert.Equal(ok expected, actual)

    [<Property>]
    let ``phone error`` value =
        
        let expected =
            Phone value

        let actual =
            Phone.validate expected

        (isNotMatch Phone.pattern value) ==>
            lazy isNotValid "Phone" Phone.pattern value actual

module EmailGenerator =
    
    open FsCheck.Gen

    let email u d = 
        Email (sprintf "%s@%s" u d)

    let generator =
        gen {
            let! u = elements ["a"; "b"; "c"; "d"; "e"]
            let! d = elements ["gmail.com"; "outlook.com"]

            return email u d
        }

    type Emails =
        static member Emails() =
            { new Arbitrary<Email>() with
                override x.Generator = generator }

module EmailTests = 
    
    [<Property(Arbitrary = [| typeof<EmailGenerator.Emails> |])>]
    let ``email ok`` email =
        
        let expected =
            email

        let actual =
            Email.validate expected

        Assert.Equal(ok expected, actual)

    [<Property>]
    let ``email error`` value =
        
        let expected =
            Email value

        let actual =
            Email.validate expected

        (isNotMatch Email.pattern value) ==>
            lazy isNotValid "Email" Email.pattern value actual

module CustomerTests =

    [<Fact>]
    let ``customer ok``() =

        let expected =
            { 
                Name = Name "Ed"
                Phone = Phone "314-651-4788"
                Email = Email "jim@tjco.net" 
            }

        let actual =
            Customer.validate expected

        Assert.Equal(ok expected, actual)

    [<Fact>]
    let ``customer error name``() =
        
        let value = "x"
        let expected =
            { 
                Name = Name value
                Phone = Phone "314-651-4788"
                Email = Email "as@tjco.net"
            }

        let actual =
            Customer.validate expected

        Assert.True(isNotValid "Name" Name.pattern value actual)

    [<Fact>]
    let ``customer error phone``() =

        let value = "x"
        let expected =
            { 
                Name = Name "Jim"
                Phone = Phone value
                Email = Email "as@tjco.net"
            }

        let actual =
            Customer.validate expected

        Assert.True(isNotValid "Phone" Phone.pattern value actual)
       
    [<Fact>]
    let ``customer error email``() =

        let value = "x"
        let expected =
            { 
                Name = Name "Jim"
                Phone = Phone "314-651-4788"
                Email = Email value
            }

        let actual =
            Customer.validate expected

        Assert.True(isNotValid "Email" Email.pattern value actual)