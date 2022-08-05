namespace AutoCaptureMobile.Tests.Infrastructure.Repositories

open AutoCaptureMobile.Domain

module VehicleFactory =

    let photoList =
        [
            Photo "photo1"
            Photo "photo2"
        ]

    let noteList =
        [
            Note "note1"
            Note "note2"
        ]

    let price1 =
        {
            Key = "test"
            Amount = decimal 1.0
        }

    let price2 =
        {
            Key = "test"
            Amount = decimal 10.0
        }

    let priceList =
        [
            price1
            price2
        ]

    let vehicle1 =
        {
            Vin = Vin "123"
            Make = Make "Acura"
            Modelv = Modelv "RSX"
            Year = Year 2008
            Customer = CustomerFactory.customer1
            Location = Some LocationFactory.location1
            Photos = photoList
            Notes = noteList
            Prices = priceList
            Total = decimal 0.20
        }

    let vehicle2 =
        {
            Vin = Vin "456"
            Make = Make "Chevrolet"
            Modelv = Modelv "Mailbu"
            Year = Year 2020
            Customer = CustomerFactory.customer2
            Location = Some LocationFactory.location2
            Photos = photoList
            Notes = noteList
            Prices = priceList
            Total = decimal 0.40
        }

    let vehicles =
        [
            vehicle1
            vehicle2
        ]