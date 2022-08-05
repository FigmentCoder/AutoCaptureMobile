namespace AutoCaptureMobile.Tests.Infrastructure.Repositories

open AutoCaptureMobile.Domain

module LocationFactory =

    let location1 =
        {
            Name = Name "Location 1"
            Street = "123 Test Lane"
            Country = "United States"
            State = "Missouri"
            PostalCode = "63010"
        }

    let location2 =
        {
            Name = Name "Location 2"
            Street = "456 Test Lane"
            Country = "United States"
            State = "Missouri"
            PostalCode = "63026"
        }