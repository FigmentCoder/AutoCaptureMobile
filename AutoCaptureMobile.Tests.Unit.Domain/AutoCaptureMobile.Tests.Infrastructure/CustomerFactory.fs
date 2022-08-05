namespace AutoCaptureMobile.Tests.Infrastructure.Repositories

open AutoCaptureMobile.Domain

module CustomerFactory =

    let customer1 =
        {
            Name = Name "Fred Jones"
            Phone = Phone "636 282-7300"
            Email = Email "fj@tjco.net"
        }

    let customer2 =
        {
            Name = Name "Brian Newton"
            Phone = Phone "636 282-7300"
            Email = Email "bn@tjco.net"
        }