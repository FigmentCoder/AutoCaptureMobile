namespace AutoCaptureMobile.Presentation

open System

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

open AutoCaptureMobile.Domain
open AutoCaptureMobile.Tests.Infrastructure.Repositories

module CustomerPage =

    type Msg =
        | UpdateName of string
        | UpdatePhone of string
        | UpdateEmail of string

        | CustomerSelected of Customer option

        //| CustomerAdded of Customer
        //| CustomerUpdated of Customer

    type ExternalMsg =
        | NoOp
        | NavigateBackToEstimateWithCustomer of Customer option

    type Model =
        {
            Customer: Customer option
            Name: Name
            Phone: Phone
            Email: Email
            IsNameValid: bool
            IsPhoneValid: bool
            IsEmailValid: bool
        }

    //let createOrUpdateAsync dbPath customer = async {
    //    match customer.Id with
    //    | 0 ->
    //        let! insertedToDoItem = create dbPath customer
    //        return CustomerAdded insertedCustomer
    //    | _ ->
    //        let! updatedToDoItem = update dbPath customer
    //        return CustomerUpdated updatedCustomer
    //}

    let sayVehicleIsNotValidAsync () =
        Application.Current.MainPage.DisplayAlert(
            "Invalid Vehicle",
            "Please create a valid Vehicle",
            "Cancel") |> Async.AwaitTask

    //let trySaveAsync model dbPath = async {
    //    if not model.IsNameValid || not model.IsPhoneValid || not model.IsEmailValid then
    //        do! sayVehicleIsNotValidAsync ()
    //        return None
    //    else
    //        let id = (match model.Customer with None -> 0 | Some c -> c.Id)
    //        let vehicle =
    //            {
    //                Id = id
    //                Name = model.Name
    //                Phone = model.Phone
    //                Email = model.Email
    //            }
    //        let! msg = createOrUpdateAsync dbPath newCustomer
    //        return Some msg
    //}

    let validateName = not << String.IsNullOrWhiteSpace
    let validatePhone = not << String.IsNullOrWhiteSpace
    let validateEmail = not << String.IsNullOrWhiteSpace

    let init (customer: Customer option) =
        let model =
            match customer with
            | Some c ->
                {
                    Customer = Some c
                    Name = c.Name
                    Phone = c.Phone
                    Email = c.Email
                    IsNameValid = true
                    IsPhoneValid = true
                    IsEmailValid = true
                }
            | None ->
                {
                    Customer = None
                    Name = CustomerFactory.customer1.Name
                    Phone = CustomerFactory.customer1.Phone
                    Email = CustomerFactory.customer1.Email
                    IsNameValid = true
                    IsPhoneValid = true
                    IsEmailValid = true
                }

        model, Cmd.none

    let update msg model =
        match msg with
        | UpdateName name ->
            let m = { model with Name = Name name; IsNameValid = (validateName name) }
            m, Cmd.none, ExternalMsg.NoOp
        | UpdatePhone phone ->
            let m = { model with Phone = Phone phone; IsPhoneValid = (validatePhone phone) }
            m, Cmd.none, ExternalMsg.NoOp
        | UpdateEmail email ->
            let m = { model with Email = Email email; IsEmailValid = (validateEmail email) }
            m, Cmd.none, ExternalMsg.NoOp

        | CustomerSelected customer ->
            model, Cmd.none, ExternalMsg.NavigateBackToEstimateWithCustomer customer

    let view model dispatch =

        let updateName = UpdateName >> dispatch
        let updatePhone = UpdatePhone >> dispatch
        let updateEmail = UpdateEmail >> dispatch

        let goBackToEstimate = fun () -> dispatch (CustomerSelected model.Customer)

        View.ContentPage(
            title = "Customer Page",
            backgroundColor = Color.Gray,
            content =
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    padding = new Thickness(10.0),
                    children = [

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            margin = new Thickness(0., 0., 0., 10.0),
                            children = [

                                View.Entry(
                                    placeholder = "Name",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand,
                                    text = Helpers.getName(model.Name),
                                    textChanged = (fun n -> n.NewTextValue |> updateName)
                                )

                                View.Label(
                                    text = "*",
                                    fontSize = FontSize.fromValue 20.,
                                    textColor = Color.Red,
                                    horizontalOptions = LayoutOptions.End,
                                    isVisible = not model.IsNameValid
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            margin = new Thickness(0., 0., 0., 10.0),
                            children = [
                                View.Entry(
                                    placeholder = "Phone",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand,
                                    text = Helpers.getCustomerPhone(model.Phone),
                                    textChanged = (fun p -> p.NewTextValue |> updatePhone)
                                )

                                View.Label(
                                    text = "*",
                                    fontSize = FontSize.fromValue 20.,
                                    textColor = Color.Red,
                                    horizontalOptions = LayoutOptions.End,
                                    isVisible = not model.IsPhoneValid
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            margin = new Thickness(0., 0., 0., 10.0),
                            children = [
                                View.Entry(
                                    placeholder = "Email",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand,
                                    text = Helpers.getCustomerEmail(model.Email),
                                    textChanged = (fun e -> e.NewTextValue |> updateEmail)
                                )

                                View.Label(
                                    text = "*",
                                    fontSize = FontSize.fromValue 20.,
                                    textColor = Color.Red,
                                    horizontalOptions = LayoutOptions.End,
                                    isVisible = not model.IsEmailValid
                                )
                            ]
                        )

                        View.Button(
                            text = "Save",
                            backgroundColor = Color.Green,
                            textColor = Color.White,
                            verticalOptions = LayoutOptions.EndAndExpand,
                            width = 300.0,
                            cornerRadius = 10,
                            padding = new Thickness(15.),
                            command = goBackToEstimate
                        )
                    ]
                )
        )