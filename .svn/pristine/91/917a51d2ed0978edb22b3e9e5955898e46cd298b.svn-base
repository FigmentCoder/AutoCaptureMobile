namespace AutoCaptureMobile.Presentation

open System

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

open AutoCaptureMobile.Domain

module LocationPage =

    type Msg =
        | UpdateName of string
        | UpdateStreet of string
        | UpdateCountry of string
        | UpdateState of string
        | UpdatePostalCode of string

        | GoToCountry
        | GoToState
        //| GoBackToEstimate

        | LocationSelected of Location option

        //Update after select
        | CountryUpdated of string
        | StateUpdated of string


    type ExternalMsg =
        | NoOp
        | NavigateToCountry
        | NavigateToState
        | NavigateBackToEstimateWithLocation of Location option

    type Model =
        {
            Location: Location option
            Name: Name
            Street: string
            Country: string
            State: string
            PostalCode: string
            IsNameValid: bool
            IsStreetValid: bool
            IsCountryValid: bool
            IsStateValid: bool
            IsPostalCodeValid: bool
        }

    let validateName = not << String.IsNullOrWhiteSpace
    let validateStreet = not << String.IsNullOrWhiteSpace
    let validateCountry = not << String.IsNullOrWhiteSpace
    let validateState = not << String.IsNullOrWhiteSpace
    let validatePostalCode = not << String.IsNullOrWhiteSpace

    let init (location: Location option) =
        let model =
            match location with
            | Some l ->
                {
                    Location = Some l
                    Name = l.Name
                    Street = l.Street
                    Country = l.Country
                    State = l.State
                    PostalCode = l.PostalCode
                    IsNameValid = true
                    IsStreetValid = true
                    IsCountryValid = true
                    IsStateValid = true
                    IsPostalCodeValid = true
                }
            | None ->
                {
                    Location = None
                    Name = Name ""
                    Street = ""
                    Country = ""
                    State = ""
                    PostalCode = ""
                    IsNameValid = false
                    IsStreetValid = false
                    IsCountryValid = false
                    IsStateValid = false
                    IsPostalCodeValid = false
                }

        model, Cmd.none

    let update msg model =
        match msg with
        | UpdateName name ->
            let m = { model with Name = Name name; IsNameValid = (validateName name) }
            m, Cmd.none, ExternalMsg.NoOp
        | UpdateStreet street ->
            let m = { model with Street = street; IsStreetValid = (validateStreet street) }
            m, Cmd.none, ExternalMsg.NoOp
        | UpdateCountry country ->
            let m = { model with Country = country; IsCountryValid = (validateCountry country) }
            m, Cmd.none, ExternalMsg.NoOp
        | UpdateState state ->
            let m = { model with State = state; IsStateValid = (validateState state) }
            m, Cmd.none, ExternalMsg.NoOp
        | UpdatePostalCode postalCode ->
            let m = { model with PostalCode = postalCode; IsPostalCodeValid = (validatePostalCode postalCode) }
            m, Cmd.none, ExternalMsg.NoOp
        | GoToCountry ->
            model, Cmd.none, ExternalMsg.NavigateToCountry
        | GoToState ->
            model, Cmd.none, ExternalMsg.NavigateToState

        //| GoBackToEstimate ->
        //    model, Cmd.none, ExternalMsg.NavigateBackToEstimateWithLocation model.Location

        | CountryUpdated country ->
            { model with Country = country }, Cmd.none, ExternalMsg.NoOp
        | StateUpdated state ->
            { model with State = state }, Cmd.none, ExternalMsg.NoOp

        | LocationSelected location ->
            { model with Location = location }, Cmd.none, ExternalMsg.NoOp

    let view model dispatch =

        let updateName = UpdateName >> dispatch
        let updateStreet = UpdateStreet >> dispatch
        let updateCountry = UpdateCountry >> dispatch
        let updateState = UpdateState >> dispatch
        let updatePostalCode = UpdatePostalCode >> dispatch

        let goBackToEstimate = fun () -> dispatch (LocationSelected model.Location)

        View.ContentPage(
            title = "Location Page",
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
                                    placeholder = "Street",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand,
                                    text = model.Street,
                                    textChanged = (fun s -> s.NewTextValue |> updateStreet)
                                )

                                View.Label(
                                    text = "*",
                                    fontSize = FontSize.fromValue 20.,
                                    textColor = Color.Red,
                                    horizontalOptions = LayoutOptions.End,
                                    isVisible = not model.IsStreetValid
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            margin = new Thickness(0., 0., 0., 10.0),
                            children = [

                                View.Entry(
                                    placeholder = "Country",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand,
                                    text = model.Country,
                                    textChanged = (fun c -> c.NewTextValue |> updateCountry)
                                )

                                View.Button(
                                    text = "->",
                                    backgroundColor = Color.Blue,
                                    textColor = Color.White,
                                    width = 50.0,
                                    horizontalOptions = LayoutOptions.End,
                                    command = fun () -> dispatch GoToCountry
                                )

                                View.Label(
                                    text = "*",
                                    fontSize = FontSize.fromValue 20.,
                                    textColor = Color.Red,
                                    horizontalOptions = LayoutOptions.End,
                                    isVisible = not model.IsCountryValid
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            margin = new Thickness(0., 0., 0., 10.0),
                            children = [

                                View.Picker(
                                    title = "State",
                                    horizontalOptions = LayoutOptions.FillAndExpand,
                                    items =
                                        if model.Country = Helpers.canada then
                                            Helpers.listOfProvinces
                                        else
                                            Helpers.listOfStates
                                )

                                View.Entry(
                                    placeholder = "State",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand,
                                    text = model.State,
                                    textChanged = (fun s -> s.NewTextValue |> updateState)
                                )

                                View.Button(
                                    text = "->",
                                    backgroundColor = Color.Blue,
                                    textColor = Color.White,
                                    width = 50.0,
                                    horizontalOptions = LayoutOptions.End,
                                    command = fun () -> dispatch GoToState
                                )

                                View.Label(
                                    text = "*",
                                    fontSize = FontSize.fromValue 20.,
                                    textColor = Color.Red,
                                    horizontalOptions = LayoutOptions.End,
                                    isVisible = not model.IsStateValid
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            margin = new Thickness(0., 0., 0., 10.0),
                            children = [

                                View.Entry(
                                    placeholder = "Postal Code:",
                                    height = 45.0,
                                    margin = new Thickness(0., 0., 0., 10.0),
                                    horizontalOptions = LayoutOptions.FillAndExpand,
                                    text = model.PostalCode,
                                    textChanged = (fun p -> p.NewTextValue |> updatePostalCode)
                                )

                                View.Label(
                                    text = "*",
                                    fontSize = FontSize.fromValue 20.,
                                    textColor = Color.Red,
                                    horizontalOptions = LayoutOptions.End,
                                    isVisible = not model.IsPostalCodeValid
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