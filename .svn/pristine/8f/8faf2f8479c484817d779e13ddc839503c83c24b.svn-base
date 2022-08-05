namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

module CountryListPage =

    type Msg =
        | CountrySelected of string

    type ExternalMsg =
        | NoOp
        | NavigateBackToLocationWithCountry of string

    type Model =
        {
            Countries: string list
        }

    let initModel =
        {
            Countries = Helpers.listOfCountries
        }

    let init () =
        initModel, Cmd.none

    let update msg model =
        match msg with
        | CountrySelected country ->
            model, Cmd.none, ExternalMsg.NavigateBackToLocationWithCountry country

    let view model dispatch =

        View.ContentPage(
            title = "List of Countries",
            content =
                View.ListView(
                    verticalOptions = LayoutOptions.FillAndExpand,
                    horizontalOptions = LayoutOptions.FillAndExpand,
                    rowHeight = 50,
                    selectionMode = ListViewSelectionMode.None,
                    itemTapped = (fun i ->
                        let selectedCountry = model.Countries.Item(i)
                        dispatch (CountrySelected selectedCountry)),
                    items = [
                        for country in model.Countries do
                            View.ViewCell(
                                View.StackLayout(
                                    orientation = StackOrientation.Vertical,
                                    children = [
                                        View.Label(
                                            text = country,
                                            fontSize = FontSize.fromValue 18.,
                                            margin = new Thickness(20., 15., 0., 0.),
                                            textColor = Color.Black,
                                            verticalOptions = LayoutOptions.Center,
                                            horizontalOptions = LayoutOptions.Start
                                        )
                                    ]
                                )
                            )
                    ]
                )
        )