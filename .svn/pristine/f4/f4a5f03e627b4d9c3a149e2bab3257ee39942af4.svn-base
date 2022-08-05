namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

module MakeListPage =

    type Msg =
        | MakeSelected of string

    type ExternalMsg =
        | NoOp
        | NavigateBackToEstimateWithMake of string

    type Model =
        {
            Makes: string list
        }

    let initModel =
        {
            Makes = Helpers.listOfMakes
        }

    let init () =
        initModel, Cmd.none

    let update msg model =
        match msg with
        | MakeSelected make ->
            model, Cmd.none, ExternalMsg.NavigateBackToEstimateWithMake make

    let view model dispatch =

        View.ContentPage(
            title = "List of Makes",
            content =
                View.ListView(
                    verticalOptions = LayoutOptions.FillAndExpand,
                    horizontalOptions = LayoutOptions.FillAndExpand,
                    rowHeight = 50,
                    selectionMode = ListViewSelectionMode.None,
                    itemTapped = (fun i ->
                        let selectedMake = model.Makes.Item(i)
                        dispatch (MakeSelected selectedMake)),
                    items = [
                        for make in model.Makes do
                            View.ViewCell(
                                View.StackLayout(
                                    orientation = StackOrientation.Vertical,
                                    children = [
                                        View.Label(
                                            text = make,
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