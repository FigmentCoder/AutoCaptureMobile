namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

module StateListPage =

    type Msg =
        | StateSelected of string

    type ExternalMsg =
        | NoOp
        | NavigateBackToLocationWithState of string

    type Model =
        {
            Country: string
            States: string list
        }

    let initModel =
        {
            Country = "United States"
            States = Helpers.listOfStates
        }

    let init () =
        initModel, Cmd.none

    let update msg model =
        match msg with
        | StateSelected state ->
            model, Cmd.none, ExternalMsg.NavigateBackToLocationWithState state

    let view model dispatch =

        View.ContentPage(
            title = "List of States",
            content =
                View.ListView(
                    verticalOptions = LayoutOptions.FillAndExpand,
                    horizontalOptions = LayoutOptions.FillAndExpand,
                    rowHeight = 50,
                    selectionMode = ListViewSelectionMode.None,
                    itemTapped = (fun i ->
                        let selectedState = model.States.Item(i)
                        dispatch (StateSelected selectedState)),
                    //itemTapped = (fun i ->
                    //    let selectedItem = openItems.Item(i)
                    //    dispatch (ToDoItemSelected selectedItem)),
                    items = [
                        for state in model.States do
                            View.ViewCell(
                                View.StackLayout(
                                    orientation = StackOrientation.Vertical,
                                    children = [
                                        View.Label(
                                            text = state,
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