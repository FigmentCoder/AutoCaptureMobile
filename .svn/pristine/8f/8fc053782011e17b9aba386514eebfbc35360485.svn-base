namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

module ModelListPage =

    type Msg =
        | ModelSelected of string

    type ExternalMsg =
        | NoOp
        | NavigateBackToEstimateWithModel of string

    type Model =
        {
            Models: string list
        }

    let initModel =
        {
            Models = Helpers.listOfModels
        }

    let init () =
        initModel, Cmd.none

    let update msg model =
        match msg with
        | ModelSelected modelv ->
            model, Cmd.none, ExternalMsg.NavigateBackToEstimateWithModel modelv

    let view model dispatch =

        View.ContentPage(
            title = "List of Models",
            content =
                View.ListView(
                    verticalOptions = LayoutOptions.FillAndExpand,
                    horizontalOptions = LayoutOptions.FillAndExpand,
                    rowHeight = 50,
                    selectionMode = ListViewSelectionMode.None,
                    itemTapped = (fun i ->
                        let selectedModel = model.Models.Item(i)
                        dispatch (ModelSelected selectedModel)),
                    items = [
                        for mModel in model.Models do
                            View.ViewCell(
                                View.StackLayout(
                                    orientation = StackOrientation.Vertical,
                                    children = [
                                        View.Label(
                                            text = mModel,
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