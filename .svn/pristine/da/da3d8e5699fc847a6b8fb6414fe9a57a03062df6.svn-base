namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

module YearListPage =

    type Msg =
        | YearSelected of string

    type ExternalMsg =
        | NoOp
        | NavigateBackToEstimateWithYear of string

    type Model =
        {
            Years: string list
        }

    let initModel =
        {
            Years = Helpers.listOfYears
        }

    let init () =
        initModel, Cmd.none

    let update msg model =
        match msg with
        | YearSelected year ->
            model, Cmd.none, ExternalMsg.NavigateBackToEstimateWithYear year

    let view model dispatch =

        View.ContentPage(
            title = "List of Years",
            content =
                View.ListView(
                    verticalOptions = LayoutOptions.FillAndExpand,
                    horizontalOptions = LayoutOptions.FillAndExpand,
                    rowHeight = 50,
                    selectionMode = ListViewSelectionMode.None,
                    itemTapped = (fun i ->
                        let selectedYear = model.Years.Item(i)
                        dispatch (YearSelected selectedYear)),
                    items = [
                        for year in model.Years do
                            View.ViewCell(
                                View.StackLayout(
                                    orientation = StackOrientation.Vertical,
                                    children = [
                                        View.Label(
                                            text = year,
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