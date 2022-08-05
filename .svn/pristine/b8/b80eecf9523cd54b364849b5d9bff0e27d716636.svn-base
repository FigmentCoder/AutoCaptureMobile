namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

module ConfigPage =

    type Msg =
        | Test

    type ExternalMsg =
        | NoOp

    type Model =
        {
            Thing: string
        }

    let initModel =
        {
            Thing = "test"
        }

    let init () =
        initModel, Cmd.none

    let update msg model =
        match msg with
        | Test ->
            model, Cmd.none, ExternalMsg.NoOp

    let view model dispatch =

        View.ContentPage(
            title = "Config Page",
            content =
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    padding = new Thickness(10.0),
                    verticalOptions = LayoutOptions.Center,
                    horizontalOptions = LayoutOptions.Center,
                    children = [
                        View.Label(
                            text = "Config Page",
                            fontSize = FontSize.fromValue 20.
                        )
                    ]
                )
        )