namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

module NotesPhotosPage =

    type Msg =
        | GoToHome

    type ExternalMsg =
        | NoOp
        | NavigateBackToHome

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
        | GoToHome ->
            model, Cmd.none, ExternalMsg.NavigateBackToHome

    let view model dispatch =

        View.ContentPage(
            title = "Notes/Photos Page",
            backgroundColor = Color.Gray,
            content =
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    padding = new Thickness(10.0),
                    children = [

                        View.Label(
                            text = "Notes"
                        )

                        View.Editor(
                            text = "Notes:",
                            height = 120.0,
                            horizontalOptions = LayoutOptions.FillAndExpand
                        )

                        View.Label(
                            text = "Photos:"
                        )

                        View.Button(
                            text = "Save",
                            backgroundColor = Color.Blue,
                            textColor = Color.White,
                            verticalOptions = LayoutOptions.EndAndExpand,
                            width = 300.0,
                            cornerRadius = 10,
                            padding = new Thickness(15.),
                            command = fun () -> dispatch GoToHome
                        )
                    ]
                )
        )