namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

module CalculatePage =

    type Msg =
        | GoToNotesPhotos

    type ExternalMsg =
        | NoOp
        | NavigateToNotesPhotos

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
        | GoToNotesPhotos ->
            model, Cmd.none, ExternalMsg.NavigateToNotesPhotos

    let view model dispatch =

        View.ContentPage(
            title = "Calculate Page",
            backgroundColor = Color.Gray,
            content =
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    padding = new Thickness(10.0),
                    children = [

                        View.Label(
                            text = "Wheel"
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            horizontalOptions = LayoutOptions.FillAndExpand,
                            children = [

                                View.Entry(
                                    placeholder = "Wheel price:",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand
                                )

                                View.CheckBox(
                                    isChecked = false,
                                    color = Color.White,
                                    horizontalOptions = LayoutOptions.End
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            horizontalOptions = LayoutOptions.FillAndExpand,
                            children = [
                                
                                View.Entry(
                                    placeholder = "Value:",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand
                                )

                                View.CheckBox(
                                    isChecked = false,
                                    color = Color.White,
                                    horizontalOptions = LayoutOptions.End
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            horizontalOptions = LayoutOptions.FillAndExpand,
                            children = [
                                
                                View.Entry(
                                    placeholder = "Nut:",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand
                                )

                                View.CheckBox(
                                    isChecked = false,
                                    color = Color.White,
                                    horizontalOptions = LayoutOptions.End
                                )
                            ]
                        )

                        View.Label(
                            text = "Battery"
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            horizontalOptions = LayoutOptions.FillAndExpand,
                            children = [                               

                                View.Entry(
                                    placeholder = "Terminal:",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand
                                )

                                View.CheckBox(
                                    isChecked = false,
                                    color = Color.White,
                                    horizontalOptions = LayoutOptions.End
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            horizontalOptions = LayoutOptions.FillAndExpand,
                            children = [

                                View.Entry(
                                    placeholder = "Wire:",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand
                                )

                                View.CheckBox(
                                    isChecked = false,
                                    color = Color.White,
                                    horizontalOptions = LayoutOptions.End
                                )
                            ]
                        )


                        View.Button(
                            text = "Next",
                            backgroundColor = Color.Blue,
                            textColor = Color.White,
                            verticalOptions = LayoutOptions.EndAndExpand,
                            width = 300.0,
                            cornerRadius = 10,
                            padding = new Thickness(15.),
                            command = fun () -> dispatch GoToNotesPhotos
                        )
                    ]
                )
        )