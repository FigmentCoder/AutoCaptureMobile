namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

module LoginPage =

    type Msg =
        | GoToMain
        //| IsSpinningTrue

    type ExternalMsg =
        | NoOp
        | NavigateToMain

    type Model =
        {
            Thing: string
            //IsSpinning: bool
        }

    let init () =
        {
            Thing = "string"
            //IsSpinning = false
        }, Cmd.none

    let update msg model = 
        match msg with
        | GoToMain ->
            model, Cmd.none, NavigateToMain

        //| IsSpinningTrue ->
        //    { model with IsSpinning = true }, Cmd.none, ExternalMsg.NoOp

    let view model dispatch =

        View.ContentPage(
            title = "Login",
            backgroundColor = Color.Gray,
            toolbarItems = [
                View.ToolbarItem(
                    text = "Update"
                    //command = fun () -> dispatch IsSpinningTrue
                )
            ],
            content =
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    verticalOptions = LayoutOptions.Center,
                    horizontalOptions = LayoutOptions.Center,
                    //margin = new Thickness(0., 50.0, 0., 0.),
                    children = [

                        //View.ActivityIndicator(
                        //    horizontalOptions = LayoutOptions.Center,
                        //    verticalOptions = LayoutOptions.Center,
                        //    color = Color.White,
                        //    margin = new Thickness(0., 0., 10., 0.),
                        //    isRunning = model.IsSpinning
                        //)

                        View.Entry(
                            placeholder = "User Name",
                            keyboard = Keyboard.Text,
                            width = 250.0
                            //text = mModel.Username,
                            //textChanged = (fun e -> e.NewTextValue |> checkUsername)
                        )

                        View.Entry(
                            placeholder = "Password",
                            keyboard = Keyboard.Text,
                            width = 250.0
                            //text = mModel.Password,
                            //textChanged = (fun e -> e.NewTextValue |> checkPassword)
                        )

                        View.Button(
                            text = "Log In",
                            backgroundColor = Color.Black,
                            textColor = Color.White,
                            width = 100.0,
                            cornerRadius = 10,
                            horizontalOptions = LayoutOptions.Center,
                            borderColor = Color.White,
                            borderWidth = 2.0,
                            command = fun () -> dispatch (GoToMain)
                        )
                    ]
                )
        )