namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

module HomePage =

   type Msg =
       | GoToEstimate
       | GoToHistory
       | GoToConfig

   type ExternalMsg =
       | NoOp
       | NavigateToEstimate
       | NavigateToHistory
       | NavigateToConfig

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
       | GoToEstimate ->
           model, Cmd.none, ExternalMsg.NavigateToEstimate
       | GoToHistory ->
           model, Cmd.none, ExternalMsg.NavigateToHistory
       | GoToConfig ->
           model, Cmd.none, ExternalMsg.NavigateToConfig

   let view model dispatch =

       View.ContentPage(
           title = "Home Page",
           backgroundColor = Color.White,
           //toolbarItems = [
           //    View.ToolbarItem(
           //        text = "Config",
           //        command = fun () -> dispatch GoToConfig
           //    )
           //],
           content =
               View.StackLayout(
                   orientation = StackOrientation.Vertical,
                   verticalOptions = LayoutOptions.Center,
                   horizontalOptions = LayoutOptions.Center,
                   children = [

                       View.Button(
                           text = "Estimate",
                           backgroundColor = Color.Blue,
                           textColor = Color.White,
                           width = 300.0,
                           cornerRadius = 10,
                           padding = new Thickness(15.),
                           margin = new Thickness(0., 0., 0., 20.),
                           horizontalOptions = LayoutOptions.End,
                           command = fun () -> dispatch GoToEstimate
                       )

                       View.Button(
                           text = "History",
                           backgroundColor = Color.Blue,
                           textColor = Color.White,
                           width = 300.0,
                           cornerRadius = 10,
                           padding = new Thickness(15.),
                           margin = new Thickness(0., 0., 0., 20.),
                           horizontalOptions = LayoutOptions.End,
                           command = fun () -> dispatch GoToHistory
                       )

                       View.Button(
                           text = "Config",
                           backgroundColor = Color.Blue,
                           textColor = Color.White,
                           width = 300.0,
                           cornerRadius = 10,
                           padding = new Thickness(15.),
                           horizontalOptions = LayoutOptions.End,
                           command = fun () -> dispatch GoToConfig
                       )
                   ]
               )
       )