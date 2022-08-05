namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

open AutoCaptureMobile.Domain
open AutoCaptureMobile.Tests.Infrastructure.Repositories

module HistoryPage =

    type Msg =
        | AddNewEstimate
        | VehiclesLoaded of Vehicle list
        | VehicleSelected of Vehicle
        | VehicleUpdated

    type ExternalMsg =
        | NoOp
        | NavigateToNewEstimate
        | NavigateToExistingEstimate of Vehicle

    type Model =
        {
            Vehicles: Vehicle list
        }

    let initModel =
        {
            Vehicles = VehicleFactory.vehicles
        }

    let init () =
        initModel, Cmd.none

    let update msg model =
        match msg with
        | AddNewEstimate ->
            model, Cmd.none, ExternalMsg.NavigateToNewEstimate
        | VehiclesLoaded vehicles ->
            let m = { model with Vehicles = vehicles }
            m, Cmd.none, ExternalMsg.NoOp
        | VehicleSelected vehicle ->
            model, Cmd.none, ExternalMsg.NavigateToExistingEstimate vehicle

    let view model dispatch =

        View.ContentPage(
            title = "History Page",
            toolbarItems = [
                View.ToolbarItem(
                    text = "Upload"
                    //command = fun () -> dispatch ()
                )

                View.ToolbarItem(
                    text = "Config"
                    //command = fun () -> dispatch ()
                )

                View.ToolbarItem(
                    text = "+",
                    command = fun () -> dispatch AddNewEstimate
                )
            ],
            content =
                View.ListView(
                    verticalOptions = LayoutOptions.FillAndExpand,
                    rowHeight = 50,
                    selectionMode = ListViewSelectionMode.None,
                    itemTapped = (fun i ->
                        let selectedVehicle = model.Vehicles.Item(i)
                        dispatch (VehicleSelected selectedVehicle)),
                    items = [
                        for vehicle in model.Vehicles do
                            View.ViewCell(
                                View.StackLayout(
                                    orientation = StackOrientation.Horizontal,
                                    margin = new Thickness(20., 5., 5., 0.),
                                    children = [

                                        View.StackLayout(
                                            orientation = StackOrientation.Vertical,
                                            children = [

                                                View.Label(
                                                    text = Helpers.getVehicleVin(vehicle.Vin),
                                                    fontSize = FontSize.fromValue 14.,
                                                    textColor = Color.Black,
                                                    verticalOptions = LayoutOptions.Start
                                                )

                                                View.Label(
                                                    text = Helpers.getName(vehicle.Customer.Name),
                                                    fontSize = FontSize.fromValue 12.,
                                                    textColor = Color.Black,
                                                    verticalOptions = LayoutOptions.End
                                                )
                                            ]
                                        )

                                        View.Label(
                                            text = ">",
                                            fontSize = FontSize.fromValue 34.0,
                                            horizontalOptions = LayoutOptions.EndAndExpand
                                        )
                                    ]
                                )
                            )
                    ]
                )
        )