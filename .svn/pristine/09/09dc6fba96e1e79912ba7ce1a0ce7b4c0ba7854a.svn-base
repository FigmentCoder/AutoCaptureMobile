namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open AutoCaptureMobile.Domain

open Xamarin.Forms

module EstimatePage =

    type Msg =
        | GoToScanVin
        | GoToMakeList
        | GoToModelList
        | GoToYearList
        | GoToCustomer
        | GoToLocation
        | GoToCalculate

        //| SaveVehicle
        //| DeleteVehicle of Vehicle

        //Update after select
        | MakeUpdated of string
        | ModelUpdated of string
        | YearUpdated of string
        | CustomerUpdated of Customer option
        | LocationUpdated of Location option

    type ExternalMsg =
        | NoOp
        | NavigateToScanVin
        | NavigateToMakeList
        | NavigateToModelList
        | NavigateToYearList
        | NavigateToCustomer
        | NavigateToLocation
        | NavigateToCalculate

    type Model =
        {
            Vehicle: Vehicle option
            Make: string
            Modelv: string
            Year: string
            Customer: Customer option
            Location: Location option
        }

    let init (vehicle: Vehicle option) =

        let model =
            match vehicle with
            | Some v ->
                {
                    Vehicle = Some v
                    Make = Helpers.getVehicleMake(v.Make)
                    Modelv = Helpers.getVehicleModel(v.Modelv)
                    Year = Helpers.getVehicleYear(v.Year).ToString()
                    Customer = Some v.Customer
                    Location = v.Location
                }
            | None ->
                {
                    Vehicle = None
                    Make = ""
                    Modelv = ""
                    Year = ""
                    Customer = None
                    Location = None
                }

        model, Cmd.none

    let update dbPath msg model =
        match msg with
        | GoToScanVin ->
            model, Cmd.none, ExternalMsg.NavigateToScanVin
        | GoToMakeList ->
            model, Cmd.none, ExternalMsg.NavigateToMakeList
        | GoToModelList ->
            model, Cmd.none, ExternalMsg.NavigateToModelList
        | GoToYearList ->
            model, Cmd.none, ExternalMsg.NavigateToYearList
        | GoToCustomer ->
            model, Cmd.none, ExternalMsg.NavigateToCustomer
        | GoToLocation ->
            model, Cmd.none, ExternalMsg.NavigateToLocation
        | GoToCalculate ->
            model, Cmd.none, ExternalMsg.NavigateToCalculate

        | MakeUpdated make ->
            { model with Make = make }, Cmd.none, ExternalMsg.NoOp
        | ModelUpdated modelv ->
            { model with Modelv = modelv }, Cmd.none, ExternalMsg.NoOp
        | YearUpdated year ->
            { model with Year = year }, Cmd.none, ExternalMsg.NoOp
        | CustomerUpdated customer ->
            { model with Customer = customer }, Cmd.none, ExternalMsg.NoOp
        | LocationUpdated location ->
            { model with Location = location }, Cmd.none, ExternalMsg.NoOp

        //| SaveVehicle ->
        //    let cmd = Cmd.ofAsyncMsgOption (trySaveAsync model dbPath)
        //    model, cmd, ExternalMsg.NoOp

        //| DeleteVehicle vehicle ->
        //    let cmd = Cmd.ofAsyncMsgOption (tryDeleteAsync dbPath vehicle)
        //    model, cmd, ExternalMsg.NoOp

    let view model dispatch =

        View.ContentPage(
            title = "Estimate Page",
            backgroundColor = Color.Gray,
            content =
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    padding = new Thickness(10.0),
                    children = [

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            margin = new Thickness(0., 0., 0., 10.0),
                            children = [

                                View.Entry(
                                    placeholder = "Vin",                                    
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand,
                                    text = Helpers.getVehicleVin(model.Vehicle.Value.Vin)
                                )

                                View.Button(
                                    text = "->",
                                    backgroundColor = Color.Blue,
                                    textColor = Color.White,
                                    width = 50.0,
                                    horizontalOptions = LayoutOptions.End,
                                    command = fun () -> dispatch GoToScanVin
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            margin = new Thickness(0., 0., 0., 10.0),
                            children = [

                                View.Entry(
                                    placeholder = "Make:",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand,
                                    text = model.Make
                                    //textChanged = (fun m -> m.NewTextValue |> updateMake)
                                )

                                View.Button(
                                    text = "->",
                                    backgroundColor = Color.Blue,
                                    textColor = Color.White,
                                    width = 50.0,
                                    horizontalOptions = LayoutOptions.End,
                                    command = fun () -> dispatch GoToMakeList
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            margin = new Thickness(0., 0., 0., 10.0),
                            children = [

                                View.Entry(
                                    placeholder = "Model:",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand,
                                    text = model.Modelv
                                    //textChanged = (fun m -> m.NewTextValue |> updateModel)
                                )

                                View.Button(
                                    text = "->",
                                    backgroundColor = Color.Blue,
                                    textColor = Color.White,
                                    width = 50.0,
                                    horizontalOptions = LayoutOptions.End,
                                    command = fun () -> dispatch GoToModelList
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            margin = new Thickness(0., 0., 0., 10.0),
                            children = [

                                View.Entry(
                                    placeholder = "Year:",
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand,
                                    text = model.Year
                                    //textChanged = (fun m -> m.NewTextValue |> updateYear)
                                )

                                View.Button(
                                    text = "->",
                                    backgroundColor = Color.Blue,
                                    textColor = Color.White,
                                    width = 50.0,
                                    horizontalOptions = LayoutOptions.End,
                                    command = fun () -> dispatch GoToYearList
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            margin = new Thickness(0., 0., 0., 10.0),
                            children = [

                                View.Entry(
                                    placeholder = "Customer",
                                    height = 45.0,
                                    text = Helpers.getName(model.Customer.Value.Name),
                                    horizontalOptions = LayoutOptions.FillAndExpand
                                )

                                View.Button(
                                    text = "->",
                                    backgroundColor = Color.Blue,
                                    textColor = Color.White,
                                    width = 50.0,
                                    horizontalOptions = LayoutOptions.End,
                                    command = fun () -> dispatch GoToCustomer
                                )
                            ]
                        )

                        View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            margin = new Thickness(0., 0., 0., 10.0),
                            children = [

                                View.Entry(
                                    placeholder = "Location",
                                    text = Helpers.getName(model.Location.Value.Name),
                                    height = 45.0,
                                    horizontalOptions = LayoutOptions.FillAndExpand
                                )

                                View.Button(
                                    text = "->",
                                    backgroundColor = Color.Blue,
                                    textColor = Color.White,
                                    width = 50.0,
                                    horizontalOptions = LayoutOptions.End,
                                    command = fun () -> dispatch GoToLocation
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
                            command = fun () -> dispatch GoToCalculate
                        )
                    ]
                )
        )