namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

open AutoCaptureMobile.Domain

module App =

    type Msg =
        //| LoginPageMsg of LoginPage.Msg
        | MainPageMsg of MainPage.Msg
        | HomePageMsg of HomePage.Msg

        | EstimatePageMsg of EstimatePage.Msg
        | ScanVinPageMsg of ScanVinPage.Msg
        | MakeListPageMsg of MakeListPage.Msg
        | ModelListPageMsg of ModelListPage.Msg
        | YearListPageMsg of YearListPage.Msg
        | CustomerPageMsg of CustomerPage.Msg
        | LocationPageMsg of LocationPage.Msg
        | CountryListPageMsg of CountryListPage.Msg
        | StateListPageMsg of StateListPage.Msg
        | CalculatePageMsg of CalculatePage.Msg
        | NotesPhotosPageMsg of NotesPhotosPage.Msg
        | HistoryPageMsg of HistoryPage.Msg
        | ConfigPageMsg of ConfigPage.Msg

        | GoToHome

        | GoToEstimate of Vehicle option
        | GoToScanVin
        | GoToMakeList
        | GoToModelList
        | GoToYearList
        | GoToCustomer of Customer option
        | GoToLocation of Location option
        | GoToCountryList
        | GoToStateList
        | GoToCalculate
        | GoToNotesPhotos
        | GoToHistory
        | GoToConfig
        //Estimate
        | BackToEstimateWithMake of string
        | BackToEstimateWithModel of string
        | BackToEstimateWithYear of string
        | BackToEstimateWithCustomer of Customer option
        | BackToEstimateWithLocation of Location option
        //Location
        | BackToLocationWithCountry of string
        | BackToLocationWithState of string

        | BackToMain

        | NavigationPopped

    type Model =
        {
            //LoginPageModel: LoginPage.Model
            MainPageModel: MainPage.Model
            HomePageModel: HomePage.Model option
            EstimatePageModel: EstimatePage.Model option
            ScanVinPageModel: ScanVinPage.Model option
            MakeListPageModel: MakeListPage.Model option
            ModelListPageModel: ModelListPage.Model option
            YearListPageModel: YearListPage.Model option
            CustomerPageModel: CustomerPage.Model option
            LocationPageModel: LocationPage.Model option
            CountryListPageModel: CountryListPage.Model option
            StateListPageModel: StateListPage.Model option
            CalculatePageModel: CalculatePage.Model option
            NotesPhotosPageModel: NotesPhotosPage.Model option
            HistoryPageModel: HistoryPage.Model option
            ConfigPageModel: ConfigPage.Model option
            
            WorkaroundNavPageBug: bool
            WorkaroundNavPageBugPendingCmd: Cmd<Msg>
        }

    type Pages =
        {
            //LoginPage: ViewElement
            MainPage: ViewElement
            HomePage: ViewElement option
            EstimatePage: ViewElement option
            ScanVinPage: ViewElement option
            MakeListPage: ViewElement option
            ModelListPage: ViewElement option
            YearListPage: ViewElement option
            CustomerPage: ViewElement option
            LocationPage: ViewElement option
            CountryListPage: ViewElement option
            StateListPage: ViewElement option
            CalculatePage: ViewElement option
            NotesPhotosPage: ViewElement option
            HistoryPage: ViewElement option
            ConfigPage: ViewElement option
        }

    let init dbPath () =

        //let loginModel, loginMsg = LoginPage.init()
        let mainModel, mainMsg = MainPage.init()

        let initialModel =
            {
                //LoginPageModel = loginModel
                MainPageModel = mainModel
                HomePageModel = None
                EstimatePageModel = None
                ScanVinPageModel = None
                MakeListPageModel = None
                ModelListPageModel = None
                YearListPageModel = None
                CustomerPageModel = None
                LocationPageModel = None
                CountryListPageModel = None
                StateListPageModel = None
                CalculatePageModel = None
                NotesPhotosPageModel = None
                HistoryPageModel = None
                ConfigPageModel = None

                WorkaroundNavPageBug = false
                WorkaroundNavPageBugPendingCmd = Cmd.none
            }
        //initialModel, (Cmd.map LoginPageMsg loginMsg)
        initialModel, (Cmd.map MainPageMsg mainMsg)

    //let handleLoginExternalMsg externalMsg =
    //    match externalMsg with
    //    | LoginPage.ExternalMsg.NoOp           -> Cmd.none
    //    | LoginPage.ExternalMsg.NavigateToMain -> Cmd.ofMsg GoToMain

    let handleMainExternalMsg externalMsg =
        match externalMsg with
        | MainPage.ExternalMsg.NoOp                               -> Cmd.none
        | MainPage.ExternalMsg.NavigateToNewEstimate              -> Cmd.ofMsg (GoToEstimate None)
        | MainPage.ExternalMsg.NavigateToExistingEstimate vehicle -> Cmd.ofMsg (GoToEstimate vehicle)

    let handleHomeExternalMsg externalMsg =
        match externalMsg with
        | HomePage.ExternalMsg.NoOp               -> Cmd.none
        | HomePage.ExternalMsg.NavigateToEstimate -> Cmd.ofMsg (GoToEstimate None)
        | HomePage.ExternalMsg.NavigateToHistory  -> Cmd.ofMsg GoToHistory
        | HomePage.ExternalMsg.NavigateToConfig   -> Cmd.ofMsg GoToConfig

    let handleEstimateExternalMsg externalMsg =
        match externalMsg with
        | EstimatePage.ExternalMsg.NoOp                -> Cmd.none
        | EstimatePage.ExternalMsg.NavigateToScanVin   -> Cmd.ofMsg GoToScanVin
        | EstimatePage.ExternalMsg.NavigateToMakeList  -> Cmd.ofMsg GoToMakeList
        | EstimatePage.ExternalMsg.NavigateToModelList -> Cmd.ofMsg GoToModelList
        | EstimatePage.ExternalMsg.NavigateToYearList  -> Cmd.ofMsg GoToYearList
        | EstimatePage.ExternalMsg.NavigateToCustomer  -> Cmd.ofMsg (GoToCustomer None)
        | EstimatePage.ExternalMsg.NavigateToLocation  -> Cmd.ofMsg (GoToLocation None)
        | EstimatePage.ExternalMsg.NavigateToCalculate -> Cmd.ofMsg GoToCalculate

    let handleScanVinExternalMsg externalMsg =
        match externalMsg with
        | ScanVinPage.ExternalMsg.NoOp -> Cmd.none

    let handleMakeListExternalMsg externalMsg =
        match externalMsg with
        | MakeListPage.ExternalMsg.NoOp                                -> Cmd.none
        | MakeListPage.ExternalMsg.NavigateBackToEstimateWithMake make -> Cmd.ofMsg (BackToEstimateWithMake make)

    let handleModelListExternalMsg externalMsg =
        match externalMsg with
        | ModelListPage.ExternalMsg.NoOp                                   -> Cmd.none
        | ModelListPage.ExternalMsg.NavigateBackToEstimateWithModel modelv -> Cmd.ofMsg (BackToEstimateWithModel modelv)

    let handleYearListExternalMsg externalMsg =
        match externalMsg with
        | YearListPage.ExternalMsg.NoOp                                -> Cmd.none
        | YearListPage.ExternalMsg.NavigateBackToEstimateWithYear year -> Cmd.ofMsg (BackToEstimateWithYear year)

    let handleCustomerExternalMsg externalMsg =
        match externalMsg with
        | CustomerPage.ExternalMsg.NoOp                                        -> Cmd.none
        | CustomerPage.ExternalMsg.NavigateBackToEstimateWithCustomer customer -> Cmd.ofMsg (BackToEstimateWithCustomer customer)

    let handleLocationExternalMsg externalMsg =
        match externalMsg with
        | LocationPage.ExternalMsg.NoOp                                        -> Cmd.none
        | LocationPage.ExternalMsg.NavigateToCountry                           -> Cmd.ofMsg GoToCountryList
        | LocationPage.ExternalMsg.NavigateToState                             -> Cmd.ofMsg GoToStateList
        | LocationPage.ExternalMsg.NavigateBackToEstimateWithLocation location -> Cmd.ofMsg (BackToEstimateWithLocation location)

    let handleCountryListExternalMsg externalMsg =
        match externalMsg with
        | CountryListPage.ExternalMsg.NoOp                                      -> Cmd.none
        | CountryListPage.ExternalMsg.NavigateBackToLocationWithCountry country -> Cmd.ofMsg (BackToLocationWithCountry country)

    let handleStateListExternalMsg externalMsg =
        match externalMsg with
        | StateListPage.ExternalMsg.NoOp                                  -> Cmd.none
        | StateListPage.ExternalMsg.NavigateBackToLocationWithState state -> Cmd.ofMsg (BackToLocationWithState state)

    let handleCalculateExternalMsg externalMsg =
        match externalMsg with
        | CalculatePage.ExternalMsg.NoOp                  -> Cmd.none
        | CalculatePage.ExternalMsg.NavigateToNotesPhotos -> Cmd.ofMsg GoToNotesPhotos

    let handleNotesPhotosExternalMsg externalMsg =
        match externalMsg with
        | NotesPhotosPage.ExternalMsg.NoOp               -> Cmd.none
        | NotesPhotosPage.ExternalMsg.NavigateBackToHome -> Cmd.ofMsg GoToHome

    let handleHistoryExternalMsg externalMsg =
        match externalMsg with
        | HistoryPage.ExternalMsg.NoOp                               -> Cmd.none
        | HistoryPage.ExternalMsg.NavigateToNewEstimate              -> Cmd.ofMsg (GoToEstimate None)
        | HistoryPage.ExternalMsg.NavigateToExistingEstimate vehicle -> Cmd.ofMsg (GoToEstimate (Some vehicle))

    let handleConfigExternalMsg externalMsg =
        match externalMsg with
        | ConfigPage.ExternalMsg.NoOp -> Cmd.none

    let navigationMapper (model : Model) =
        //let mainModel = model.MainPageModel
        let estimateModel = model.EstimatePageModel
        let scanVinModel = model.ScanVinPageModel
        let makeListModel = model.MakeListPageModel
        let modelListModel = model.ModelListPageModel
        let yearListModel = model.YearListPageModel
        let customerModel = model.CustomerPageModel
        let locationModel = model.LocationPageModel
        let countryListModel = model.CountryListPageModel
        let stateListModel = model.StateListPageModel
        let calculateModel = model.CalculatePageModel
        let notesPhotosModel = model.NotesPhotosPageModel
        let historyModel = model.HistoryPageModel
        let configModel = model.ConfigPageModel

        match estimateModel, scanVinModel, makeListModel, modelListModel, yearListModel, customerModel,
              locationModel, countryListModel, stateListModel, calculateModel, notesPhotosModel,
              historyModel, configModel with
        | None, None, None, None, None, None, None, None, None, None, None, None, None   -> model
        | Some _, None, None, None, None, None, None, None, None, None, None, None, None -> { model with EstimatePageModel = None }
        | _, Some _, None, None, None, None, None, None, None, None, None, None, None    -> { model with ScanVinPageModel = None }
        | _, _, Some _, None, None, None, None, None, None, None, None, None, None       -> { model with MakeListPageModel = None }
        | _, _, _, Some _, None, None, None, None, None, None, None, None, None          -> { model with ModelListPageModel = None }
        | _, _, _, _, Some _, None, None, None, None, None, None, None, None             -> { model with YearListPageModel = None }
        | _, _, _, _, _, Some _, None, None, None, None, None, None, None                -> { model with CustomerPageModel = None }
        | _, _, _, _, _, _, Some _, None, None, None, None, None, None                   -> { model with LocationPageModel = None }
        | _, _, _, _, _, _, _, Some _, None, None, None, None, None                      -> { model with CountryListPageModel = None }
        | _, _, _, _, _, _, _, _, Some _, None, None, None, None                         -> { model with StateListPageModel = None }
        | _, _, _, _, _, _, _, _, _, Some _, None, None, None                            -> { model with CalculatePageModel = None }
        | _, _, _, _, _, _, _, _, _, _, Some _, None, None                               -> { model with NotesPhotosPageModel = None }
        | _, _, _, _, _, _, _, _, _, _, _, Some _, None                                  -> { model with HistoryPageModel = None }
        | _, _, _, _, _, _, _, _, _, _, _, _, Some _                                     -> { model with ConfigPageModel = None }

    let update dbPath msg model =
        match msg with

        //| LoginPageMsg msg ->
        //    let m, cmd, externalMsg = LoginPage.update msg model.LoginPageModel
        //    let cmd2 = handleLoginExternalMsg externalMsg
        //    let batchCmd = Cmd.batch [ (Cmd.map LoginPageMsg cmd); cmd2 ]
        //    { model with LoginPageModel = m }, batchCmd

        | MainPageMsg msg ->
            let m, cmd, externalMsg = MainPage.update msg model.MainPageModel
            let cmd2 = handleMainExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map MainPageMsg cmd); cmd2 ]
            { model with MainPageModel = m }, batchCmd

        | HomePageMsg msg ->
            let m, cmd, externalMsg = HomePage.update msg model.HomePageModel.Value
            let cmd2 = handleHomeExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map HomePageMsg cmd); cmd2 ]
            { model with HomePageModel = Some m }, batchCmd

        | EstimatePageMsg msg ->
            let m, cmd, externalMsg = EstimatePage.update dbPath msg model.EstimatePageModel.Value
            let cmd2 = handleEstimateExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map EstimatePageMsg cmd); cmd2 ]
            { model with EstimatePageModel = Some m }, batchCmd

        | ScanVinPageMsg msg ->
            let m, cmd, externalMsg = ScanVinPage.update msg model.ScanVinPageModel.Value
            let cmd2 = handleScanVinExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map ScanVinPageMsg cmd); cmd2 ]
            { model with ScanVinPageModel = Some m }, batchCmd

        | MakeListPageMsg msg ->
            let m, cmd, externalMsg = MakeListPage.update msg model.MakeListPageModel.Value
            let cmd2 = handleMakeListExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map MakeListPageMsg cmd); cmd2 ]
            { model with MakeListPageModel = Some m }, batchCmd

        | ModelListPageMsg msg ->
            let m, cmd, externalMsg = ModelListPage.update msg model.ModelListPageModel.Value
            let cmd2 = handleModelListExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map ModelListPageMsg cmd); cmd2 ]
            { model with ModelListPageModel = Some m }, batchCmd

        | YearListPageMsg msg ->
            let m, cmd, externalMsg = YearListPage.update msg model.YearListPageModel.Value
            let cmd2 = handleYearListExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map YearListPageMsg cmd); cmd2 ]
            { model with YearListPageModel = Some m }, batchCmd

        | CustomerPageMsg msg ->
            let m, cmd, externalMsg = CustomerPage.update msg model.CustomerPageModel.Value
            let cmd2 = handleCustomerExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map CustomerPageMsg cmd); cmd2 ]
            { model with CustomerPageModel = Some m }, batchCmd

        | LocationPageMsg msg ->
            let m, cmd, externalMsg = LocationPage.update msg model.LocationPageModel.Value
            let cmd2 = handleLocationExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map LocationPageMsg cmd); cmd2 ]
            { model with LocationPageModel = Some m }, batchCmd

        | CountryListPageMsg msg ->
            let m, cmd, externalMsg = CountryListPage.update msg model.CountryListPageModel.Value
            let cmd2 = handleCountryListExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map CountryListPageMsg cmd); cmd2 ]
            { model with CountryListPageModel = Some m }, batchCmd

        | StateListPageMsg msg ->
            let m, cmd, externalMsg = StateListPage.update msg model.StateListPageModel.Value
            let cmd2 = handleStateListExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map StateListPageMsg cmd); cmd2 ]
            { model with StateListPageModel = Some m }, batchCmd

        | CalculatePageMsg msg ->
            let m, cmd, externalMsg = CalculatePage.update msg model.CalculatePageModel.Value
            let cmd2 = handleCalculateExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map CalculatePageMsg cmd); cmd2 ]
            { model with CalculatePageModel = Some m }, batchCmd

        | NotesPhotosPageMsg msg ->
            let m, cmd, externalMsg = NotesPhotosPage.update msg model.NotesPhotosPageModel.Value
            let cmd2 = handleNotesPhotosExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map NotesPhotosPageMsg cmd); cmd2 ]
            { model with NotesPhotosPageModel = Some m }, batchCmd

        | HistoryPageMsg msg ->
            let m, cmd, externalMsg = HistoryPage.update msg model.HistoryPageModel.Value
            let cmd2 = handleHistoryExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map HistoryPageMsg cmd); cmd2 ]
            { model with HistoryPageModel = Some m }, batchCmd

        | ConfigPageMsg msg ->
            let m, cmd, externalMsg = ConfigPage.update msg model.ConfigPageModel.Value
            let cmd2 = handleConfigExternalMsg externalMsg
            let batchCmd = Cmd.batch [ (Cmd.map ConfigPageMsg cmd); cmd2 ]
            { model with ConfigPageModel = Some m }, batchCmd

        | GoToHome ->
            let m, cmd = HomePage.init()
            { model with HomePageModel = Some m }, (Cmd.map HomePageMsg cmd)

        | GoToEstimate vehicle ->
            let m, cmd = EstimatePage.init (vehicle)
            { model with EstimatePageModel = Some m }, (Cmd.map EstimatePageMsg cmd)

        | GoToScanVin ->
            let m, cmd = ScanVinPage.init()
            { model with ScanVinPageModel = Some m }, (Cmd.map ScanVinPageMsg cmd)

        | GoToMakeList ->
            let m, cmd = MakeListPage.init()
            { model with MakeListPageModel = Some m }, (Cmd.map MakeListPageMsg cmd)

        | GoToModelList ->
            let m, cmd = ModelListPage.init()
            { model with ModelListPageModel = Some m }, (Cmd.map ModelListPageMsg cmd)

        | GoToYearList ->
            let m, cmd = YearListPage.init()
            { model with YearListPageModel = Some m }, (Cmd.map YearListPageMsg cmd)

        | GoToCustomer customer ->
            let m, cmd = CustomerPage.init customer
            { model with CustomerPageModel = Some m }, (Cmd.map CustomerPageMsg cmd)

        | GoToLocation location ->
            let m, cmd = LocationPage.init (location)
            { model with LocationPageModel = Some m }, (Cmd.map LocationPageMsg cmd)

        | GoToCountryList ->
            let m, cmd = CountryListPage.init()
            { model with CountryListPageModel = Some m }, (Cmd.map CountryListPageMsg cmd)

        | GoToStateList ->
            let m, cmd = StateListPage.init()
            { model with StateListPageModel = Some m }, (Cmd.map StateListPageMsg cmd)

        | GoToCalculate ->
            let m, cmd = CalculatePage.init()
            { model with CalculatePageModel = Some m }, (Cmd.map CalculatePageMsg cmd)

        | GoToNotesPhotos ->
            let m, cmd = NotesPhotosPage.init()
            { model with NotesPhotosPageModel = Some m }, (Cmd.map NotesPhotosPageMsg cmd)

        | GoToHistory ->
            let m, cmd = HistoryPage.init()
            { model with HistoryPageModel = Some m }, (Cmd.map HistoryPageMsg cmd)

        | GoToConfig ->
            let m, cmd = ConfigPage.init()
            { model with ConfigPageModel = Some m }, (Cmd.map ConfigPageMsg cmd)

        | BackToEstimateWithMake make ->
            {
                model with MakeListPageModel = None
                           WorkaroundNavPageBug = true
                           WorkaroundNavPageBugPendingCmd = Cmd.ofMsg (EstimatePageMsg (EstimatePage.Msg.MakeUpdated make))
            }, Cmd.none

        | BackToEstimateWithModel modelv ->
            {
                model with ModelListPageModel = None
                           WorkaroundNavPageBug = true
                           WorkaroundNavPageBugPendingCmd = Cmd.ofMsg (EstimatePageMsg (EstimatePage.Msg.ModelUpdated modelv))
            }, Cmd.none

        | BackToEstimateWithYear year ->
            {
                model with YearListPageModel = None
                           WorkaroundNavPageBug = true
                           WorkaroundNavPageBugPendingCmd = Cmd.ofMsg (EstimatePageMsg (EstimatePage.Msg.YearUpdated year))
            }, Cmd.none

        | BackToEstimateWithCustomer customer ->
            {
                model with CustomerPageModel = None
                           WorkaroundNavPageBug = true
                           WorkaroundNavPageBugPendingCmd = Cmd.ofMsg (EstimatePageMsg (EstimatePage.Msg.CustomerUpdated customer))
            }, Cmd.none

        | BackToEstimateWithLocation location ->
            {
                model with LocationPageModel = None
                           WorkaroundNavPageBug = true
                           WorkaroundNavPageBugPendingCmd = Cmd.ofMsg (EstimatePageMsg (EstimatePage.Msg.LocationUpdated location))
            }, Cmd.none

        | BackToLocationWithCountry country ->
            {
                model with CountryListPageModel = None
                           WorkaroundNavPageBug = true
                           WorkaroundNavPageBugPendingCmd = Cmd.ofMsg (LocationPageMsg (LocationPage.Msg.CountryUpdated country))
            }, Cmd.none

        | BackToLocationWithState state ->
            {
                model with StateListPageModel = None
                           WorkaroundNavPageBug = true
                           WorkaroundNavPageBugPendingCmd = Cmd.ofMsg (LocationPageMsg (LocationPage.Msg.StateUpdated state))
            }, Cmd.none

        | BackToMain ->
            {
                model with NotesPhotosPageModel = None
                           WorkaroundNavPageBug = true
                           WorkaroundNavPageBugPendingCmd = Cmd.ofMsg (MainPageMsg (MainPage.Msg.VehicleUpdated))
            }, Cmd.none


        | NavigationPopped ->
            match model.WorkaroundNavPageBug with
            | true ->
            let newModel =
                { model with
                    WorkaroundNavPageBug = false
                    WorkaroundNavPageBugPendingCmd = Cmd.none }
            newModel, model.WorkaroundNavPageBugPendingCmd
            | false ->
                navigationMapper model, Cmd.none

    let getPages allPages =
        //let loginPage = allPages.LoginPage
        let mainPage = allPages.MainPage
        let estimatePage = allPages.EstimatePage
        let scanVinPage = allPages.ScanVinPage
        let makeListPage = allPages.MakeListPage
        let modelListPage = allPages.ModelListPage
        let yearListPage = allPages.YearListPage
        let customerPage = allPages.CustomerPage
        let locationPage = allPages.LocationPage
        let countryListPage = allPages.CountryListPage
        let stateListPage = allPages.StateListPage
        let calculatePage = allPages.CalculatePage
        let notesPhotosPage = allPages.NotesPhotosPage
        let historyPage = allPages.HistoryPage
        let configPage = allPages.ConfigPage

        match estimatePage, scanVinPage, makeListPage, modelListPage, yearListPage, customerPage,
              locationPage, countryListPage, stateListPage, calculatePage, notesPhotosPage,
              historyPage, configPage with
        | None, None, None, None, None, None, None, None, None, None, None, None, None                  -> [ mainPage ]
        | Some estimate, None, None, None, None, None, None, None, None, None, None, None, None         -> [ mainPage; estimate ]
        | Some estimate, Some scanVin, None, None, None, None, None, None, None, None, None, None, None -> [ mainPage; estimate; scanVin ]
        | _,  Some scanVin, None, None, None, None, None, None, None, None, None, None, None            -> [ mainPage; scanVin ]
        | Some estimate, _, Some makeList, None, None, None, None, None, None, None, None, None, None   -> [ mainPage; estimate; makeList ]
        | _, _, Some makeList, None, None, None, None, None, None, None, None, None, None               -> [ mainPage; makeList ]
        | Some estimate, _, _, Some modelList, None, None, None, None, None, None, None, None, None     -> [ mainPage; estimate; modelList ]
        | _, _, _, Some modelList, None, None, None, None, None, None, None, None, None                 -> [ mainPage; modelList ]
        | Some estimate, _, _, _, Some yearList, None, None, None, None, None, None, None, None         -> [ mainPage; estimate; yearList ]
        | _, _, _, _, Some yearList, None, None, None, None, None, None, None, None                     -> [ mainPage; yearList ]
        | Some estimate, _, _, _, _, Some customer, None, None, None, None, None, None, None            -> [ mainPage; estimate; customer ]
        | _, _, _, _, _, Some customer, None, None, None, None, None, None, None                        -> [ mainPage; customer ]
        | Some estimate, _, _, _, _, _, Some location, None, None, None, None, None, None               -> [ mainPage; estimate; location ]
        | _, _, _, _, _, _, Some location, None, None, None, None, None, None                           -> [ mainPage; location ]
        | Some estimate, _, _, _, _, _, Some location, Some countryList, None, None, None, None, None   -> [ mainPage; estimate; location; countryList ]
        | _, _, _, _, _, _, _, Some countryList, None, None, None, None, None                           -> [ mainPage; countryList ]
        | Some estimate, _, _, _, _, _, Some location, _, Some stateList, None, None, None, None        -> [ mainPage; estimate; location; stateList ]
        | _, _, _, _, _, _, _, _, Some stateList, None, None, None, None                                -> [ mainPage; stateList ]
        | Some estimate, _, _, _, _, _, _, _, _, Some calculate, None, None, None                       -> [ mainPage; estimate; calculate ]
        | _, _, _, _, _, _, _, _, _, Some calculate, None, None, None                                   -> [ mainPage; calculate ]
        | Some estimate, _, _, _, _, _, _, _, _, Some calculate, Some notesPhotos, None, None           -> [ mainPage; estimate; calculate; notesPhotos ]
        | _, _, _, _, _, _, _, _, _, _, Some notesPhotos, None, None                                    -> [ mainPage; notesPhotos ]
        | _, _, _, _, _, _, _, _, _, _, _, Some history, None                                           -> [ mainPage; history ]
        | _, _, _, _, _, _, _, _, _, _, _, _, Some config                                               -> [ mainPage; config ]

    let view (model: Model) dispatch =

        //let loginPage =
        //    LoginPage.view model.LoginPageModel (LoginPageMsg >> dispatch)

        let mainPage =
            MainPage.view model.MainPageModel (MainPageMsg >> dispatch)

        let homePage =
            model.HomePageModel
            |> Option.map (fun hModel -> HomePage.view hModel (HomePageMsg >> dispatch))

        let estimatePage =
            model.EstimatePageModel
            |> Option.map (fun eModel -> EstimatePage.view eModel (EstimatePageMsg >> dispatch))

        let scanVinPage =
            model.ScanVinPageModel
            |> Option.map (fun sModel -> ScanVinPage.view sModel (ScanVinPageMsg >> dispatch))

        let makeListPage =
            model.MakeListPageModel
            |> Option.map (fun mModel -> MakeListPage.view mModel (MakeListPageMsg >> dispatch))

        let modelListPage =
            model.ModelListPageModel
            |> Option.map (fun mModel -> ModelListPage.view mModel (ModelListPageMsg >> dispatch))

        let yearListPage =
            model.YearListPageModel
            |> Option.map (fun yModel -> YearListPage.view yModel (YearListPageMsg >> dispatch))

        let customerPage =
            model.CustomerPageModel
            |> Option.map (fun cModel -> CustomerPage.view cModel (CustomerPageMsg >> dispatch))

        let locationPage =
            model.LocationPageModel
            |> Option.map (fun lModel -> LocationPage.view lModel (LocationPageMsg >> dispatch))

        let countryListPage =
            model.CountryListPageModel
            |> Option.map (fun cModel -> CountryListPage.view cModel (CountryListPageMsg >> dispatch))

        let stateListPage =
            model.StateListPageModel
            |> Option.map (fun sModel -> StateListPage.view sModel (StateListPageMsg >> dispatch))

        let calculatePage =
            model.CalculatePageModel
            |> Option.map (fun cModel -> CalculatePage.view cModel (CalculatePageMsg >> dispatch))

        let notesPhotosPage =
            model.NotesPhotosPageModel
            |> Option.map (fun nModel -> NotesPhotosPage.view nModel (NotesPhotosPageMsg >> dispatch))

        let historyPage =
            model.HistoryPageModel
            |> Option.map (fun hModel -> HistoryPage.view hModel (HistoryPageMsg >> dispatch))

        let configPage =
            model.ConfigPageModel
            |> Option.map (fun cModel -> ConfigPage.view cModel (ConfigPageMsg >> dispatch))

        let allPages =
            {
                //LoginPage = loginPage
                MainPage = mainPage
                HomePage = homePage
                EstimatePage = estimatePage
                ScanVinPage = scanVinPage
                MakeListPage = makeListPage
                ModelListPage = modelListPage
                YearListPage = yearListPage
                CustomerPage = customerPage
                LocationPage = locationPage
                CountryListPage = countryListPage
                StateListPage = stateListPage
                CalculatePage = calculatePage
                NotesPhotosPage = notesPhotosPage
                HistoryPage = historyPage
                ConfigPage = configPage
            }

        View.NavigationPage(
            barTextColor = Style.whiteColor,
            barBackgroundColor = Style.blackColor,
            popped = (fun _ -> dispatch NavigationPopped),
            pages = getPages allPages
        )

type App(dbPath) as app = 
    inherit Application ()

    let init = App.init dbPath
    let update = App.update dbPath
    let view = App.view
    
    let runner = 
        Program.mkProgram init update view
        |> Program.withConsoleTrace
        |> XamarinFormsProgram.run app