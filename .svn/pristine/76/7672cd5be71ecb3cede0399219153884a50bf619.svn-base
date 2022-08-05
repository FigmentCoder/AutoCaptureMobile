namespace AutoCaptureMobile.Presentation

open Fabulous
open Fabulous.XamarinForms

open Xamarin.Forms

open AutoCaptureMobile.Domain

module MainPage =

    type Msg =
        | HistoryPageMsg of HistoryPage.Msg

    type ExternalMsg =
        | NoOp
        | NavigateToNewEstimate
        | NavigateToExistingEstimate of Vehicle option

    type Model =
        {
            HistoryPageModel: HistoryPage.Model
        }

    let init() =
        let (modelHistory, msgHistory) = HistoryPage.init()

        let m =
            {
                HistoryPageModel = modelHistory
            }

        let batchCmd = Cmd.batch [
            Cmd.map HistoryPageMsg msgHistory
        ]

        m, batchCmd

    let update msg model =

        let goToHistoryPage msg mapMsgFunc model =
            let m, cmd, externalMsg = HistoryPage.update msg model
            let cmd2, externalMsg2 =
                match externalMsg with
                | HistoryPage.ExternalMsg.NoOp                               -> Cmd.none, ExternalMsg.NoOp
                | HistoryPage.ExternalMsg.NavigateToNewEstimate              -> Cmd.none, ExternalMsg.NavigateToNewEstimate
                | HistoryPage.ExternalMsg.NavigateToExistingEstimate vehicle -> Cmd.none, ExternalMsg.NavigateToExistingEstimate (Some vehicle)

            m, Cmd.batch [ Cmd.map mapMsgFunc cmd; cmd2 ], externalMsg2

        match msg with

        | HistoryPageMsg msg ->
            let m, cmd, externalMsg = goToHistoryPage msg HistoryPageMsg model.HistoryPageModel
            { model with HistoryPageModel = m }, cmd, externalMsg

    let view model dispatch =

        let goToHistoryPage = HistoryPageMsg >> dispatch

        let historyPage = HistoryPage.view model.HistoryPageModel goToHistoryPage
        
        View.TabbedPage(
            children = [
                historyPage
            ]
        ).BarBackgroundColor(Color.Black).BarTextColor(Color.White)