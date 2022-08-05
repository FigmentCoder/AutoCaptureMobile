// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace AutoCaptureMobile.iOS

open UIKit
open Foundation
open Xamarin.Forms
open Xamarin.Forms.Platform.iOS

//open System
//open System.IO

open AutoCaptureMobile.Infrastructure

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit FormsApplicationDelegate ()

    override this.FinishedLaunching (app, options) =
        Forms.Init()

        let dbPath = Helpers.getDbPath()
        //let appcore = new AutoCaptureMobile.Presentation.App()
        let appcore = new AutoCaptureMobile.Presentation.App(dbPath)

        this.LoadApplication (appcore)
        base.FinishedLaunching(app, options)

module Main =
    [<EntryPoint>]
    let main args =
        UIApplication.Main(args, null, "AppDelegate")
        0