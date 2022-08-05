namespace AutoCaptureMobile.Infrastructure

open System
open System.IO

module Helpers = 

    let getDbPath() =
        let docFolder = Environment.GetFolderPath(Environment.SpecialFolder.Personal)
        let libFolder = Path.Combine(docFolder, "..", "Library", "Databases")

        if not (Directory.Exists libFolder) then
            Directory.CreateDirectory(libFolder) |> ignore
        else
            ()

        Path.Combine(libFolder, "ACM.db3")