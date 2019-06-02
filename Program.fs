// Learn more about F# at http://fsharp.org

open System
open Veldrid.StartupUtilities

module Game = 
    let createWindow title = 
        let mutable wci = WindowCreateInfo()
        wci.X <- 100
        wci.Y <- 100
        wci.WindowWidth <- 960
        wci.WindowHeight <- 540
        wci.WindowTitle <- title
        wci

[<EntryPoint>]
let main argv =
    
    let wci = Game.createWindow "Suits"
    
    let window = VeldridStartup.CreateWindow(ref wci)
    let graphicsDevice = VeldridStartup.CreateGraphicsDevice(window)
    while window.Exists do
        window.PumpEvents() |> ignore
    0 // return an integer exit code
