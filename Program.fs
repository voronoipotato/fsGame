// Learn more about F# at http://fsharp.org

open System
open Veldrid.StartupUtilities
open System.Numerics
open Veldrid
open Veldrid
open Veldrid
open Veldrid
open Veldrid
open Veldrid
open Veldrid
open Veldrid

module Game = 
    [<Struct>]
    type VertexPositionColor = 
        {Position: Vector2; Color: RgbaFloat}
        static member VertexPositionColor(position, color) = {Position = position; Color = color}

    let createWindow title = 
        let mutable wci = WindowCreateInfo()
        wci.X <- 100
        wci.Y <- 100
        wci.WindowWidth <- 960
        wci.WindowHeight <- 540
        wci.WindowTitle <- title
        wci

    let createResources window = 
        let graphicsDevice = VeldridStartup.CreateGraphicsDevice(window)
        let factory = graphicsDevice.ResourceFactory
        let quadVerticies' = [|
            Vector2(-0.75f,0.75f), RgbaFloat.Red
            Vector2(0.75f, 0.75f), RgbaFloat.Green 
            Vector2(-0.75f,-0.75f), RgbaFloat.Blue
            Vector2(0.75f,-0.75f), RgbaFloat.Yellow 
            |]
        let quadVerticies = quadVerticies' |> Array.map VertexPositionColor.VertexPositionColor
        let quadIndicies : uint16[] = [|0us;1us;2us;3us|]
        let quadVerticiesSize : uint32 = sizeof<VertexPositionColor> * quadVerticies.Length |> uint32
        let vertexBuffer = factory.CreateBuffer(BufferDescription(quadVerticiesSize,BufferUsage.VertexBuffer))
        let indiciesSize = 4u * uint32 sizeof<uint16>
        let indexBuffer = factory.CreateBuffer(BufferDescription(indiciesSize, BufferUsage.IndexBuffer))
        graphicsDevice.UpdateBuffer (vertexBuffer, 0u , quadVerticies) |> ignore
        graphicsDevice.UpdateBuffer (indexBuffer, 0u, quadIndicies ) |> ignore
        ()
        
        

[<EntryPoint>]
let main argv =
    
    let wci = Game.createWindow "Suits"
    
    let window = VeldridStartup.CreateWindow(ref wci)
    Game.createResources window |> ignore
    while window.Exists do
        window.PumpEvents() |> ignore
    0 // return an integer exit code
