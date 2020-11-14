﻿// Learn more about F# at http://fsharp.org

open System
open Veldrid.StartupUtilities
open System.Numerics
open Veldrid
open Veldrid.SPIRV
open System.Text

module Shape =
    let rectangle position width height (color: RgbaFloat) =
        let dimensions = Vector2(width, height) / Vector2(2.f)
        let format =
            [
                (-1.f, 1.f)
                (1.f,  1.f)
                (-1.f,-1.f)
                (1.f, -1.f)
            ] |> Array.ofList
        
        Array.map ((fun (x,y) -> Vector2(x,y)) 
                    >> ((*) dimensions ) 
                    >> ((+) position) 
                    >> (fun v-> (v,color))) format
    let square position size color = rectangle position size size color

module Game = 
    [<Struct>]
    type Vertex = 
        {Position: Vector2; Color: RgbaFloat}
    type Buffers =
        { Projection: DeviceBuffer
          View: DeviceBuffer
          World: DeviceBuffer
          Vertex:DeviceBuffer
          Index: DeviceBuffer }
    module Vertex= 
        let create (position, color) = {Position = position; Color = color}
    let createWindow title = 
        let mutable wci = WindowCreateInfo()
        wci.X            <- 100
        wci.Y            <- 100
        wci.WindowWidth  <- 960
        wci.WindowHeight <- 960
        wci.WindowTitle  <- title
        wci

    let inline getSize (t: 't[]) : uint32 = sizeof<'t> * t.Length |> uint32

    let createResources window = 
        let graphicsDevice = VeldridStartup.CreateGraphicsDevice(window)
        let factory = graphicsDevice.ResourceFactory

        let createBuffers quadVerticies =
            let createBuffer (bufferDescription:BufferDescription) = factory.CreateBuffer(bufferDescription)

            let quadVerticies = quadVerticies |> Array.map Vertex.create
            let quadIndicies : uint16[] = quadVerticies |> Array.mapi (fun i _ -> uint16(i) )
            let vertexBuffer = BufferDescription( getSize quadVerticies, BufferUsage.VertexBuffer) |> createBuffer
            let indexBuffer = BufferDescription(getSize quadIndicies, BufferUsage.IndexBuffer) |> createBuffer
            do graphicsDevice.UpdateBuffer (vertexBuffer, 0u, quadVerticies)
            do graphicsDevice.UpdateBuffer (indexBuffer,  0u, quadIndicies)
            {   Projection = vertexBuffer
                View = vertexBuffer
                World = vertexBuffer
                Vertex = vertexBuffer
                Index = indexBuffer} 

        let grayRect = Shape.rectangle (Vector2(0.0f)) 1.75f 0.85f RgbaFloat.LightGrey
        let blueRect = Shape.square (Vector2(0.0f)) 0.5f RgbaFloat.CornflowerBlue
        let redRect = Shape.square (Vector2(0.125f)) 0.5f RgbaFloat.DarkRed
        let yellowRect = Shape.square (Vector2(0.25f)) 0.5f RgbaFloat.Yellow

        let quadVerticies: (Vector2 * RgbaFloat) [] = [|
            yield! grayRect
            yield! blueRect
            yield! redRect
            yield! yellowRect
            |]

        let buffers = createBuffers quadVerticies
        let  {Vertex = vertexBuffer ; Index=indexBuffer} = buffers
        let vertexLayout = 
            let position = VertexElementDescription("Position", VertexElementSemantic.TextureCoordinate, VertexElementFormat.Float2)
            let color = VertexElementDescription("Color", VertexElementSemantic.TextureCoordinate, VertexElementFormat.Float4)
            VertexLayoutDescription(position, color)

        let getBytes (s : string) = Encoding.UTF8.GetBytes s

        let createShaderDesc (stage:ShaderStages) (shader:string) = 
            let shaderCode = IO.File.ReadAllText(shader)
            ShaderDescription(stage, getBytes(shaderCode), "main")
        let vertexShaderDesc = createShaderDesc ShaderStages.Vertex "vertexShader.glsl"
        let fragmentShaderDesc = createShaderDesc ShaderStages.Fragment "fragmentShader.glsl"
        let shaders = factory.CreateFromSpirv(vertexShaderDesc, fragmentShaderDesc)

        let pipelineDescription =
            let mutable pipelineDescription = GraphicsPipelineDescription()
            pipelineDescription.BlendState <- BlendStateDescription.SingleOverrideBlend
            pipelineDescription.DepthStencilState <- DepthStencilStateDescription(
                depthTestEnabled= true,
                depthWriteEnabled= true,
                comparisonKind= ComparisonKind.LessEqual)
            pipelineDescription.RasterizerState <- RasterizerStateDescription(
                cullMode= FaceCullMode.Back,
                fillMode= PolygonFillMode.Solid,
                frontFace= FrontFace.Clockwise,
                depthClipEnabled= true,
                scissorTestEnabled= false)
            pipelineDescription.PrimitiveTopology <- PrimitiveTopology.TriangleStrip
            pipelineDescription.ResourceLayouts <- Array.empty<ResourceLayout>
            pipelineDescription.ShaderSet <- ShaderSetDescription(
                vertexLayouts =  [|vertexLayout|] ,
                shaders = shaders )
            pipelineDescription.Outputs <- graphicsDevice.SwapchainFramebuffer.OutputDescription
            pipelineDescription

        let pipeline = factory.CreateGraphicsPipeline pipelineDescription
        let commandList = factory.CreateCommandList()
        commandList, graphicsDevice, buffers, pipeline, shaders, uint32 quadVerticies.Length

    let Draw (commandList: CommandList, graphicsDevice: GraphicsDevice, buffers, pipeline:Pipeline, shaders, indexCount) = 
        commandList.Begin()
        commandList.SetFramebuffer graphicsDevice.SwapchainFramebuffer
        commandList.ClearColorTarget (0u, RgbaFloat.Black)
        commandList.SetVertexBuffer (0u, buffers.Vertex)
        commandList.SetIndexBuffer (buffers.Index, IndexFormat.UInt16)
        commandList.SetPipeline (pipeline)
        commandList.DrawIndexed(
            indexCount    = indexCount,
            instanceCount = 1u,
            indexStart    = 0u,
            vertexOffset  = 0,
            instanceStart = 0u)
        commandList.End()
        graphicsDevice.SubmitCommands(commandList)
        graphicsDevice.SwapBuffers()
        ()

[<EntryPoint>]
let main argv =
    let wci = Game.createWindow "veldridGame"
    let window = VeldridStartup.CreateWindow(ref wci)
    let props = Game.createResources window
    while window.Exists do
        window.PumpEvents() |> ignore
        do Game.Draw props
    let (commandList, graphicsDevice , buffers, pipeline, shaders, _) = props    
    let dispose () =
        commandList.Dispose()
        graphicsDevice.Dispose()
        buffers.Vertex.Dispose()
        buffers.Index.Dispose()
        pipeline.Dispose()
        shaders |> Array.iter(fun x -> x.Dispose())
        
    do dispose()
    0 // return an integer exit code
