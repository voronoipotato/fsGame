// Learn more about F# at http://fsharp.org

open System
open System.Runtime.CompilerServices
open Veldrid.StartupUtilities
open System.Numerics
open Veldrid
open Veldrid.SPIRV
open FShade
open System.Text
open Aardvark.Base

open ShaderTools

module Shape =
    let rectangle position width height (color: RgbaFloat) =
        let dimensions = Vector2(width, height) / Vector2(2.f)
        let format =
            [|
                Vector2(1.f, -1.f)
                Vector2(1.f,  1.f)
                Vector2(-1.f, 1.f)

                Vector2(-1.f, 1.f)
                Vector2(-1.f,-1.f)
                Vector2(1.f, -1.f)
            |]
        format
        |> Array.map (fun v -> (v * dimensions + position,color))
    
    let square position size color = rectangle position size size color
    
    let prism position l w h = 
        let dimensions = Vector3(l, w, h) / Vector3(2.f)
        [| 
            Vector3(-1.0f,-1.0f,-1.0f) // triangle 1 : begin
            Vector3(-1.0f,-1.0f, 1.0f)
            Vector3(-1.0f, 1.0f, 1.0f) // triangle 1 : end

            Vector3(1.0f, 1.0f,-1.0f) // triangle 2 : begin
            Vector3(-1.0f,-1.0f,-1.0f)
            Vector3(-1.0f, 1.0f,-1.0f) // triangle 2 : end

            Vector3(1.0f,-1.0f, 1.0f)
            Vector3(-1.0f,-1.0f,-1.0f)
            Vector3(1.0f,-1.0f,-1.0f)

            Vector3(1.0f, 1.0f,-1.0f)
            Vector3(1.0f,-1.0f,-1.0f)
            Vector3(-1.0f,-1.0f,-1.0f)

            Vector3(-1.0f,-1.0f,-1.0f)
            Vector3(-1.0f, 1.0f, 1.0f)
            Vector3(-1.0f, 1.0f,-1.0f)

            Vector3(1.0f,-1.0f, 1.0f)
            Vector3(-1.0f,-1.0f, 1.0f)
            Vector3(-1.0f,-1.0f,-1.0f)

            Vector3(-1.0f, 1.0f, 1.0f)
            Vector3(-1.0f,-1.0f, 1.0f)
            Vector3(1.0f,-1.0f, 1.0f)

            Vector3(1.0f, 1.0f, 1.0f)
            Vector3(1.0f,-1.0f,-1.0f)
            Vector3(1.0f, 1.0f,-1.0f)

            Vector3(1.0f,-1.0f,-1.0f)
            Vector3(1.0f, 1.0f, 1.0f)
            Vector3(1.0f,-1.0f, 1.0f)

            Vector3(1.0f, 1.0f, 1.0f)
            Vector3(1.0f, 1.0f,-1.0f)
            Vector3(-1.0f, 1.0f,-1.0f)

            Vector3(1.0f, 1.0f, 1.0f)
            Vector3(-1.0f, 1.0f,-1.0f)
            Vector3(-1.0f, 1.0f, 1.0f)

            Vector3(1.0f, 1.0f, 1.0f)
            Vector3(-1.0f, 1.0f, 1.0f)
            Vector3(1.0f,-1.0f, 1.0f)
        |] |> Array.map (fun v -> v * dimensions + position)

module VeldridTools = 
    let inline getSize (t: 't[]) : uint32 = sizeof<'t> * t.Length |> uint32
    let createWindow title = 
        let mutable wci = WindowCreateInfo()
        wci.X <- 100
        wci.Y <- 100
        wci.WindowWidth  <- 400
        wci.WindowHeight <- 400
        wci.WindowTitle  <- title
        wci
    let toIndex a =  a |> Array.mapi (fun i _ -> uint16(i) )
    let createBuffers (graphicsDevice: GraphicsDevice) (factory: ResourceFactory) quadVerticies =
            let createBuffer (bufferDescription:BufferDescription) = factory.CreateBuffer(bufferDescription)
            let quadIndicies : uint16[] = quadVerticies |> toIndex
            let vertexBuffer = BufferDescription(getSize quadVerticies, BufferUsage.VertexBuffer) |> createBuffer
            let indexBuffer = BufferDescription(getSize quadIndicies, BufferUsage.IndexBuffer) |> createBuffer
            do graphicsDevice.UpdateBuffer (vertexBuffer, 0u, quadVerticies)
            do graphicsDevice.UpdateBuffer (indexBuffer,  0u, quadIndicies)
            (vertexBuffer, indexBuffer)

    let updateBuffers (graphicsDevice: GraphicsDevice) vertexBuffer indexBuffer quadVerticies = 
        let quadIndicies : uint16[] = quadVerticies |> toIndex
        do graphicsDevice.UpdateBuffer(vertexBuffer, 0u, quadVerticies)
        do graphicsDevice.UpdateBuffer(indexBuffer, 0u, quadIndicies)

    let getPipelineDescription vertexLayout shaders (graphicsDevice: GraphicsDevice) =
            let mutable pd = GraphicsPipelineDescription()
            pd.BlendState <- BlendStateDescription.SingleOverrideBlend
            pd.DepthStencilState <- DepthStencilStateDescription(
                depthTestEnabled= true,
                depthWriteEnabled= true,
                comparisonKind= ComparisonKind.LessEqual)            
            pd.RasterizerState <- RasterizerStateDescription(
                cullMode= FaceCullMode.Back,
                fillMode= PolygonFillMode.Solid, 
                frontFace= FrontFace.Clockwise,
                depthClipEnabled= true,
                scissorTestEnabled= false)
            pd.PrimitiveTopology <- PrimitiveTopology.TriangleList
            pd.ResourceLayouts <- Array.empty<ResourceLayout>
            pd.ShaderSet <- ShaderSetDescription(
                vertexLayouts =  [|vertexLayout|] ,
                shaders = shaders )
            
            pd.Outputs <- graphicsDevice.SwapchainFramebuffer.OutputDescription
            pd

module Game = 
    open VeldridTools

    type Frag = {[<Color>] Color: V4d; [<Position>] Position: V4d; [<FragCoord>] FragCoord: V4d;}

    [<Struct>]
    type Vertex = 
        {
            Color: RgbaFloat
            Position: Vector4
        }
    module Vertex= 
        let from2D (position: Vector2, color) = {Position = Vector4(position, 0.f ,1.f); Color = color}
        let from3D (position: Vector3, color) = {Position = Vector4(position, 1.f); Color = color}

    //TODO: Replace with model loading
    let createScene tick = 
        let t = float32 (tick ) / 2000.f

        let cube = 
            let scale (p: Vector3) = p * 0.2f
            //TODO: move to the world matrix
            let rotate (p: Vector3) = 
                let transform (q: Quaternion) (p: Vector3)  = Vector3.Transform(p, q)
                p 
                |> transform (Quaternion(0.f, cos(t),0.f,sin(t)))
                |> transform (Quaternion(0.f,0.f,cos(t),sin(t)))
            let paint (p: Vector3) = 
                (p,  RgbaFloat.DarkRed)
            let p1 = (Shape.prism (Vector3(0.f,0.f,0.f)) 1.f 2.f 1.f)
        
            [| yield! p1; |]|> Array.map ( scale >> rotate >> paint )
        
        [| yield! cube |]
        |> Array.map Vertex.from3D

    let createResources window = 
        let graphicsDevice = VeldridStartup.CreateGraphicsDevice(window)
        let factory = graphicsDevice.ResourceFactory

        let (vertexBuffer, indexBuffer) = 
            let quadVerticies = createScene (Environment.TickCount64)
            createBuffers graphicsDevice factory quadVerticies

        let vertexLayout = 
            let makeVED s = VertexElementDescription(s, VertexElementSemantic.TextureCoordinate, VertexElementFormat.Float4)
            let color = makeVED "Colors"
            let position = makeVED "Positions"
            VertexLayoutDescription(color, position)
   
        let fragmentShader (v: Frag)  = 
            fragment {
                return v
            }
        
        //fshade prepends vertex and fragment ifdef allowing you to use the same code for both

        let shaders =  
            let shaderCode = 
                fragmentShader
                |> Effect.ofFunction 
                |> Tool.print []
            let vertexCode = shaderCode.Replace("#version 450","#version 450\n#define Vertex")
            let fragmentCode = shaderCode.Replace("#version 450","#version 450\n#define Fragment")
            let getShaderDesc (s: ShaderStages) (c: string) = ShaderDescription(s, Encoding.UTF8.GetBytes(c), "main")
            let vertexShaderDesc = getShaderDesc ShaderStages.Vertex vertexCode
            let fragmentShaderDesc = getShaderDesc ShaderStages.Fragment fragmentCode
            factory.CreateFromSpirv(vertexShaderDesc, fragmentShaderDesc)

        let pipelineDescription = getPipelineDescription vertexLayout shaders graphicsDevice

        let pipeline = factory.CreateGraphicsPipeline pipelineDescription
        let commands = factory.CreateCommandList()
        commands, graphicsDevice, pipeline, shaders, vertexBuffer, indexBuffer

    let Draw (commands: CommandList, graphicsDevice: GraphicsDevice, pipeline:Pipeline, shaders, vertexBuffer, indexBuffer) = 
        let quadVerticies = (createScene Environment.TickCount64)
        let indexCount = uint32 quadVerticies.Length
        
        do updateBuffers graphicsDevice vertexBuffer indexBuffer quadVerticies
        
        commands.Begin()
        commands.SetFramebuffer graphicsDevice.SwapchainFramebuffer
        commands.ClearColorTarget (0u, RgbaFloat.Black)
        commands.SetVertexBuffer (0u, vertexBuffer)
        commands.SetIndexBuffer (indexBuffer, IndexFormat.UInt16)
        commands.SetPipeline (pipeline)
        commands.DrawIndexed(
            indexCount    = indexCount,
            instanceCount = 1u,
            indexStart    = 0u,
            vertexOffset  = 0,
            instanceStart = 0u)
        commands.End()
        graphicsDevice.SubmitCommands(commands)
        graphicsDevice.SwapBuffers()
        ()

[<EntryPoint>]
let main argv =
    let wci = VeldridTools.createWindow "veldridGame"
    let window = VeldridStartup.CreateWindow(ref wci)
    let props = Game.createResources window
    while window.Exists do
        window.PumpEvents() |> ignore
        do Game.Draw props
    let (commands, graphicsDevice , pipeline, shaders, vertexBuffer, indexBuffer) = props    
    let dispose () =
        commands.Dispose()
        graphicsDevice.Dispose()
        vertexBuffer.Dispose()
        indexBuffer.Dispose()
        pipeline.Dispose()
        shaders |> Array.iter(fun x -> x.Dispose())
        
    do dispose()
    0 // return an integer exit code
