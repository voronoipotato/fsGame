// Learn more about F# at http://fsharp.org

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
                    >> (fun v-> (Vector3(v,1.f),color))) format
    let square position size color = rectangle position size size color

    let prism position l w h = 
        let dimensions = Vector3(l, w, h)
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

module Game = 
    [<Struct>]
    type Vertex = 
        { Position: Vector4; Color: RgbaFloat;}
    
    type Buffers =
        { Projection: DeviceBuffer
          View: DeviceBuffer
          World: DeviceBuffer
          Vertex:DeviceBuffer
          Index: DeviceBuffer }
    module Vertex= 
        let create (position: Vector3, color) = {Position = Vector4(position, 1.f); Color = color; }
    let createWindow title = 
        let mutable wci = WindowCreateInfo()
        wci.X            <- 100
        wci.Y            <- 100
        wci.WindowWidth  <- 400
        wci.WindowHeight <- 400
        wci.WindowTitle  <- title
        wci

    let inline getSize (t: 't[]) : uint32 = sizeof<'t> * t.Length |> uint32
    let createScene tick = 
        let t = float32 (tick ) / 2000.f
        let scale (p: Vector3) = p * 0.2f
        let paint  (c:RgbaFloat)(p: Vector3) = 
                (p,  c)
        let cube = 
            
            //TODO: move to the world matrix
            let rotate (p: Vector3) = 
                let transform (q: Quaternion) (p: Vector3)  = Vector3.Transform(p, q)
                p 
                |> transform (Quaternion(0.f, cos(t),0.f,sin(t)))
                |> transform (Quaternion(0.f,0.f,cos(t),sin(t)))
            
            let p1 = (Shape.prism (Vector3(0.f,0.f,0.f)) 1.f 2.f 1.f)
        
            p1 |> Array.map ( scale >> rotate >> paint RgbaFloat.DarkRed)
        
        [| yield! cube 
           yield! Shape.prism (Vector3(3.f,0.f,0.f)) 1.f 2.f 1.f |> Array.map (scale >> paint RgbaFloat.Grey)
            |]
        |> Array.map Vertex.create
    let toIndex a =  a |> Array.mapi (fun i _ -> uint16(i) )
    let updateBuffers  (graphicsDevice: GraphicsDevice) buffers quadVerticies = 
        let quadIndicies : uint16[] = quadVerticies |> toIndex
        do graphicsDevice.UpdateBuffer(buffers.Vertex, 0u, quadVerticies)
        do graphicsDevice.UpdateBuffer(buffers.Index, 0u, quadIndicies)
    let createResources window = 
        let graphicsDevice = VeldridStartup.CreateGraphicsDevice(window)
        let factory = graphicsDevice.ResourceFactory

        let createBuffers quadVerticies =
            let createBuffer (bufferDescription:BufferDescription) = factory.CreateBuffer(bufferDescription)

            
            let quadIndicies : uint16[] = quadVerticies |> Array.mapi (fun i _ -> uint16(i) )
            let vertexBuffer = BufferDescription( getSize quadVerticies, BufferUsage.VertexBuffer) |> createBuffer
            let indexBuffer = BufferDescription(getSize quadIndicies, BufferUsage.IndexBuffer) |> createBuffer
            let worldBuffer = BufferDescription(64u,BufferUsage.UniformBuffer) |> createBuffer
            let viewBuffer = BufferDescription(64u,BufferUsage.UniformBuffer) |> createBuffer
            let projectionBuffer = BufferDescription(64u,BufferUsage.UniformBuffer) |> createBuffer
            do graphicsDevice.UpdateBuffer (vertexBuffer, 0u, quadVerticies)
            do graphicsDevice.UpdateBuffer (indexBuffer,  0u, quadIndicies)
            {   World = worldBuffer      // Arrange the objects (or models, or avatar) in the world (Model Transformation or World transformation).
                View = viewBuffer       // Position and orientation the camera (View transformation).
                Projection = projectionBuffer // Select a camera lens (wide angle, normal or telescopic), adjust the focus length and zoom factor to set the camera's field of view (Projection transformation).
                Vertex = vertexBuffer     // The verticies that you wish to render
                Index = indexBuffer}      // The order in which verticies are rendered
        
   
        let quadVerticies = createScene Environment.TickCount64
        let buffers = createBuffers quadVerticies
        let  {Vertex = vertexBuffer ; Index=indexBuffer} = buffers
        let vertexLayout = 
            let position = VertexElementDescription("Position", VertexElementSemantic.TextureCoordinate, VertexElementFormat.Float4)
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
            let mutable pd = GraphicsPipelineDescription()
            pd.BlendState <- BlendStateDescription.SingleOverrideBlend
            pd.DepthStencilState <- DepthStencilStateDescription(
                depthTestEnabled= true,
                depthWriteEnabled= true,
                comparisonKind= ComparisonKind.LessEqual)
            pd.RasterizerState <- RasterizerStateDescription(
                cullMode= FaceCullMode.None,
                fillMode= PolygonFillMode.Wireframe,
                frontFace= FrontFace.Clockwise,
                depthClipEnabled= false,
                scissorTestEnabled= false)
            pd.PrimitiveTopology <- PrimitiveTopology.TriangleList
            pd.ResourceLayouts <- Array.empty<ResourceLayout>
            pd.ShaderSet <- ShaderSetDescription(
                vertexLayouts =  [|vertexLayout|] ,
                shaders = shaders )
            pd.Outputs <- graphicsDevice.SwapchainFramebuffer.OutputDescription
            pd

        let pipeline = factory.CreateGraphicsPipeline pipelineDescription
        let commandList = factory.CreateCommandList()
        commandList, graphicsDevice, buffers, pipeline, shaders, uint32 quadVerticies.Length

    let Draw (commands: CommandList, graphicsDevice: GraphicsDevice, buffers, pipeline:Pipeline, shaders, indexCount) = 
        let quadVerticies = (createScene Environment.TickCount64)
        do updateBuffers graphicsDevice buffers quadVerticies
        commands.Begin()
        commands.SetFramebuffer graphicsDevice.SwapchainFramebuffer
        commands.ClearColorTarget (0u, RgbaFloat.CornflowerBlue)
        commands.SetVertexBuffer (0u, buffers.Vertex)
        commands.SetIndexBuffer (buffers.Index, IndexFormat.UInt16)
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
        buffers.World.Dispose()
        buffers.View.Dispose()
        buffers.Projection.Dispose()
        buffers.Vertex.Dispose()
        buffers.Index.Dispose()
        pipeline.Dispose()
        shaders |> Array.iter(fun x -> x.Dispose())
        
    do dispose()
    0 // return an integer exit code
