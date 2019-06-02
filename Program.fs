// Learn more about F# at http://fsharp.org

open System
open Veldrid.StartupUtilities
open System.Numerics
open Veldrid
open Veldrid.SPIRV
open System.Text
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

        let vertexLayout = 
            let position = VertexElementDescription("Position", VertexElementSemantic.TextureCoordinate, VertexElementFormat.Float2)
            let color = VertexElementDescription("Color", VertexElementSemantic.TextureCoordinate, VertexElementFormat.Float4)
            VertexLayoutDescription(position, color)

        let vertexCode = 
            "
            #version 450

            layout(location = 0) in vec2 Position;
            layout(location = 1) in vec4 Color;

            layout(location = 0) out vec4 fsin_Color;

            void main()
            {
                gl_Position = vec4(Position, 0, 1);
                fsin_Color = Color;
            }
            "

        let fragmentCode = 
            "
            #version 450

            layout(location = 0) in vec4 fsin_Color;
            layout(location = 0) out vec4 fsout_Color;

            void main()
            {
                fsout_Color = fsin_Color;
            }"
        let vertexShaderDesc = ShaderDescription(ShaderStages.Vertex, Encoding.UTF8.GetBytes(vertexCode), "main" )
        let fragmentShaderDesc =  ShaderDescription(ShaderStages.Fragment, Encoding.UTF8.GetBytes(fragmentCode), "main")
        let shaders = factory.CreateFromSpirv(vertexShaderDesc, fragmentShaderDesc)
        
        let mutable pipelineDescription = GraphicsPipelineDescription()
        pipelineDescription.BlendState <- BlendStateDescription.SingleOverrideBlend;
        pipelineDescription.DepthStencilState <- DepthStencilStateDescription(
            depthTestEnabled= true,
            depthWriteEnabled= true,
            comparisonKind= ComparisonKind.LessEqual);
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
            shaders = shaders );
        ()
        
        

[<EntryPoint>]
let main argv =
    
    let wci = Game.createWindow "Suits"
    
    let window = VeldridStartup.CreateWindow(ref wci)
    Game.createResources window |> ignore
    while window.Exists do
        window.PumpEvents() |> ignore
    0 // return an integer exit code
