namespace ShaderTools
open FShade
open Aardvark.Base


module Tool = 
    let print (add : list<string * System.Type>) (effect : Effect) =
        match effect.LastShader with
            | Some shader ->
                let mutable index = 0
                let identifier () =
                    let i = index
                    index <- i + 1
                    i

                let existing = 
                    shader.shaderOutputs 
                    |> Map.remove Intrinsics.SourceVertexIndex 
                    |> Map.map (fun name desc -> desc.paramType, identifier())
                let additional = 
                    add 
                    |> Map.ofList 
                    |> Map.map (fun name t -> t, identifier())

                let config =
                    {
                        depthRange      = Range1d(-1.0, 1.0)
                        flipHandedness  = false
                        lastStage       = shader.shaderStage
                        outputs         = Map.union existing additional 
                    }

                let glsl410 =
                    GLSL.Backend.Create {
                        version                 = System.Version(4,5)
                        enabledExtensions       = Set.ofList [ ]
                        createUniformBuffers    = true
                        bindingMode             = GLSL.BindingMode.PerKind
                        createDescriptorSets    = true
                        stepDescriptorSets      = false
                        createInputLocations    = true
                        createPerStageUniforms  = false
                        reverseMatrixLogic      = true
                    }

                let glsl = 
                    effect
                        // compile the thing
                        |> Effect.toModule config
                        |> ModuleCompiler.compileGLSL glsl410
                        
                
                sprintf "%s" glsl.code

            | None ->
                ""
