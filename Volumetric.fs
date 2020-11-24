namespace Volumetric

open System.Numerics
//The idea is that it's akin to a component system but for 3d models

type Align = 
    | Top | Bottom 
    | Left | Right 
    | Front | Back

type Attribute = 
    | Rotation of Quaternion //is this right??
    | Translation of Vector3
    | Scale of Vector3
type Element =
    | Scene of string * Element list
    | Model of Vector3 array * Element list
   // | PlayerInput of (InputSnapshot -> Sdl2Window -> unit)

module Shape =
    let rectangle position width height color =
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
