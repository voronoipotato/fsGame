namespace Volumetric

open System.Collections.Generic
open System.Numerics
open System.IO
//The idea is that it's akin to a component system but for 3d models

type Align = 
    | Top | Bottom 
    | Left | Right 
    | Front | Back

type Attribute = 
    | Rotation of Quaternion //is this right??
    | Translation of Vector3
    | Scale of Vector3
    | Align of Align

type Element =
    | Scene of string * Element list
    | Model of Vector3 array * Element list
   // | PlayerInput of (InputSnapshot -> Sdl2Window -> unit)

module Stl = 
    let read filename =
        let tris = new List<Vector3>()
        let mutable triangleCount = 0u
        let readFile (s: BinaryReader) =
            s.ReadBytes(80) |> ignore //header
            triangleCount <- s.ReadUInt32() //number of triangles
            
            for i in 1 .. (int triangleCount) do
                Vector3(s.ReadSingle(), s.ReadSingle(), s.ReadSingle()) |> ignore //normal vector
                tris.Add(Vector3(s.ReadSingle(), s.ReadSingle(), s.ReadSingle())) |> ignore // Vertex 1
                tris.Add(Vector3(s.ReadSingle(), s.ReadSingle(), s.ReadSingle())) |> ignore // Vertex 2
                tris.Add(Vector3(s.ReadSingle(), s.ReadSingle(), s.ReadSingle())) |> ignore // Vertex 3
                s.ReadBytes(2) |> ignore //information bits
            ()
        using (new BinaryReader(File.Open(filename, FileMode.Open))) readFile
        tris.ToArray()

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
        //This uses the point list rather than the triangle strip where points of triangles can overlap        
        let dimensions = Vector3(l, w, h)
        [| 
        Vector3(-1.0f,-1.0f,-1.0f) // triangle 1 : begin
        Vector3(-1.0f,-1.0f, 1.0f)
        Vector3(-1.0f, 1.0f, 1.0f) // triangle 1 : end

        Vector3(1.0f, 1.0f,-1.0f)  // triangle 2 : begin
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
