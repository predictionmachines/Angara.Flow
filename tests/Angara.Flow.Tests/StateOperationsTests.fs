﻿module ``State operations``

open Xunit
open FsUnit.Xunit
open Angara
open System
open Angara.Graph
open Angara.States

type Vertex(id: int, inps: Type list, outs: Type list) =
    member x.Id = id
    interface IVertex with
        member x.Inputs = inps
        member x.Outputs = outs
    interface IComparable with
        member x.CompareTo(other) = 
            match other with
            | :? Vertex as v -> id.CompareTo(v.Id)
            | _ -> invalidArg "other" "cannot compare values of different types"
    override x.Equals(other) =
        match other with
        | :? Vertex as y -> x.Id = y.Id
        | _ -> false
    override x.GetHashCode() = x.Id.GetHashCode()
        
type Data =
    interface IVertexData with
        member x.Shape: OutputShape = 
            failwith "Not implemented yet"
        
        

[<Fact; Trait("Category", "CI")>]
let ``State normalization adds a proper status when it is missing for a vertex``() =
    let v1 = Vertex(1, [], [typeof<int>])
    let v2 = Vertex(2, [typeof<int>], [typeof<int>])
    let g1 = FlowGraph<Vertex>().Add(v1).Add(v2).Connect (v2,0) (v1,0)
    let s1 = { State.Vertices = Map.empty; State.Graph = g1; State.TimeIndex = 0UL }
    
    let s2 = normalize s1
    Assert.Equal(2, s2.State.Vertices.Count) // "Number of states"
    let vs1 = s2.State.Vertices.[v1].AsScalar()
    Assert.Null(vs1.Data)
    Assert.True(match vs1.Status with VertexStatus.CanStart t when t = 0UL -> true | _ -> false)
    let vs2 = s2.State.Vertices.[v2].AsScalar()
    Assert.Null(vs2.Data)
    Assert.True(match vs2.Status with VertexStatus.Incomplete (OutdatedInputs) -> true | _ -> false)

