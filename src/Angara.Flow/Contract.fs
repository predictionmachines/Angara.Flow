﻿namespace Angara

open System
open Angara.Execution
open System.Diagnostics.Contracts


type InputContract = 
    { Type : Type 
      Name : string }

type OutputContract = 
    { Type : Type
      Name : string }

[<System.Diagnostics.DebuggerDisplay("Contract {Id} {DisplayName}")>]
type MethodContract = 
    { Id : string
      TypeArgs : System.Type list
      DisplayName : string
      Description : string
      Inputs : InputContract list
      Outputs : OutputContract list 
    }

[<AbstractClass>]
[<System.Diagnostics.DebuggerDisplay("Method {Contract.Id} {Id}")>]
type Method(id: MethodId, contract: MethodContract) =
    inherit ExecutableMethod(id, contract.Inputs |> List.map(fun c -> c.Type), contract.Outputs |> List.map(fun c -> c.Type))

    member x.Contract = contract

    
[<System.Diagnostics.DebuggerDisplay("Method {Contract.Id} {Id}")>]
type FunctionMethod(id: MethodId, contract: MethodContract, func: Artefact list * MethodCheckpoint option -> (Artefact list * MethodCheckpoint) seq) =
    inherit Method(id, contract)

    override x.Execute (inputs: Artefact list, chk: MethodCheckpoint option) : (Artefact list * MethodCheckpoint) seq =
        func(inputs, chk)

module BasicMethods =
    let value_ContractId = "value_"
    
    let createMakeValueUntyped (v:obj) (t:Type) = 
        let valueId = value_ContractId + t.FullName
        FunctionMethod(
            Guid.NewGuid(), 
            { Id = valueId
            ; TypeArgs = []
            ; DisplayName = "make value"
            ; Description = "Produces a constant value"
            ; Inputs = List.empty
            ; Outputs = [ { OutputContract.Type = t; OutputContract.Name = "value" } ] },
            fun (_, _) -> Seq.singleton ([box v], null))

    let createMakeValue<'a> (v:'a) = createMakeValueUntyped v typeof<'a>

