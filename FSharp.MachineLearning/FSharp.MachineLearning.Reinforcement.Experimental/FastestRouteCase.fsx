﻿
// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Engine.fs"
open FSharp.MachineLearning.Reinforcement.Experimental

// Define your library scripting code here

let replace f sub xs = 
    let rec finish acc = function
      | [] -> acc
      | x::xs -> finish (x::acc) xs
    let rec search acc = function
      | [] -> None
      | x::xs -> 
        if f x then Some(finish ((sub x)::xs) acc)
        else search (x::acc) xs
    search [] xs

let rec replaceAt list value index =
    match list with
    | [] when index = 0 -> [ value; ]
    | [] when index > 0 -> failwith "Index bigger than list size!"
    | car::cdr when index > 0 -> car :: replaceAt cdr value (index - 1)
    | car::cdr when index = 0 -> value :: cdr
    | _ -> failwith "One or more errors occurred!"

let initialWorld = [""; ""; ""; ""; ""; "";]

let initialState : Engine.State = {
    world = initialWorld;
    reward = 0.0;
    agent = { id = "Humus" }
}

let getNextAgent (agent : Engine.Agent) = agent

let performAction (state : Engine.State) (action : Engine.Action) = 
    replaceAt initialWorld state.agent.id action.value

let reward won (state : Engine.State) = 
    match Engine.findAgentPosition state with
    | 5 -> 100.0
    | _ -> 0.0

let getAvailableActions (actionMap : Map<int, int list>) (state : Engine.State) =
    let position = Engine.findAgentPosition state
    actionMap.[position]

let lookup (map : Map<(Engine.State * Engine.Action), float>) (state : Engine.State) (action : Engine.Action) =
    match map.ContainsKey((state, action)) with
    | true -> map.[(state, action)]
    | false -> 0.0

let isWinningState (state : Engine.State) =
    let pos = Engine.findAgentPosition state
    match pos with
    | 5 -> true
    | _ -> false

(*
    TODO : This is is the section where the experiment should run. Make a random number generator
*)
do
    let actionMap = Map.ofList [ (0, [4;]); (1, [3; 5;]); (2, [3;]); (3, [1; 2; 4]); (4, [3; 5;]); (5, [1; 4;]); ]
    let getActions = getAvailableActions actionMap
    printfn "Hello world"



