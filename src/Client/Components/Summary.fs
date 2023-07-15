module Components.Summary
open Fable.React
open Fable.React.Props

open Elmish
open Fulma

open Shared
open Shared.Helpers
open CodeHelpers.FableHelpers
open Components.SharedComponents

type Model = {
    Str: int
    Dex: int
    Wis: int
    StrDiscount: int
    DexDiscount: int
    WisDiscount: int
}
type StatChange =
    | StrChange of int
    | DexChange of int
    | WisChange of int
    | StrDiscChange of int
    | DexDiscChange of int
    | WisDiscChange of int

type Msg =
    | StatChange of StatChange

// override allows parent to load from storage old values for example
let init overrideOpt : Model * Cmd<Msg> =
    overrideOpt
    |> Option.defaultValue {
        Str=0
        Dex=0
        Wis=0
        StrDiscount=0
        DexDiscount=0
        WisDiscount=0
    }, Cmd.none

let update msg model : Model * Cmd<Msg> =
    match msg with
    | StatChange v ->
        match v with
        |StrChange v -> {model with Str= v}, Cmd.none
        |DexChange v -> {model with Str= v}, Cmd.none
        |WisChange v -> {model with Str= v}, Cmd.none
        |StrDiscChange v -> {model with StrDiscount= v}, Cmd.none
        |DexDiscChange v -> {model with StrDiscount= v}, Cmd.none
        |WisDiscChange v -> {model with StrDiscount= v}, Cmd.none
let inputComponent (model: Model) (dispatch : Msg -> unit) =
    ul [] [
        li [] [
            label [] [unbox "STR"]
            NumberInput {
                Name= "str"
                Value = if model.Str >= 0 then Some 0 else None
                OnChange = (fun nv -> nv.Value |> Option.map int |> Option.defaultValue 0 |> StatChange.StrChange |> Msg.StatChange |> dispatch)
                Placeholder = Some "0"
            }
        ]
    ]
let view (model: Model) (dispatch : Msg -> unit) =
    div [] [
        inputComponent model dispatch
    ]