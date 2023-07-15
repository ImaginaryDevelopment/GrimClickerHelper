module Components.Summary
open Fable.React
open Fable.React.Props

open Elmish
open Fulma

open Shared
open Shared.Helpers
open CodeHelpers.FableHelpers
open Components.SharedComponents
type Mask =
    | Fire
type Stats = {
    Str: int
    Dex: int
    Wis: int
    StrDiscount: int
    DexDiscount: int
    WisDiscount: int
}
type Model = {
    UseScientific: bool
    Mask: Mask option
    WeaponBaseDamage: int
    Stats: Stats
}

type StatChange =
    | StrChange of int
    | DexChange of int
    | WisChange of int
    | StrDiscChange of int
    | DexDiscChange of int
    | WisDiscChange of int
    | MaskChange of Mask option
    | WeaponBaseDamageChange of int

type Msg =
    | StatChange of StatChange

// override allows parent to load from storage old values for example
let init overrideOpt : Model * Cmd<Msg> =
    overrideOpt
    |> Option.defaultValue {
        Stats = {
            Str=0
            Dex=0
            Wis=0
            StrDiscount=0
            DexDiscount=0
            WisDiscount=0
        }
        Mask=None
        WeaponBaseDamage=1
        UseScientific=false
    }, Cmd.none

let update msg model : Model * Cmd<Msg> =
    match msg with
    | StatChange v ->
        let updateStat f = {model with Stats = f model.Stats}
        match v with

        |StrChange v -> updateStat (fun stat -> {stat with Str= v}), Cmd.none
        |DexChange v -> updateStat (fun stat -> {stat with Dex = v}), Cmd.none
        |WisChange v -> updateStat (fun stat -> {stat with Wis= v}), Cmd.none

        |StrDiscChange v -> updateStat (fun stat -> {stat with StrDiscount= v}), Cmd.none
        |DexDiscChange v -> updateStat (fun stat -> {stat with DexDiscount= v}), Cmd.none
        |WisDiscChange v -> updateStat (fun stat -> {stat with WisDiscount= v}), Cmd.none

        |MaskChange v -> {model with Mask = v}, Cmd.none
        |WeaponBaseDamageChange v -> {model with WeaponBaseDamage = v}, Cmd.none

let inputComponent (model: Model) (dispatch : Msg -> unit) =
    let statInput (labelText:string) name value statChangeType =
        [
            label [] [unbox labelText]
            NumberInput {
                Name= name
                Value = if value >= 0 then Some 0 else None
                OnChange = (fun nv -> nv.Value |> Option.map int |> Option.defaultValue 0 |> statChangeType |> Msg.StatChange |> dispatch)
                Placeholder = Some "0"
            }
        ]

    ul [] [
        li [] (statInput "STR" "str" model.Stats.Str StatChange.StrChange)
        li [] (statInput "DEX" "dex" model.Stats.Dex StatChange.DexChange)
        li [] (statInput "WIS" "wis" model.Stats.Wis StatChange.WisChange)
        // this causes rounding on the input
        li [] (statInput "WPN" "wpn" model.WeaponBaseDamage StatChange.WeaponBaseDamageChange)
    ]

let getStrBonus strength mask =
    match mask with
    | Some Fire -> strength * 50 * 10 // b11
    | _ -> strength * 50 // b10

let strSummary strength =
    let strVal = getStrBonus strength None
    let strMult = 1.0 + float strVal / 100.0
    let mofVal = getStrBonus strength (Some Fire)
    let mofMult = 1.0 + float mofVal / 100.0
    // name, %, mult, text, rel inc
    [
        tr [] [
            td [] [ unbox "STR bonus (basic)"] // name
            td [] [ strVal |> string |> unbox] // %
            td [] [ sprintf "%.1f" strMult |> unbox] // mult
            td [] [ sprintf "+%i%%" strVal |> unbox] // text
        ]
        tr [] [
            td [] [ unbox "STR bonus (Mask of Fire)"] // name
            td [] [ mofVal|> string |> unbox ]
            td [] [ sprintf "%.1f" mofMult |> unbox]
            td [] [ sprintf "+%i%%" mofVal|> unbox] // text
            td [] [ mofMult / strMult |> sprintf "%.1f" |> unbox]
        ]
    ]

let statSummaryText mask str dex wis =
    let strBonus = getStrBonus str mask // b14
    Table {|
        headers = [ "Name";"Bonus Text"]
        children = [
            "STR", sprintf "+%i%% damage" strBonus
            "DEX", sprintf "+%i max energy, +%i%% crit chance" (dex * 10) (dex * 2)
         ] |> List.map(fun (n,v) -> tr [] [td [] [unbox n]; td [] [ unbox v]])

    |}
let displayComponent (model: Model) (dispatch : Msg -> unit) =
    Components.SharedComponents.Table {|
        headers= ["Name";"%";"Multiplier";"Text";"Rel. inc."]
        children= [
            yield! strSummary (int model.Stats.Str)
        ]
    |}

let view (model: Model) (dispatch : Msg -> unit) =
    div [] [
        inputComponent model dispatch
        statSummaryText model.Mask model.Stats.Str model.Stats.Dex model.Stats.Wis
        displayComponent model dispatch
        div [] [model.WeaponBaseDamage.ToString("e") |> unbox]
    ]