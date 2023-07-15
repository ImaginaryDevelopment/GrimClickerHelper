module Index

open Elmish

open Elmish.React
open Fable.Core.JsInterop
open Fable.Import
open Fable.FontAwesome
open Fable.React
open Fable.React.Props
open Fulma

open Shared
open Shared.Helpers
open CodeHelpers.FableHelpers


// open GrimClickerHelper
open Components.SharedComponents
open Components.SharedComponents.TabLink

let private debug = false

type Component =
    | Summary

    with
        static member All =
            [
                Summary
            ]

type ComponentStates = {
    Summary: Components.Summary.Model
}

type State = {
    ActiveTab: Component
    ShowTextMenus: bool
    Theme: string
}

type Model = {
    ComponentStates: ComponentStates
    AppState:State
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events

type ComponentMsg =
    | SumMsg of Components.Summary.Msg

type Msg =
    | TabChange of Component
    | ThemeChange of string option
    | TextMenuChange
    | CMsg of ComponentMsg

// debug section forces local dev code consistentcy across components
#if DEBUG
// model, msg, init, update
[<RequireQualifiedAccess>]
type InitType<'tState,'tInit,'tMsg> =
    | Value of 'tState*Cmd<'tMsg>
    | Method of ('tInit -> 'tState*Cmd<'tMsg>)

type SubComponent<'tProps,'tState,'tMsg, 'tInit> = {
    Wrapper: 'tMsg -> Msg
    Init: InitType<'tState,'tInit,'tMsg>
    View: 'tProps -> 'tState -> ('tMsg -> unit) -> ReactElement
    Update: 'tMsg -> 'tState -> 'tState * Cmd<'tMsg>
}

let _subcomponents x =
    match x with
    | Summary _ ->
        {
            Wrapper= SumMsg >> CMsg
            Init= InitType.Method Components.Summary.init
            View= fun _ -> Components.Summary.view // ignore 'props
            Update= Components.Summary.update
        }
        |> ignore


#endif

module Storage =
    open BrowserStorage
    let app : StorageAccess<State> =  BrowserStorage.StorageAccess.CreateStorage "AppState"
    let sum = BrowserStorage.StorageAccess.CreateStorage "AppState_Sum"

let init () =
    printfn "Starting up Index.init"
    let inline mapCmd title (wrapper: _ -> Msg) (cmd1:Cmd<Msg>) init fOverride : 't * Cmd<Msg> =
        let m,cmd =
            try
                fOverride()
            with ex ->
                eprintfn "Failed to deserialize for %s: %s" title ex.Message
                None
            |> init

        m, cmd |> Cmd.map wrapper |> List.append cmd1

    let sum, cmd = mapCmd "SumInit" (SumMsg>>CMsg) Cmd.none Components.Summary.init Storage.sum.Get
    let app =
        try
        Storage.app.Get()
        |> function
            | Some x -> x
            | None ->
                eprintfn "init: no stored site"
                { ActiveTab= Summary; ShowTextMenus= false; Theme= ""}
        with ex ->
            eprintfn "App Init: %s" ex.Message
            { ActiveTab= Summary; ShowTextMenus= false; Theme= ""}
    if debug then Fable.Core.JS.console.log("starting up app with state", Resolver.Serialize app)

    let model =
        {   AppState = app
            ComponentStates= {
                            Summary = sum
            }
        }
    // Fable.Core.JS.console.log("starting up app with comstate", model.ComponentStates)
    model,cmd

let updateC msg cs =
    let inline fRegular fu msg model save fmsg fmodel =
        let next,cmd = fu msg model
        Some next
        |> save
        |> ignore
        fmodel cs next, cmd |> Cmd.map fmsg

    match msg with
    | SumMsg msg ->
        fRegular Components.Summary.update msg cs.Summary Storage.sum.Save
            SumMsg
            <| fun model next -> {model with Summary= next}


let update (msg:Msg) (model:Model) =
    let lensState f =
        let next = f model.AppState
        Storage.app.Save (Some next)
        |> function
            | Ok () -> ()
            | Error e ->
                eprintfn "Storage failed"
        {model with AppState= next}
    match msg with
    | TabChange c ->
        lensState (fun s -> {s with ActiveTab= c}), Cmd.none
    | ThemeChange t ->
        lensState (fun s -> {s with Theme = t |> Option.defaultValue ""}), Cmd.none
    | TextMenuChange ->
        lensState (fun s -> {s with ShowTextMenus = not s.ShowTextMenus}), Cmd.none

    | CMsg msg ->
        let next,cmd = updateC msg model.ComponentStates
        {model with ComponentStates = next},cmd |> Cmd.map CMsg

importAll "./style.scss"

let tabSelector ({AppState={Theme=theme;ActiveTab=at};ComponentStates=cs} as x) (dispatch:ComponentMsg -> unit) =
    try
        match at with
        | Summary -> Components.Summary.view cs.Summary (SumMsg >> dispatch)
        // | Bazaar ->
        //     Components.Bazaar.view {Theme=theme} cs.Bazaar (BazMsg >> dispatch)
    with ex ->
        div [] [
            unbox ex.Message
        ]

let view (model : Model) (dispatch : Msg -> unit) =
    printfn "Starting up Index.view"
    let tabs =
        Component.All
        |> List.map(fun x ->
            let icon =
                match x with
                | Summary -> Fa.Solid.Brain
                // | Bazaar -> Fa.Solid.DollarSign
                // | Brewing -> Fa.Solid.Flask
                // | Collections -> Fa.Solid.Warehouse
                // | Damage -> Fa.Solid.Biohazard
                // | Enchanting -> Fa.Solid.HatWizard
                // | EventCalc -> Fa.Solid.CalendarAlt
                // | Minions -> Fa.Solid.HardHat
                // | Pets -> Fa.Solid.Bone


            {| c= x; icon = icon |}
        )

    let tabIt (c:Component) (icon:Fa.IconOption) =
        TabLink {Name= string c; Active=Some <| string model.AppState.ActiveTab
                 Title= None; OnClick= fun _ -> TabChange c |> dispatch
                 Children= [
                    Fa.FaIcon List.empty icon
                 ] }
    div []
        [

            TabContainer None None (
                [
                    yield! tabs
                    |> List.map(fun x ->
                        if model.AppState.ShowTextMenus then
                            TabTextLink (string x.c) (Some <| string model.AppState.ActiveTab) (fun _ -> TabChange x.c |> dispatch)
                        else
                            tabIt x.c (x.icon)
                    )
                    yield li [Class "select is-pulled-right"] [
                        select [
                            OnChange (getTargetValue("theme select") >> Msg.ThemeChange >> dispatch)
                            Value model.AppState.Theme
                        ] [
                            option [Value ""] [unbox "Themes..."]
                            option [Value "callout"] [unbox "Callout"]
                            option [Value "text"] [unbox "Text"]
                        ]
                    ]
                    yield li [Class "m-left"] [
                        label [Class "checkbox"] [
                            input [ Type "checkbox"; Checked model.AppState.ShowTextMenus
                                    OnChange (fun _ -> dispatch Msg.TextMenuChange)
                            ]
                            unbox "Text menus"
                        ]
                    ]
                ]
            )

            Container.container [] []
            h2 [Class "is-size-2 has-text-centered"] [
                unbox (string model.AppState.ActiveTab)
            ]
            tabSelector model (CMsg >> dispatch)
        ]