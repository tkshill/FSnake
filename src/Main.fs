module Main

open Feliz
open Browser.Dom
open Fable.Core.JsInterop
open Snake
open Fable.Core.JS
open Helpers.Helpers

importSideEffects "./styles/global.scss"

//  STATE
type State = Game

// MESSAGES
type Message =
  | Restart
  | Dir of Direction
  | Tick

  member this.name =
    match this with
    | Dir direction -> direction.ToString()
    | otherwise -> otherwise.ToString()

let update game =
  function
  | Restart -> Game.init 10
  | Dir direction -> Game.changeDirection game direction
  | Tick -> Game.advance game

let Cell game position =

  let (x, y) = position

  let (|SnakeCell|FoodCell|EmptyCell|) p =
    if p = game.Snake.Head
       || List.contains p game.Snake.Tail then
      SnakeCell
    elif p = game.Food then
      FoodCell
    else
      EmptyCell

  let cellType =
    match position with
    | SnakeCell -> "snake"
    | FoodCell -> "food"
    | EmptyCell -> "empty"

  Html.div [ prop.id (x * 10 + y); prop.classes [ "cell"; cellType ] ]

let Button updater (msg: Message) =
  Html.button [ prop.id msg.name
                prop.text msg.name
                prop.onClick (fun _ -> updater msg) ]


let Buttons updater =
  [ Dir Up; Dir Down; Dir Left; Dir Right; Restart ]
  |> List.map (Button updater)
  |> fun elements -> Html.div [ prop.id "buttons"; prop.children elements ]


let Grid game =
  Game.gamePositions game.Size
  |> Seq.map (Cell game)
  |> fun cells -> Html.div [ prop.id "grid"; prop.children (Seq.toList cells) ]

let keyboardEvent updater (event: Browser.Types.KeyboardEvent) =
  match event.keyCode with
  | 38.0 -> updater (Dir Up)
  | 40.0 -> updater (Dir Down)
  | 37.0 -> updater (Dir Left)
  | 39.0 -> updater (Dir Right)
  | _ -> ()

[<ReactComponent>]
let App () =
  let (game, setGame) = React.useState (Game.init 10)

  let resolve = update game >> setGame

  let subscribeToTimer () =
    let subscriptionId = (flip setTimeout) 750 <| fun _ -> resolve Tick

    { new System.IDisposable with
        member this.Dispose() = clearTimeout subscriptionId }

  React.useEffect subscribeToTimer

  [ prop.id "app"
    prop.children [ Grid game; Buttons resolve ]
    prop.onKeyDown (keyboardEvent resolve) ]
  |> Html.div

ReactDOM.render (App(), document.getElementById "feliz-app")
