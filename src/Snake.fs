namespace Snake

type Position = int * int

type Direction =
  | Up
  | Down
  | Left
  | Right

type Head = Position

type Tail = Position list

type Snake = { Head: Head; Tail: Tail; Direction: Direction }

type Food = Position

type Status =
  | Active
  | Won
  | Lost

[<StructAttribute>]
type Game = { Food: Food; Snake: Snake; Size: int; Status: Status }

/// This module contains all the logic for running the snake game itself.
[<RequireQualifiedAccess>]
module Game =

  open Helpers.Helpers

  /// determins if a particular position will intersect with the boundary of the grid
  let private hitsBoundary edge (x, y) =
    x >= edge + 1 || y >= edge + 1 || x <= 0 || y <= 0

  /// Given a position and a direction, determine the next position.
  let private nextPosition (x, y) =
    function
    | Left -> (x, y - 1)
    | Right -> (x, y + 1)
    | Up -> (x - 1, y)
    | Down -> (x + 1, y)

  let gamePositions size =
    seq {
      for row in 1..size do
        for column in 1..size -> (row, column)
    }

  let creatFood size occupied =
    gamePositions size
    |> Seq.filter (not << (flip List.contains) occupied)
    |> shuffle
    |> Seq.head

  let init size =

    let head = (size / 2, size / 2)

    let food = creatFood size <| List.singleton head

    let direction = [ Up; Down; Left; Right ] |> shuffle |> Seq.head

    { Food = food
      Snake = { Head = head; Tail = []; Direction = direction }
      Size = size
      Status = Active }

  let (|GrowingTail|_|) gotFood _ = if gotFood then Some() else None

  let (|NoTail|RegularTail|) snake =
    if List.isEmpty snake.Tail then NoTail else RegularTail

  let private updateSnake snake justAte =
    let newTail =
      match snake with
      | GrowingTail justAte -> snake.Head :: snake.Tail
      | NoTail -> []
      | RegularTail ->
        snake.Head
        :: (List.rev << List.tail << List.rev) snake.Tail

    { snake with Head = nextPosition snake.Head snake.Direction; Tail = newTail }

  let private opposite =
    function
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

  let (|Perpendicular|_|) dir1 dir2 =
    if dir1 <> opposite dir2 && dir1 <> dir2 then Some() else None

  let changeDirection game proposedChange =
    match proposedChange with
    | Perpendicular game.Snake.Direction ->
      { game with Snake = { game.Snake with Direction = proposedChange } }
    | _ -> game

  let (|AlreadyOver|HitsEdge|IsFull|HitsItself|EatsFood|JustVibing|) game =
    let newHead = nextPosition game.Snake.Head game.Snake.Direction

    if game.Status = Won || game.Status = Lost then
      AlreadyOver
    elif hitsBoundary game.Size newHead then
      HitsEdge
    elif List.contains newHead game.Snake.Tail then
      HitsItself
    elif newHead = game.Food
         && List.length game.Snake.Tail = game.Size * game.Size - 2 then
      IsFull
    elif newHead = game.Food then
      EatsFood
    else
      JustVibing

  let advance game =
    match game with
    | AlreadyOver -> game
    | HitsEdge
    | HitsItself -> { game with Status = Lost }
    | IsFull -> { game with Status = Won }
    | JustVibing -> { game with Snake = updateSnake game.Snake false }
    | EatsFood ->
      let newSnake = updateSnake game.Snake true

      let newFood =
        gamePositions game.Size
        |> Seq.filter (
          not
          << (flip List.contains) (newSnake.Head :: newSnake.Tail)
        )
        |> shuffle
        |> Seq.head

      { game with Snake = newSnake; Food = newFood }
