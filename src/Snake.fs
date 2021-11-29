/// All the types and logic for implementing a simple game of Snake
namespace Snake

[<StructAttribute>]
type Game = { Food: Food; Snake: Snake; Size: int; Status: Status }

and Snake = { Head: Head; Tail: Tail; Direction: Direction }

and Head = Position

and Tail = Position list

and Food = Position

and Position = int * int

and Status =
  | Active
  | Won
  | Lost

and Direction =
  | Up
  | Down
  | Left
  | Right


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

  /// Accepts an int representing the size of a grid, and a list of positions representing
  /// occupied positions. Returns a position at random that is in the grid but isn't
  /// currently occupied.
  let createFood size occupied =
    gamePositions size
    |> Seq.filter (not << (flip List.contains) occupied)
    |> shuffle
    |> Seq.head

  /// Accepts an integer representing the length of the grid and creates the initial
  /// configuration for the game. The snake always starts in one of the central squares
  /// and the food is placed at random.
  let init size =

    // the (/) operator rounds down for integer division
    let head = (size / 2, size / 2)

    let food = createFood size [ head ]

    let direction = [ Up; Down; Left; Right ] |> shuffle |> Seq.head

    { Food = food
      Snake = { Head = head; Tail = []; Direction = direction }
      Size = size
      Status = Active }

  let (|GrowingTail|_|) gotFood _ = if gotFood then Some() else None

  let (|NoTail|RegularTail|) snake = if snake.Tail.IsEmpty then NoTail else RegularTail

  /// given the current snake and a boolean representing whether it's about to eat,
  /// we can create a new snake.
  let private updateSnake snake aboutToEat =
    let newTail =
      match snake with
      | GrowingTail aboutToEat -> snake.Head :: snake.Tail
      | NoTail -> []
      | RegularTail ->
        snake.Head
        :: (List.rev << List.tail << List.rev) snake.Tail

    // (List.rev << List.tail << List.rev) is a quick way to remove an element
    // from the back of a list since there's no native list function to do that.
    // Reverse it, then use List.tail to get everything after the first element,
    // then reverse it again.

    { snake with Head = nextPosition snake.Head snake.Direction; Tail = newTail }

  let private opposite =
    function
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

  let (|Perpendicular|_|) dir1 dir2 =
    if dir1 <> opposite dir2 && dir1 <> dir2 then Some() else None

  /// Accepts a game and a direction to change to and returns a game. You can
  /// only change directions perpendicularly to whichever way you're going.
  /// e.g. If you're going Up, you can change to Left or Right but not Up or
  /// Down. How would you even go more Up anyway?
  let changeDirection game proposedChange =
    match proposedChange with
    | Perpendicular game.Snake.Direction ->
      { game with Snake = { game.Snake with Direction = proposedChange } }
    | _ -> game

  /// There are six distinct states the game can be in that matter for the purposes
  /// of advancement.
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

  /// Steps the game forward by one 'Tick' if the game isn't already over.
  let advance game =
    match game with
    | AlreadyOver -> game
    | HitsEdge
    | HitsItself -> { game with Status = Lost }
    | IsFull -> { game with Status = Won }
    | JustVibing -> { game with Snake = updateSnake game.Snake false }
    | EatsFood ->
      let newSnake = updateSnake game.Snake true
      let newFood = createFood game.Size (newSnake.Head :: newSnake.Tail)

      { game with Snake = newSnake; Food = newFood }
