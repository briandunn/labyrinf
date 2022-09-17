module Labyrinf.Wilson

// implementation of
// https://en.wikipedia.org/wiki/Maze_generation_algorithm#Wilson's_algorithm

open Extensions

type Side =
    | North = 0
    | South = 1
    | East = 2
    | West = 3

type Cell = { Visited: bool; Walls: Side Set }

type Walk = (int * int) list

type State = { Grid: Cell Grid; Walk: Walk }

type Next = (Cell Grid * State) option

let allSides =
    [ Side.North
      Side.South
      Side.East
      Side.West ]
    |> Set.ofList

let rand (max: int) =
    max
    |> float
    |> ((*) (Math.random ()))
    |> round
    |> int

let random (f: (int * int * 'a) -> bool) (grid: 'a Grid) =
    let candidates = grid |> Grid.cells |> Seq.filter f

    let (x, y, _) =
        Seq.item (rand (Seq.length candidates)) candidates

    x, y

let findNeighbors (x, y) grid =
    let fold acc (x', y') =
        match Grid.tryGet x' y' grid with
        | Some cell -> Set.add (x', y', cell) acc
        | None -> acc

    [ x - 1, y
      x + 1, y
      x, y - 1
      x, y + 1 ]
    |> List.fold fold Set.empty

let randomSetMember set =
    match Set.count set with
    | n when n > 0 ->
        let index = (rand (n - 1))

        set |> Set.toList |> List.item index |> Some
    | _ -> None

let removeWall a b grid =
    let removeWall wall ({ Walls = walls } as cell) =
        { cell with Walls = Set.remove wall walls }

    let map (removeFromA, removeFromB) =
        grid
        |> Grid.update a (removeWall removeFromA)
        |> Grid.update b (removeWall removeFromB)

    match a, b with
    | (ax, ay), (bx, by) when ax = bx && ay = by + 1 -> // a below b
        Some(Side.North, Side.South)
    | (ax, ay), (bx, by) when ax = bx && ay = by - 1 -> // a above b
        Some(Side.South, Side.North)
    | (ax, ay), (bx, by) when ay = by && ax = bx + 1 -> // a right of b
        Some(Side.West, Side.East)
    | (ax, ay), (bx, by) when ay = by && ax = bx - 1 -> // a left of b
        Some(Side.East, Side.West)
    | _ -> None
    |> Option.map map
    |> Option.defaultValue grid

let notVisited (_, _, { Visited = visited }) = not visited

let markVisited cell = { cell with Visited = true }

let resetWalk: Cell Grid -> Walk =
    let extractCoordinates (x, y, _) = x, y

    Grid.cells
    >> Seq.filter notVisited
    >> Seq.tryHead
    >> Option.map extractCoordinates
    >> Option.toList

let rec applyWalk ({ Grid = grid; Walk = walk } as state) =
    match walk with
    | head :: neck :: tail ->
        applyWalk
            { Grid =
                grid
                |> Grid.update head markVisited
                |> removeWall head neck
              Walk = neck :: tail }
    | start :: _ ->
        applyWalk
            { Grid = Grid.update start markVisited grid
              Walk = [] }
    | [] -> state

let eraseLoop head tail =
    match tail |> List.skipWhile ((<>) head) with
    | [] -> (* no loop *) head :: tail
    | unlooped -> unlooped

let stepLoopErasedWalk ({ Grid = grid; Walk = walk } as state) =
    let mapNeighbor (x, y, { Visited = visited }) =
        if visited then
            applyWalk { state with Walk = (x, y) :: walk }
        else
            { state with Walk = eraseLoop (x, y) walk }

    let notAt (x1, y1) (x2, y2, _) = (x1, y1) <> (x2, y2)

    (match walk with
     | head :: neck :: _ ->
         grid
         |> findNeighbors head
         |> Set.filter (notAt neck)
     | head :: _ -> findNeighbors head grid
     | [] -> Set.empty)
    |> randomSetMember
    |> Option.map mapNeighbor

let next: State -> Next =
    let next ({ Walk = walk; Grid = grid } as state) =
        if List.isEmpty walk then
            grid, { state with Walk = resetWalk grid }
        else
            (applyWalk { state with Grid = Grid.copy grid })
                .Grid,
            state

    stepLoopErasedWalk >> Option.map next

let init (width, height) : State =
    let grid =
        Grid.create width height { Visited = false; Walls = allSides }

    { Grid = Grid.update ((Grid.colCount grid) - 1, (Grid.rowCount grid) - 1) markVisited grid
      Walk = [ 0, 0 ] }

let resize (newSize: int * int) =
    Grid.resize newSize { Visited = false; Walls = allSides }

let mapGrid (fn: Cell Grid -> 'a) : Next -> 'a option =
    let map (grid, _state) = fn grid
    Option.map map

let generate dimensions = Seq.unfold next <| init dimensions
