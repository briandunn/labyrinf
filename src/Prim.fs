module Labyrinf.Prim

module Math =
    open Fable.Core
    // interface
    type Math =
        abstract random : unit -> float

    [<Global>]
    let Math: Math = jsNative

    // emit
    [<Emit("Math.random()")>]
    let random () : float = jsNative

module Grid =
    type 'a Grid = private Grid of 'a array array

    let create (cols: int) (rows: int) (initialValue: 'a) : 'a Grid =
        Grid(Array.init rows (fun _ -> (Array.init cols (fun _ -> initialValue))))

    let tryGet (col: int) (row: int) (Grid (grid): 'a Grid) : 'a option =
        grid
        |> Array.tryItem row
        |> Option.bind (Array.tryItem col)

    let colCount (Grid (rows): 'a Grid) =
        rows
        |> Array.tryHead
        |> Option.map Array.length
        |> Option.defaultValue 0

    let rowCount (Grid (rows): 'a Grid) = rows |> Array.length

    let cells ((Grid rows): 'a Grid) : (int * int * 'a) seq =
        seq {
            for y in 0 .. ((Array.length rows) - 1) do
                let row = Array.item y rows

                for x in 0 .. ((Array.length row) - 1) do
                    yield (x, y, Array.item x row)
        }

    let set (col: int) (rowIndex: int) (value: 'a) ((Grid (rows) as grid): 'a Grid) : 'a Grid =
        Array.set (Array.get rows rowIndex) col value
        grid

    let update (col, row) (f: ('a -> 'a)) (grid: 'a Grid) : 'a Grid =
        match tryGet col row grid with
        | Some x -> set col row (f x) grid
        | None -> grid

    let dimensions grid = colCount grid, rowCount grid

    let resize ((w, h): int * int) (newValue: 'a) ((Grid (rows) as grid): 'a Grid) =
        let currentW = colCount grid
        let currentH = rowCount grid
        let clamp i = if i > 0 then i else 0

        let map row =
            Array.init (clamp (w - currentW)) (fun _ -> newValue)
            |> Array.append row
            |> Array.take w

        Array.append
            (Array.map map rows)
            (Array.init (clamp (h - currentH)) (fun _ -> Array.init w (fun _ -> newValue)))
        |> Array.take h
        |> Grid

    let contains (value: 'a) (Grid (rows): 'a Grid) : bool =
        Array.exists (Array.contains value) rows

    let copy (Grid (rows): 'a Grid) : 'a Grid = rows |> Array.map Array.copy |> Grid

type 'a Grid = 'a Grid.Grid

(*
  prim

  https://stackoverflow.com/questions/29739751/implementing-a-randomly-generated-maze-using-prims-algorithm

  Start with a Grid full of Cells in state Blocked.
  Pick a random Cell, set it to state Passage and Compute its frontier cells.
  A frontier cell of a Cell is a cell with distance 2 in state Blocked and within the grid.
  While the list of frontier cells is not empty:
    Pick a random frontier cell from the list of frontier cells.
    Neighbors of a frontier cell are all cells in distance 2 in state Passage.
    Pick a random neighbor and connect the frontier cell with the neighbor by setting the cell in-between to state Passage.
    Compute the frontier cells of the chosen frontier cell and add them to the frontier list.
    Remove the chosen frontier cell from the list of frontier cells.
  *)

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

    let notAt (x1,y1) (x2,y2,_) = (x1, y1) <> (x2,y2)

    (match walk with
    | head::neck::_ -> grid |> findNeighbors head |> Set.filter (notAt neck)
    | head::_ -> findNeighbors head grid
    | [] -> Set.empty) |> randomSetMember |> Option.map mapNeighbor

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
