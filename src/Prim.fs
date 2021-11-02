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

type State = { Grid: Cell Grid; Current: int * int }

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

let randomCell (width, height) =
    let randEven n = (rand ((n - 1) / 2)) * 2
    randEven width, randEven height

let random (f: (int * int * 'a) -> bool) (grid: 'a Grid) =
    let candidates = grid |> Grid.cells |> Seq.filter f
    Seq.tryItem (rand (Seq.length candidates)) candidates

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

let pickRandomNeighbor coordinates =
    findNeighbors coordinates >> randomSetMember

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

let next { Grid = grid; Current = current } : Next =
    if grid
       |> Grid.cells
       |> Seq.exists (fun (_, _, { Visited = visited }) -> not visited) then

        grid
        |> pickRandomNeighbor current
        |> function
            | Some (x, y, { Visited = false }) ->
                let grid =
                    grid
                    |> removeWall current (x, y)
                    |> Grid.update (x, y) (fun cell -> { cell with Visited = true })

                Some(grid, { Grid = grid; Current = (x, y) })

            | Some (x, y, { Visited = true }) -> Some(grid, { Grid = grid; Current = (x, y) })
            | None -> None // not sure what to do here
    else
        None

let init ((width, height) as dimensions) : State =
    let (x, y) as start = randomCell dimensions

    { Grid =
        Grid.create width height { Visited = false; Walls = allSides }
        |> Grid.set x y { Visited = true; Walls = allSides }
      Current = start }

let resize (newSize: int * int) =
    Grid.resize newSize { Visited = false; Walls = allSides }

let mapGrid (fn: Cell Grid -> 'a) : Next -> 'a option =
    let map (grid, _state) = fn grid
    Option.map map

let generate dimensions = Seq.unfold next <| init dimensions
