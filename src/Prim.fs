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

type Cell =
    | Wall = 0
    | Passage = 1

type Frontier = (int * int * Cell) Set
type State = Cell Grid * Frontier
type Next = (Cell Grid * State) option

let rand (max: int) =
    max
    |> float
    |> ((*) (Math.random ()))
    |> round
    |> int

let randomCell (width, height) = rand (width - 1), rand (height - 1)

let twoAway x y grid =
    let fold acc (x', y') =
        match Grid.tryGet x' y' grid with
        | Some cell -> Set.add (x', y', cell) acc
        | None -> acc

    [ x - 2, y
      x + 2, y
      x, y - 2
      x, y + 2 ]
    |> List.fold fold Set.empty

let findNeighbors (x, y) =
    twoAway x y
    >> Set.filter (function
        | (_, _, Cell.Passage) -> true
        | _ -> false)

let findFrontiers (x, y) =
    twoAway x y
    >> Set.filter (function
        | (_, _, Cell.Wall) -> true
        | _ -> false)

let randomSetMember set =
    match Set.count set with
    | n when n > 0 ->
        let index = (rand (n - 1))

        set |> Set.toList |> List.item index |> Some
    | _ -> None

let between a b =
    match a, b with
    | (ax, ay), (bx, by) when ax = bx && ay = by + 2 -> ax, by + 1
    | (ax, ay), (bx, by) when ax = bx && ay = by - 2 -> ax, by - 1
    | (ax, ay), (bx, by) when ay = by && ax = bx + 2 -> bx + 1, ay
    | (ax, ay), (bx, by) when ay = by && ax = bx - 2 -> bx - 1, ay

let next ((grid, frontiers): State) : Next =
    let map ((fx, fy, _) as frontier) =

        let grid =
            match grid
                  |> Grid.set fx fy Cell.Passage
                  |> findNeighbors (fx, fy)
                  |> randomSetMember
                with
            | Some (nx, ny, _) ->
                let (px, py) = between (fx, fy) (nx, ny)
                Grid.set px py Cell.Passage grid
            | None -> grid

        grid,
        (grid,
         frontiers
         |> Set.union (findFrontiers (fx, fy) grid) // must be returning previously visited frontiers some of the time
         |> Set.remove frontier)

    frontiers |> randomSetMember |> Option.map map

let init ((width, height) as dimensions) : State =
    let grid: Cell Grid = Grid.create width height Cell.Wall

    let (x, y) as start = randomCell dimensions

    let frontiers = findFrontiers start grid

    Grid.set x y Cell.Passage grid, frontiers

let resize (newSize: int * int) ((grid, fronts): State) =
    let grid = Grid.resize newSize Cell.Wall grid
    let w = Grid.colCount grid
    let h = Grid.rowCount grid
    let inGrid (x, y, _) = x < w && y < h

    (grid, Set.filter inGrid fronts)

let mapGrid (fn: Cell Grid -> 'a) : Next -> 'a option =
    let map (grid, _state) = fn grid
    Option.map map

let generate dimensions = Seq.unfold next <| init dimensions
