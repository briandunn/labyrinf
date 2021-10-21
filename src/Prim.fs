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
        Grid(Array.init rows (fun _ -> (Array.create cols initialValue)))

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

    let set (col: int) (rowIndex: int) (value: 'a) ((Grid (rows) as grid): 'a Grid) : 'a Grid =
        try
            Array.set (Array.get rows rowIndex) col value
            grid
        with
        | _ -> grid

    let dimensions grid = colCount grid, rowCount grid

    let neighbors (x, y) grid =
        let (height, width) = dimensions grid

        let inBounds (x', y') =
            x' >= 0 && x' < width && y' >= 0 && y' < height

        [ x - 1, y
          x + 1, y
          x, y - 1
          x, y + 1 ]
        |> List.filter inBounds
        |> Set.ofList


type 'a Grid = 'a Grid.Grid

(*
  prim
  Start with a grid full of cells surounded by walls.
  Pick a cell, mark it as part of the maze. Add the walls of the cell to the wall list.
  While there are walls in the list:
    Pick a random wall from the list. If only one of the cells that the wall divides is visited, then:
      Make the wall a passage and mark the unvisited cell as part of the maze.
      Add the neighboring walls of the cell to the wall list.
    Remove the wall from the list.
  *)

type CellKind =
    | Wall
    | Passage

type Cell = { Visited: bool; Kind: CellKind }

let rand (max: int) =
    max
    |> float
    |> ((*) (Math.random ()))
    |> round
    |> int

let randomCell (width, height) = rand (width - 1), rand (height - 1)

let formatGrid grid =
    let formatCell cell =
        match cell.Kind, cell.Visited with
        | Passage, _ -> "  "
        | Wall, _ -> "\x1b[42m  \x1b[0m"

    seq {
        yield "\x1b[H"

        for row in 0 .. Grid.rowCount grid - 1 do
            yield!
                seq {
                    for col in 0 .. Grid.colCount grid - 1 ->
                        grid
                        |> Grid.tryGet col row
                        |> Option.map formatCell
                        |> Option.defaultValue ""
                }

            yield "\n"
    }
    |> String.concat ""

let next (grid, walls) =
    let countVisted count (x, y) =
        match Grid.tryGet x y grid with
        | Some ({ Visited = true }) -> count + 1
        | _ -> count

    if Set.isEmpty walls then
        None
    else
        let (x, y) as wall =
            walls
            |> Set.toList
            |> List.item (rand ((Set.count walls) - 1))

        let neighbors = Grid.neighbors wall grid

        if neighbors |> Set.fold countVisted 0 |> ((=) 1) then
            let grid =
                Grid.set x y { Kind = Passage; Visited = true } grid

            Some(grid, (grid, walls |> Set.remove wall |> Set.union neighbors))
        else
            Some(grid, (grid, walls |> Set.remove wall))

let init ((width, height) as dimensions) =
    let grid: Cell Grid =
        Grid.create width height { Visited = false; Kind = Wall }

    let (x, y) as start = randomCell dimensions

    let walls = Grid.neighbors start grid

    Grid.set x y { Visited = true; Kind = Wall } grid, walls

let generate dimensions = Seq.unfold next <| init dimensions
