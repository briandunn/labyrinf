namespace Labyrinf

[<RequireQualifiedAccess>]
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
