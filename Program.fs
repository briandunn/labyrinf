module Prim =
    (*
  prim
  Start with a grid full of walls.
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

    let randFloat () =
        (float (System.Random.Shared.Next()))
        / (float System.Int32.MaxValue)

    let rand (max: int) =
        max
        |> float
        |> ((*) (randFloat ()))
        |> round
        |> int

    let randomCell (width, height) = rand (width - 1), rand (height - 1)

    let neighbors (width, height) (x, y) =
        let inBounds (x', y') =
            x' >= 0 && x' < width && y' >= 0 && y' < height

        [ x - 1, y
          x + 1, y
          x, y - 1
          x, y + 1 ]
        |> List.filter inBounds
        |> Set.ofList


    let formatGrid grid =
        let formatCell cell =
            match cell.Kind, cell.Visited with
            | Passage, _ -> "  "
            | Wall, _ -> "\x1b[42m  \x1b[0m"

        seq {
            yield "\x1b[H"

            for row in 0 .. Array2D.length2 grid - 1 do
                yield! seq { for col in 0 .. Array2D.length1 grid - 1 -> formatCell (Array2D.get grid col row) }

                yield "\n"
        }
        |> String.concat ""

    let generate ((width, height) as dimensions) =
        let unfold (grid, walls) =
            let countVisted count (x, y) =
                if (Array2D.get grid x y).Visited then
                    count + 1
                else
                    count

            if Set.isEmpty walls then
                None
            else
                let (x, y) as wall =
                    walls
                    |> Set.toList
                    |> List.item (rand ((Set.count walls) - 1))

                let neighbors = neighbors dimensions wall

                if neighbors |> Set.fold countVisted 0 |> ((=) 1) then
                    Array2D.set grid x y { Kind = Passage; Visited = true }

                    Some(grid, (grid, walls |> Set.remove wall |> Set.union neighbors))
                else
                    Some(grid, (grid, walls |> Set.remove wall))

        let grid: Cell [,] =
            Array2D.create width height { Visited = false; Kind = Wall }

        let (x, y) as start = randomCell dimensions

        let walls = neighbors dimensions start

        Array2D.set grid x y { Visited = true; Kind = Wall }

        Seq.unfold unfold (grid, walls)


(30, 30)
|> Prim.generate
|> Seq.iter (Prim.formatGrid >> printfn "%s")
