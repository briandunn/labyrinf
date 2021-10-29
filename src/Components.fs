namespace Labyrinf

open Feliz

open Prim

type Cell = { color: string; x: int; y: int }

type Components =
    [<ReactComponent>]
    static member cell(cell: Cell) =

        let key =
            sprintf "%s-%d-%d" cell.color cell.x cell.y

        Html.div [ prop.key key
                   prop.style [ style.backgroundColor cell.color
                                style.padding 0
                                style.borderCollapse.collapse ] ]

    [<ReactComponent>]
    static member frame(colCount: int, cells: Cell seq) =
        let children =
            Seq.map (fun cell -> Components.cell (cell = cell)) cells

        Html.div [ prop.style [ style.width (length.percent 100)
                                style.height (length.percent 100)
                                style.display.grid
                                style.gridTemplateColumns (colCount, length.fr 1) ]
                   prop.children children ]


    [<ReactComponent>]
    static member Faze() =
        let (frame, setFrame) =
            (69, 69)
            |> Prim.init
            |> Prim.next
            |> Feliz.React.useStateWithUpdater

        let rec loop _ =
            let fn ((_, state) as lastFrame) =
                match Prim.next state with
                | None -> Some lastFrame
                | next ->
                    scheduleLoop ()
                    next

            setFrame <| Option.bind fn

        and scheduleLoop () =
            loop
            |> Browser.Dom.window.requestAnimationFrame
            |> ignore

        let renderFrame (grid, (_, fronts)) =
            let toCell ((x, y, state) as cell) =
                let color =
                    match state, Set.contains cell fronts with
                    | (_, true) -> color.steelBlue
                    | (Cell.Passage, _) -> color.papayaWhip
                    | (Cell.Wall, _) -> color.rebeccaPurple

                { color = color; x = x; y = y }

            let cells =
                grid |> Prim.Grid.cells |> Seq.map toCell

            Components.frame (cells = cells, colCount = Grid.colCount grid)

        Feliz.React.useEffectOnce scheduleLoop |> ignore

        Html.div [ prop.style [ style.width (length.percent 100)
                                style.height (length.vh 100)
                                style.backgroundColor <| color.seaGreen ]
                   prop.children [ frame
                                   |> Option.map renderFrame
                                   |> Option.defaultValue Html.none ] ]
