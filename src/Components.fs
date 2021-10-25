namespace Labyrinf

open Feliz

open Prim

type Components =
    static member cell x y isFront (cell: Prim.Cell) =
        let color =
            match cell, isFront with
            | _, true -> "red"
            | Prim.Cell.Passage, _ -> "white"
            | Prim.Cell.Wall, _ -> "black"

        let key = sprintf "%s-%d-%d" color x y

        Html.div [prop.key key
                  prop.style [ style.backgroundColor color
                               style.padding 0
                               style.borderCollapse.collapse ] ]

    static member frame((grid: Prim.Cell Prim.Grid), (_,fronts)) =
        let cells = grid |> Prim.Grid.cells |> Seq.map(fun ((x,y,c) as cell) -> Components.cell x y (Set.contains cell fronts) c)

        Html.div   [ prop.style [ style.width (length.percent 100)
                                  style.height (length.percent 100)
                                  style.display.grid
                                  style.gridTemplateColumns (Prim.Grid.colCount grid, length.fr 1) ]
                     prop.children cells ]


    /// <summary>
    /// The simplest possible React component.
    /// Shows a header with the text Hello World
    /// </summary>
    [<ReactComponent>]
    static member Faze() =
        let (frame, setFrame) =
            (101, 61)
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

        let _ = Feliz.React.useEffectOnce scheduleLoop

        Html.div [ prop.style [ style.width (length.percent 100)
                                style.height (length.vh 100)
                                style.backgroundColor "green" ]
                   prop.children [ frame
                                   |> Option.map Components.frame
                                   |> Option.defaultValue Html.none ] ]
