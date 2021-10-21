namespace Labyrinf

open Feliz
open Prim

type Components =
    static member cell x y cell =
        let color =
            match cell.Kind with
            | Passage -> "white"
            | Wall -> "black"

        let key = sprintf "%s-%d-%d" color x y

        Html.td [ prop.key key
                  prop.style [ style.backgroundColor color ] ]

    static member frame(grid: 'a Prim.Grid) =
        let rows =
            seq {
                for row in 0 .. Grid.rowCount grid - 1 do
                    yield
                        seq {
                            for col in 0 .. Grid.colCount grid - 1 do
                                match grid |> Grid.tryGet col row with
                                | Some cell -> yield Components.cell col row cell
                                | None -> ()
                        }

            }
            |> Seq.map (fun cells -> Html.tr [ cells |> prop.children ])

        Html.table [ prop.style [ style.width (length.percent 100)
                                  style.height (length.percent 100) ]
                     prop.children [ Html.tbody [ prop.children rows ] ] ]


    /// <summary>
    /// The simplest possible React component.
    /// Shows a header with the text Hello World
    /// </summary>
    [<ReactComponent>]
    static member Faze() =
        let (frame, setFrame) =
            (50, 50)
            |> Prim.init
            |> Prim.next
            |> Feliz.React.useState


        let loop _ =
            match frame with
            | Some (_, state) ->
                match state |> Prim.next with
                | Some _ as state -> state |> setFrame
                | _ -> ()
            | _ -> ()

        let scheduleLoop () =
            loop
            |> Browser.Dom.window.requestAnimationFrame
            |> ignore

        let mapFrame fn =
            frame
            |> Option.map
                (function
                | (frame, _) -> fn frame)

        let _ =
            Feliz.React.useEffect scheduleLoop, [| frame |]

        Html.div [ prop.style [ style.width (length.percent 100)
                                style.height (length.vh 100)
                                style.backgroundColor "green" ]
                   prop.children [ Components.frame
                                   |> mapFrame
                                   |> Option.defaultValue Html.none ] ]
