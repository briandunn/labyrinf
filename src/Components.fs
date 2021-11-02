namespace Labyrinf

open Feliz

open Prim

type Cell = { color: string; x: int; y: int }
type ControlState = int * int

type Components =
    [<ReactComponent>]
    static member Cell(cell: Cell) =

        let key =
            sprintf "%s-%d-%d" cell.color cell.x cell.y

        Html.div [ prop.key key
                   prop.style [ style.backgroundColor cell.color
                                style.padding 0
                                style.borderCollapse.collapse ] ]

    [<ReactComponent>]
    static member Frame(colCount: int, cells: Cell seq) =
        let children =
            Seq.map (fun cell -> Components.Cell(cell = cell)) cells

        Html.div [ prop.style [ style.width (length.percent 100)
                                style.height (length.percent 100)
                                style.display.grid
                                style.gridTemplateColumns (colCount, length.fr 1) ]
                   prop.children children ]

    [<ReactComponent>]
    static member Controls(state: ControlState, onChange: (ControlState -> unit)) =
        let (w, h) = state

        let input (label: string) (value: int) =
            let onInputChange label (v: int) : unit =
                match label with
                | "h" -> onChange (w, v)
                | "w" -> onChange (v, h)
                | _ -> ()

            let onChange: int -> unit = onInputChange label

            Html.label [ prop.style [ style.paddingBottom (length.px 10) ]
                         prop.children [ Html.text label
                                         Html.input [ prop.style [ style.marginLeft (length.px 10)
                                                                   style.width (length.px 50) ]
                                                      prop.type'.number
                                                      prop.onChange onChange
                                                      prop.value value ] ] ]

        Html.div [ prop.style [ style.position.absolute
                                style.top (length.px 10)
                                style.backgroundColor (color.rgba (0, 0, 0, 0.2))
                                style.display.flex
                                style.padding ((length.px 10), (length.px 10), (length.px 0))
                                style.flexDirection.column
                                style.alignItems.flexEnd
                                style.right (length.px 10) ]
                   prop.children [ input "h" h
                                   input "w" w ] ]

    [<ReactComponent>]
    static member Faze() =
        let (frame, setFrame) =
            let w = 69
            let h = 69

            ((w, h), (w, h) |> Prim.init |> Prim.next)
            |> Feliz.React.useStateWithUpdater

        let setControlState newDims =
            setFrame (fun (_dims, frame) -> (newDims, frame))

        let rec loop _ =
            let fn dims ((_, ({ Grid = grid } as state)) as lastFrame) =
                match { state with Grid = grid |> Prim.resize dims }
                      |> Prim.next
                    with
                | None -> Some lastFrame
                | next ->
                    scheduleLoop ()
                    next

            setFrame
            <| fun (dims, next) -> dims, Option.bind (fn dims) next

        and scheduleLoop () =
            loop
            |> Browser.Dom.window.requestAnimationFrame
            |> ignore

        let renderFrame grid =
            let toCell ((x, y, state) as cell) =
                let color =
                    match state with
                    | Cell.Passage -> color.papayaWhip
                    | Cell.Wall -> color.rebeccaPurple

                { color = color; x = x; y = y }

            let cells = grid |> Grid.cells |> Seq.map toCell

            Components.Frame(cells = cells, colCount = Grid.colCount grid)

        Feliz.React.useEffectOnce scheduleLoop |> ignore

        Html.div [ prop.style [ style.width (length.percent 100)
                                style.height (length.vh 100)
                                style.position.relative
                                style.backgroundColor <| color.seaGreen ]
                   prop.children [ Components.Controls(state = (fst frame), onChange = setControlState)
                                   frame
                                   |> snd
                                   |> Option.map (fst >> renderFrame)
                                   |> Option.defaultValue Html.none ] ]
