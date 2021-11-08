namespace Labyrinf

open Feliz

open Prim

type ControlState = int * int

type Components =
    static member defaultDimensions = (10, 10)

    [<ReactComponent>]
    static member Cell(cell: (int * int * Cell)) =
        let (x, y, { Visited = visited; Walls = walls }) = cell

        let key = sprintf "%d-%d" x y

        let toStyle =
            (function
            | Side.North -> style.borderTopStyle
            | Side.South -> style.borderBottomStyle
            | Side.East -> style.borderRightStyle
            | Side.West -> style.borderLeftStyle)
            >> ((|>) borderStyle.solid)

        let backgroundColor =
            style.backgroundColor (
                if visited then
                    color.violet
                else
                    color.yellowGreen
            )

        Html.div [ prop.key key
                   //    prop.children [ Html.text key ]
                   prop.style (
                       (walls |> Set.toList |> List.map toStyle)
                       @ [ backgroundColor
                           style.display.flex
                           style.alignItems.center
                           style.justifyContent.center ]
                   ) ]

    [<ReactComponent>]
    static member Frame(colCount: int, cells: (int * int * Cell) seq) =
        let children =
            Seq.map (fun ((x, y, _) as cell) -> Components.Cell(cell = cell)) cells

        Html.div [ prop.style [ style.width (length.percent 100)
                                style.height (length.percent 100)
                                style.display.grid
                                style.gridTemplateColumns (colCount, length.fr 1) ]
                   prop.children children ]

    [<ReactComponent>]
    static member Controls(state: ControlState, onChange: (ControlState -> unit), onReset: unit -> unit) =
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
                                style.padding (length.px 10)
                                style.flexDirection.column
                                style.alignItems.flexEnd
                                style.right (length.px 10) ]
                   prop.children [ input "h" h
                                   input "w" w
                                   Html.input [ prop.type'.submit
                                                prop.value "reset"
                                                prop.onClick (fun _ -> onReset ()) ] ] ]

    [<ReactComponent>]
    static member Faze() =
        let (frame, setFrame) = Feliz.React.useStateWithUpdater None

        let setControlState newDims =
            setFrame
            <| Option.map (fun (_dims, frame) -> (newDims, frame))

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
            <| Option.map (fun (dims, next) -> dims, Option.bind (fn dims) next)

        and scheduleLoop () =
            loop
            |> Browser.Dom.window.requestAnimationFrame
            |> ignore

        let renderFrame grid =
            Components.Frame(cells = (Grid.cells grid), colCount = Grid.colCount grid)

        let onReset () =
            setFrame (
                (function
                | Some ((w, h), _) -> (w, h)
                | None -> Components.defaultDimensions)
                >> (fun dims -> Some(dims, dims |> Prim.init |> Prim.next))
            )

            scheduleLoop ()


        Feliz.React.useEffectOnce (fun () ->

            setFrame
            <| (fun _ ->
                Some(
                    Components.defaultDimensions,
                    Components.defaultDimensions
                    |> Prim.init
                    |> Prim.next
                ))

            scheduleLoop ())

        Html.div [ prop.style [ style.width (length.percent 100)
                                style.height (length.vh 100)
                                style.position.relative
                                style.backgroundColor <| color.seaGreen ]
                   prop.children [ Components.Controls(
                                       state = (Option.map fst frame |> Option.defaultValue (0, 0)),
                                       onChange = setControlState,
                                       onReset = onReset
                                   )
                                   frame
                                   |> Option.bind (snd)
                                   |> Option.map (fst >> renderFrame)
                                   |> Option.defaultValue Html.none ] ]
