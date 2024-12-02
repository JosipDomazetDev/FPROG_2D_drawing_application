module PolygonDrawing 

open Fable.Core
open Feliz
open Elmish

type Coord = { x : float; y : float }

type Polygon = list<Coord>

type Model = {
    finishedPolygons : list<Polygon>
    currentPolygon : Option<Polygon>
    mousePos : Option<Coord>
    past : Option<Model>
    future : Option<Model>
}


type Msg =
    | AddPoint of Coord
    | SetCursorPos of Option<Coord>
    | FinishPolygon
    | Undo
    | Redo

let init () =
    let m = 
        { 
            finishedPolygons = []
            currentPolygon = None

            mousePos = None 
            
            past = None; future = None
        }
    m, Cmd.none


let updateModel (msg : Msg) (model : Model) =
    match msg with
    | AddPoint p -> 
        match model.currentPolygon with
        | None -> 
             { model with currentPolygon = Some [p] }
        | Some currentPolygon -> 
            let updatedPolygon = p :: currentPolygon
            { model with currentPolygon = Some updatedPolygon }
    | FinishPolygon -> 
        match model.currentPolygon with
        | None -> model
        | Some c -> 
            { model with currentPolygon = None; finishedPolygons = c :: model.finishedPolygons }
    | _ -> model

let addUndoRedo (updateFunction : Msg -> Model -> Model) (msg : Msg) (model : Model) =
    match msg with
    | SetCursorPos p -> 
        { model with mousePos = p }
    | Undo -> 
        match model.past with
        | None -> model
        | Some past -> 
            { past with future = Some model }
    | Redo -> 
        match model.future with
        | None -> model
        | Some future -> future
    | _ -> 
        { updateFunction msg model with past = Some model }


let update (msg : Msg) (model : Model)  =
    let newModel = addUndoRedo updateModel msg model
    newModel, Cmd.none

[<Emit("getSvgCoordinates($0)")>]
let getSvgCoordinates o: Coord = jsNative

let viewPolygon (color : string) (points : Polygon) =
    points 
    |> List.pairwise 
    |> List.map (fun (c0,c1) ->
        Svg.line [
            svg.x1 c0.x; svg.y1 c0.y
            svg.x2 c1.x; svg.y2 c1.y
            svg.stroke(color)
            svg.strokeWidth 2.0
            svg.strokeLineJoin "round"
        ]
    )
 

let render (model : Model) (dispatch : Msg -> unit) =
    let border = 
        Svg.rect [
            svg.x1 0; svg.x2 500
            svg.y1 0; svg.y2 500
            svg.width 500; svg.height 500
            svg.stroke("black") 
            svg.strokeWidth(2)
            svg.fill "none"
        ] 

    let finisehdPolygons = 
        model.finishedPolygons |> List.collect (viewPolygon "green")
    let currentPolygon =
        match model.currentPolygon with
        | None -> []
        | Some p -> 
            let withPreview = 
                match model.mousePos with
                | None -> p
                | Some preview -> preview :: p
            viewPolygon "red" withPreview
 
    let svgElements = List.concat [finisehdPolygons; currentPolygon]

    Html.div [
        prop.style [style.custom("user-select","none")]
        prop.children [
            Html.h1 "Simplest drawing"
            Html.button [
                prop.style [style.margin 20]; 
                prop.onClick (fun _ -> dispatch Undo)
                prop.children [Html.text "undo"]
            ]
            Html.button [
                prop.style [style.margin 20]
                prop.onClick (fun _ -> dispatch Redo)
                prop.children [Html.text "redo"]
            ]
            Html.br []
            Svg.svg [
                svg.width 500
                svg.height 500
                svg.onMouseMove (fun mouseEvent -> 
                    let pos = getSvgCoordinates mouseEvent
                    dispatch (SetCursorPos (Some pos))
                )
                svg.onClick (fun mouseEvent -> 
                    if mouseEvent.detail = 1 then
                        let pos = getSvgCoordinates mouseEvent
                        AddPoint pos |> dispatch
                    elif mouseEvent.detail = 2 then
                        dispatch FinishPolygon
                    else
                        ()
                )
                svg.children (border :: svgElements)
            ]
        ]
    ]