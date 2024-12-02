namespace PolygonDrawing

open FsCheck
open Expecto

module Tests =
     
    let empty = { finishedPolygons = []; currentPolygon = None; mousePos = None ; past = None; future = None }
    let triangle = [{ x = 0.0; y = 0.0 }; { x = 50.0; y = 100.0 };  { x = 100.0; y = 0.0 }; { x = 0.0; y = 0.0 }]
    let singlePolygon = { empty with finishedPolygons = [ triangle ] }
    let applicationStates =
        [
            empty
            singlePolygon 
        ]

    type Overrides() =
       static member Float() =
            Arb.Default.Float()
            |> Arb.filter (fun f -> not <| System.Double.IsNaN(f) &&
                                    not <| System.Double.IsInfinity(f) &&
                                    f <> -0.0) 

    let ignorePast (m : Model) = { m with past = None; future = None; mousePos = None }

    module Properties =

        let undoUndoes (m : Model) (msg : Msg) =
            let afterUndo = PolygonDrawing.update msg m |> fst |> PolygonDrawing.update Undo |> fst
            let original = ignorePast m
            let result = ignorePast afterUndo
            original = result


    let polygonEditingTests =   
        testList "Main" [
            test "finishEmptyIsSafe" {
                Expect.equal empty (PolygonDrawing.update Msg.FinishPolygon empty |> fst |> ignorePast) "model"
            }

            test "undoOnEmptyIsEmpty" {
                Expect.equal empty (PolygonDrawing.update Msg.Undo empty |> fst |> ignorePast) "model"
            }

            test "canUndoArbitraryProperties" {
                for m in applicationStates do
                    FsCheck.Check.One( { Config.Default with Arbitrary = [typeof<Overrides>]; }, Properties.undoUndoes m )
            }
            
            yield! applicationStates |> List.mapi (fun i m ->
                test $"canUndoArbitraryProperties_{i}" {
                    FsCheck.Check.One( { Config.Default with Arbitrary = [typeof<Overrides>]; }, Properties.undoUndoes m )
                }
            )

            yield! applicationStates |> List.mapi (fun i m ->
                test $"canDrawTrianglesToAnyState_{i}" {
                    let pointsDrawn = 
                        triangle 
                        |> List.rev
                        |> List.fold (fun model point -> PolygonDrawing.update (Msg.AddPoint point) model |> fst) m
                        |> PolygonDrawing.update Msg.FinishPolygon |> fst

                    Expect.equal pointsDrawn.currentPolygon None "current empty after drawing"
                    let hasBeenAdded = pointsDrawn.finishedPolygons |> List.contains triangle
                    Expect.isTrue hasBeenAdded "drawing polygon has been added"
                }
            )
        ]