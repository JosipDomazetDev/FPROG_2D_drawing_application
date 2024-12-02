open FsCheck
open Expecto


module Sorting =
    let sorted xs = 
        xs 
        |> List.pairwise 
        |> List.forall (fun (p0, p1) -> p0 <= p1)

    let canSort (xs : list<int>) = 
        xs |> List.sort |> sorted

    
let tests =
    testList "Main" [
        test "Hello World" {
            let subject = "Hello World"
            Expect.equal subject "Hello World" "The strings should equal"
        }
        test "Sorting" {
            FsCheck.Check.Quick Sorting.canSort
        }
        PolygonDrawing.Tests.polygonEditingTests
    ]


[<EntryPoint>]
let main args =
  runTestsWithCLIArgs [] args tests