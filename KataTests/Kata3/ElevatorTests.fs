module KataTests.Kata3.ElevatorTests

open Xunit
open Katas.Kata3.Elevator
open FsUnit.Xunit

[<Fact>]
let ``Simple "closest elevator" scenario``() =
    let response = determineElevatorResponse 1 2 (0, Up) (4, Down)
    
    response |> should equal (Left, Right)
    
[<Fact>]
let ``Reverse of Simple "closest elevator" scenario``() =
    let response = determineElevatorResponse 2 1 (0, Up) (4, Down)
    
    response |> should equal (Right, Left)

[<Fact>]
let ``All distances equal prefer the right elevator for the first call``() =
    let response = determineElevatorResponse 1 1 (0, Up) (0, Down)
    
    response |> should equal (Right, Left)

[<Fact>]
let ``Left at 1 takes 2 and 3 both going up`` () =
    let response = determineElevatorResponse 1 0 (2, Up) (3, Up)
    response |> should equal (Left, Left)