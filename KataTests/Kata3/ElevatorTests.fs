module KataTests.Kata3.ElevatorTests

open Xunit
open Katas.Kata3.Elevator
open FsUnit.Xunit



[<Fact>]
let ``Simple "closest elevator" scenario``() =
    let startingState = 1,2
    let calls = [0, Up; 4, Down]
    
    let response = determineElevatorResponse startingState calls
    
    response |> should equal [Left; Right]
    