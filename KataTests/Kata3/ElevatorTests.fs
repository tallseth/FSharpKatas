module KataTests.Kata3.ElevatorTests

open Xunit
open Katas.Kata3.Elevator
open FsUnit.Xunit

[<Fact>]
let ``From examples on the wiki page``() =
    //The closest elevator to a call should take the call.
    determineElevatorResponse 0 3 (3, Down) (0, Up) |> should equal (Right, Left)
    determineElevatorResponse 0 3 (1, Down) (4, Down) |> should equal (Left, Right)
    
    //If both elevators are equally close to the call, the default elevator is 'right'
    determineElevatorResponse 2 2 (2, Down) (3, Up) |> should equal (Right, Left)
    determineElevatorResponse 1 1 (2, Down) (2, Up) |> should equal (Right, Left)
    
    //An elevator will take both calls only if ALL of these conditions are met:

    //Both calls are in the same direction
    determineElevatorResponse 0 0 (1, Up) (3, Up) |> should equal (Right, Right)
    determineElevatorResponse 4 4 (3, Down) (2, Down) |> should equal (Right, Right)
    determineElevatorResponse 4 4 (3, Down) (2, Up) |> should equal (Right, Left)
    
    //The same elevator is closer to both calls
    determineElevatorResponse 3 4 (3, Down) (2, Down) |> should equal (Left, Left)
    determineElevatorResponse 1 4 (4, Down) (1, Down) |> should equal (Right, Left)
    
    //The elevator doesn't have to change it's direction to get them both.
    determineElevatorResponse 2 2 (4, Down) (1, Down) |> should equal (Right, Left)
    determineElevatorResponse 0 0 (2, Up) (3, Up) |> should equal (Right, Right)
    determineElevatorResponse 0 0 (3, Up) (2, Up) |> should equal (Right, Right)
    determineElevatorResponse 0 0 (3, Up) (0, Up) |> should equal (Right, Right)
    
    //The elevator doesn't have to go against a taken call direction to get them both.
    determineElevatorResponse 2 2 (2, Down) (4, Down) |> should equal (Right, Left)
    determineElevatorResponse 0 3 (2, Down) (4, Down) |> should equal (Right, Left)