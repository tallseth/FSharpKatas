module Katas.Kata3.Elevator

open System.Runtime.ExceptionServices

type Direction = Up | Down

type Floor = int

type Elevator = Left | Right

type ElevatorCall = Floor * Direction

type ElevatorState = Floor * Floor

let distanceToCall position call =
    abs (position - (fst call))
    
let callDirectionsAreTheSame x y =
    (snd x) = (snd y)

let otherElevator (elevator:Elevator) =
    match elevator with
    | Left -> Right
    | Right -> Left

let determineElevatorResponse (leftElevatorCurrentFloor:Floor) (rightElevatorCurrentFloor:Floor) (callOne:ElevatorCall) (callTwo:ElevatorCall) =
    let leftDistanceToCallOne = distanceToCall leftElevatorCurrentFloor callOne
    let leftDistanceToCallTwo = distanceToCall leftElevatorCurrentFloor callTwo
    
    let rightDistanceToCallOne = distanceToCall rightElevatorCurrentFloor callOne
    let rightDistanceToCallTwo = distanceToCall rightElevatorCurrentFloor callTwo
    
    let firstCallElevator = if leftDistanceToCallOne < rightDistanceToCallOne then Left else Right
    let secondCallElevator = if not (callDirectionsAreTheSame callOne callTwo) then (otherElevator firstCallElevator) else firstCallElevator

    firstCallElevator, secondCallElevator