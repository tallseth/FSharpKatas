module Katas.Kata3.Elevator

type Direction = Up | Down | Stay

type Floor = int

type Elevator = Left | Right

type ElevatorCall = Floor * Direction

type ElevatorState = Floor * Floor

type Route = { Distance:int;  Direction:Direction }

type CallRoutes = { RouteToCallOne:Route; RouteToCallTwo:Route;}

let determineRoute currentElevatorFloor call =
    let floorCalledFrom = (fst call)
    
    {
        Distance = abs(currentElevatorFloor - floorCalledFrom)
        Direction = if floorCalledFrom > currentElevatorFloor then Up else if floorCalledFrom = currentElevatorFloor then Stay else Down
    }
    
let callDirectionsAreTheSame x y =
    (snd x) = (snd y)

let otherElevator (elevator:Elevator) =
    match elevator with
    | Left -> Right
    | Right -> Left

let calculateCallRoutesForElevator currentFloor callOne callTwo =
    let callOneDistance = determineRoute currentFloor callOne
    let callTwoDistance = determineRoute currentFloor callTwo
    {
        RouteToCallOne = callOneDistance
        RouteToCallTwo = callTwoDistance
    }

let directionsAreCompatible (directions:Direction list) =
    let hasUp = List.contains Up directions
    let hasDown = List.contains Down directions
    not (hasUp && hasDown)

let doesNotChangeDirections (x:CallRoutes) =
    directionsAreCompatible [x.RouteToCallOne.Direction; x.RouteToCallTwo.Direction]
    
let doesNotGoAgainstCalledDirection (call:ElevatorCall) (x:CallRoutes) = 
    directionsAreCompatible [x.RouteToCallOne.Direction; x.RouteToCallTwo.Direction; (snd call)]


let determineElevatorResponse (leftElevatorCurrentFloor:Floor) (rightElevatorCurrentFloor:Floor) (callOne:ElevatorCall) (callTwo:ElevatorCall) =
    let leftRoutes = calculateCallRoutesForElevator leftElevatorCurrentFloor callOne callTwo
    let rightRoutes = calculateCallRoutesForElevator rightElevatorCurrentFloor callOne callTwo
    let routeLookup = Map.ofList [Left,leftRoutes; Right,rightRoutes] 
    
    let firstCallElevator = if leftRoutes.RouteToCallOne.Distance < rightRoutes.RouteToCallOne.Distance then Left else Right
    
    let other = otherElevator firstCallElevator
    let firstCallElevatorRoutes = routeLookup.[firstCallElevator]
    let otherElevatorRoutes = routeLookup.[other]
    let firstCallElevatorQualifiesForSecondCall = (callDirectionsAreTheSame callOne callTwo)
                                                  && firstCallElevatorRoutes.RouteToCallTwo.Distance <= otherElevatorRoutes.RouteToCallTwo.Distance
                                                  && doesNotChangeDirections firstCallElevatorRoutes
                                                  && doesNotGoAgainstCalledDirection callOne firstCallElevatorRoutes
    
    let secondCallElevator = if firstCallElevatorQualifiesForSecondCall then firstCallElevator else other

    firstCallElevator, secondCallElevator