module Katas.Kata3.Elevator

type Direction = Up | Down

type Floor = int

type Elevator = Left | Right

type ElevatorCall = Floor * Direction

type ElevatorState = Floor * Floor



let determineElevatorResponse (state:ElevatorState) (calls:ElevatorCall list) =
    [Left; Right]