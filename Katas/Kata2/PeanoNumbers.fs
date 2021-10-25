module Katas.Kata2.PeanoNumbers

type peano = Zero | Successor of peano
let one = Successor Zero

let rec add left right =
    match left with
    | Zero ->
        right
    | Successor more ->
        add more (Successor right)
        
let rec private innerSubtract left right =
    match left, right with
    | _, Zero ->
        Ok left
    | Successor leftRemainder, Successor rightRemainder ->
        innerSubtract leftRemainder rightRemainder
    | Zero, Successor _ ->
        Error "negative number"

let subtract left right =
    match (innerSubtract left right) with
    | Ok difference -> difference
    | Error message -> failwith message
    
