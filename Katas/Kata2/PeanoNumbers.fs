module Katas.Kata2.PeanoNumbers

type peano = Zero | Successor of peano
let one = Successor Zero

let rec add left right =
    match left with
    | Zero ->
        right
    | Successor more ->
        add more (Successor right)
        
let rec subtract left right =
    match left, right with
    | _, Zero ->
        left
    | Successor leftRemainder, Successor rightRemainder ->
        subtract leftRemainder rightRemainder
    | Zero, Successor _ ->
        failwith "negative number"


    
