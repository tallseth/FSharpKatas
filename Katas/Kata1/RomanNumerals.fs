module Katas.Kata1.RomanNumerals

let I = "I", 1
let IV = "IV", 4
let V = "V", 5
let IX = "IX", 9
let X = "X", 10
let XL = "XL", 40
let L = "L", 50
let XC = "XC", 90
let C = "C", 100
let CD = "CD", 400
let D = "D", 500
let CM = "CM", 900
let M = "M", 1000

let alphabet = [I; IV; V; IX; X; XL; L; XC; C; CD; D; CM; M]


let getGlyphFromString (romanNumeral:string) =
    let twoLetterMatch = List.tryFind (fun x -> (fst x) = romanNumeral.[0..1]) alphabet
    let oneLetterMatch = List.tryFind (fun x -> (fst x) = romanNumeral.[0..0]) alphabet
    match (twoLetterMatch, oneLetterMatch) with
    | Some x, _ -> x
    | None, Some x -> x
    | _ -> failwith "Invalid Numeral"


let rec private fromRomanNumeralRecursive baseNumber (romanNumeral:string) =
    match romanNumeral.Length with
    | 0 -> baseNumber
    | _ ->
        let currentSymbol, currentNumber = getGlyphFromString romanNumeral
        fromRomanNumeralRecursive (baseNumber + currentNumber) (romanNumeral.Substring currentSymbol.Length)

let fromRomanNumeral = fromRomanNumeralRecursive 0

let rec private toRomanNumeralRecursive accumulatingNumeral remainingNumber =
    match remainingNumber with
       | 0 -> accumulatingNumeral
       | _ ->
            let (s, n) = alphabet
                            |> List.where (fun x-> (snd x) <=  remainingNumber)
                            |> List.maxBy snd
            toRomanNumeralRecursive (accumulatingNumeral + s) (remainingNumber - n)
    

let toRomanNumeral (number:int) =
    toRomanNumeralRecursive "" number
    