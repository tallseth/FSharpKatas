module KataTests.Kata2.PeanoNumbersTests

open System
open Katas.Kata2.PeanoNumbers
open Xunit
open FsUnit.Xunit

let peanoFromNumber x =
    if x = 0 then Zero
    else
        List.fold (fun p _ -> Successor p) Zero [1..x]
   
let numberFromPeano =
    let rec toValueRecursive accumulator peano =
        match peano with
        | Successor x ->
            toValueRecursive (accumulator + 1) x
        | Zero ->
            accumulator
    toValueRecursive 0
    
let random = Random()
let anyPositiveInt () =
    random.Next(1, 1000) // big enough to be interesting, small enough not to worry about multiplication overflows
    
let anyPeano () = peanoFromNumber (anyPositiveInt())

let peanoifyInputs peanoFunc leftNumber rightNumber = peanoFunc (peanoFromNumber leftNumber) (peanoFromNumber rightNumber)    

let peanoifyInputsAndOutput peanoFunc leftNumber rightNumber = numberFromPeano (peanoifyInputs peanoFunc leftNumber rightNumber)

let addViaPeano = peanoifyInputsAndOutput add
let subtractViaPeano = peanoifyInputsAndOutput subtract
let cmpViaPeano = peanoifyInputs cmp

let smaller = anyPositiveInt ()
let bigger = smaller + anyPositiveInt ()

[<Fact>]
let ``Round trip some values for sanity checking``() =
    let startingNumber = anyPositiveInt ()
    let peano = peanoFromNumber startingNumber
    let endingNumber = numberFromPeano peano
    endingNumber |> should equal startingNumber
    
[<Fact>]
let ``Add two peanos`` () =
    let actualSum = addViaPeano bigger smaller
    
    actualSum |> should equal (bigger + smaller)

[<Fact>]
let ``subtract two peanos`` () =
    let actualDifference = subtractViaPeano bigger smaller
    
    actualDifference |> should equal (bigger - smaller)
    
[<Fact>]
let ``The zero peano is an additive identity`` () =
    let nonzero = anyPeano ()
    
    (add Zero nonzero) |> should equal nonzero
    (add nonzero Zero) |> should equal nonzero
    (subtract nonzero Zero) |> should equal nonzero
    
[<Fact>]
let ``subtracting into negatives throws an error``() =
    //this kind of test fails in an annoying way in F# when I write it like this.
    (fun () -> (subtractViaPeano smaller bigger) |> ignore) |> should (throwWithMessage "negative number") typeof<Exception>

[<Fact>]
let ``compare equal peanos returns zero``() =
    let peano = anyPeano ()
    
    let result = cmp peano peano
    
    result |> should equal 0

[<Fact>]
let ``cmp returns 1 if left is bigger``() =
    let result = cmpViaPeano bigger smaller
    
    result |> should equal 1

[<Fact>]
let ``cmp returns -1 if right is bigger``() =
    let result = cmpViaPeano smaller bigger
    
    result |> should equal -1