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

[<Fact>]
let ``Round trip some values for sanity checking``() =
    let startingNumber = anyPositiveInt ()
    let peano = peanoFromNumber startingNumber
    let endingNumber = numberFromPeano peano
    endingNumber |> should equal startingNumber
    
[<Fact>]
let ``Add two peanos`` () =
    let left = anyPositiveInt ()
    let right = anyPositiveInt ()
    let expectedSum = left + right
    
    let leftPeano = peanoFromNumber left
    let rightPeano = peanoFromNumber right
    let actualPeanoSum = add leftPeano rightPeano
    let actualSum = numberFromPeano actualPeanoSum
    
    actualSum |> should equal expectedSum

[<Fact>]
let ``subtract two peanos`` () =
    let right = anyPositiveInt ()
    let left = right + anyPositiveInt() //making sure it's positive
    let expectedDifference = left - right
    
    let leftPeano = peanoFromNumber left
    let rightPeano = peanoFromNumber right
    let actualPeanoDifference = subtract leftPeano rightPeano
    let actualDifference = numberFromPeano actualPeanoDifference
    
    actualDifference |> should equal expectedDifference
    
[<Fact>]
let ``The zero peano is an additive identity`` () =
    let nonzero = anyPeano ()
    
    (add Zero nonzero) |> should equal nonzero
    (add nonzero Zero) |> should equal nonzero
    (subtract nonzero Zero) |> should equal nonzero
    
[<Fact>]
let ``subtracting into negatives throws an error``() =
    let left = anyPositiveInt ()
    let right = left + anyPositiveInt() //making sure it's negative
    
    let leftPeano = peanoFromNumber left
    let rightPeano = peanoFromNumber right
    
    //this kind of test fails in an annoying way in F# when I write it like this.
    (fun () -> (subtract leftPeano rightPeano) |> ignore) |> should (throwWithMessage "negative number") typeof<Exception>

[<Fact>]
let ``compare equal peanos returns zero``() =
    let peano = anyPeano ()
    
    let result = cmp peano peano
    
    result |> should equal 0

[<Fact>]
let ``cmp returns 1 if left is bigger``() =
    let right = anyPositiveInt ()
    let left = right + anyPositiveInt ()
    let leftPeano = peanoFromNumber left
    let rightPeano = peanoFromNumber right
    
    let result = cmp leftPeano rightPeano
    
    result |> should equal 1

[<Fact>]
let ``cmp returns -1 if right is bigger``() =
    let left = anyPositiveInt ()
    let right = left + anyPositiveInt ()
    let leftPeano = peanoFromNumber left
    let rightPeano = peanoFromNumber right    
    
    let result = cmp leftPeano rightPeano
    
    result |> should equal -1