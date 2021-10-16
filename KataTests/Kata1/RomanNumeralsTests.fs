module KataTests.Kata1.RomanNumeralsTests

open System
open Katas.Kata1
open Xunit
open FsUnit.Xunit

type RomanNumeralPair = {StringForm:string; NumericForm:int}

type RomanNumeralTests() = 
    static member ValidNumeralPairs
        with get() =
            [|
                {StringForm ="I"; NumericForm =  1}
                {StringForm ="II"; NumericForm =  2}
                {StringForm ="III"; NumericForm =  3}
                {StringForm ="IV"; NumericForm =  4}
                {StringForm ="V"; NumericForm =  5}
                {StringForm ="VIII"; NumericForm =  8}
                {StringForm ="IX"; NumericForm =  9}
                {StringForm ="X"; NumericForm =  10}
                {StringForm ="XIV"; NumericForm =  14}
                {StringForm ="XVII"; NumericForm =  17}
                {StringForm ="XXXIX"; NumericForm =  39}
                {StringForm ="XLIX"; NumericForm =  49}
                {StringForm ="XLVII"; NumericForm =  47}
                {StringForm ="XCIV"; NumericForm =  94}
                {StringForm ="CCXLIV"; NumericForm =  244}
                {StringForm ="MCMLXXX"; NumericForm =  1980}
                {StringForm ="MMXIX"; NumericForm =  2019}
                {StringForm ="CDII"; NumericForm =  402}
                {StringForm ="DXXXVII"; NumericForm =  537}                
            |]
            |> Array.map (fun x-> [|x|]) // this makes xunit happy

    static member NumbersExpressableInRomanNumerals 
        with get() :Object[][] = [|1..3999|]
                     |> Array.map (fun x-> [|x|]) // this makes xunit happy

    [<Theory>]
    [<MemberData("ValidNumeralPairs")>]
    member this.``Valid roman numerals give correct results`` (testCase:RomanNumeralPair) =
        let actual = RomanNumerals.fromRomanNumeral testCase.StringForm
        actual |> should equal testCase.NumericForm

    [<Theory>]
    [<MemberData("ValidNumeralPairs")>]
    member this.``Numbers are transformed into correct roman numerals`` (testCase:RomanNumeralPair) =
        let actual = RomanNumerals.toRomanNumeral testCase.NumericForm
        actual |> should equal testCase.StringForm

    [<Theory>]
    [<MemberData("NumbersExpressableInRomanNumerals")>]
    member this.``Arbitrary ints round trip correctly`` (arbitrary:int) =
        let actual = RomanNumerals.fromRomanNumeral (RomanNumerals.toRomanNumeral arbitrary)
        actual |> should equal arbitrary

    [<Fact>]
    member this.``Invalid cases that shouldn't work, but currently do``() =
        let strangeSeven = RomanNumerals.fromRomanNumeral "IIIIIII"
        strangeSeven |> should equal 7
        
        let invalidTwentyNineHundred = RomanNumerals.fromRomanNumeral "DDMCM"
        invalidTwentyNineHundred |> should equal 2900
        
        let weirdAdditionToThirteen = RomanNumerals.fromRomanNumeral "IVIX"
        weirdAdditionToThirteen |> should equal 13