module KataTests.Kata1.RomanNumeralsTests

open Katas.Kata1
open Xunit
open FsUnit.Xunit

[<Theory>]
[<InlineData("I", 1)>]
[<InlineData("II", 2)>]
[<InlineData("III", 3)>]
[<InlineData("IV", 4)>]
[<InlineData("V", 5)>]
[<InlineData("VIII", 8)>]
[<InlineData("IX", 9)>]
[<InlineData("X", 10)>]
[<InlineData("XIV", 14)>]
[<InlineData("XVII", 17)>]
[<InlineData("XXXIX", 39)>]
[<InlineData("XLIX", 49)>]
[<InlineData("XLVII", 47)>]
[<InlineData("XCIV", 94)>]
[<InlineData("CCXLIV", 244)>]
[<InlineData("MCMLXXX", 1980)>]
[<InlineData("MMXIX", 2019)>]
[<InlineData("CDII", 402)>]
[<InlineData("DXXXVII", 537)>]
let ``Valid roman numerals give correct results`` input expected =
    let actual = RomanNumerals.fromRomanNumeral input
    actual |> should equal expected