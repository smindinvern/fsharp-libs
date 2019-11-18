module Tests

open System
open Xunit

open smindinvern.Alternative
open smindinvern.Parser.Types
open smindinvern.Parser.Monad
open smindinvern.Parser.Primitives
open smindinvern.Parser.Combinators

module SimpleTests =

    let someStrings = [| "The quick brown fox"; "jumped over"; "the lazy dog." |]
    let someLowerCase = Array.map (fun (s:string) -> s.ToLower()) someStrings
    [<Fact>]
    let someStringTest () =
        let p =
            parse {
                let! (s: string) = pop1
                return s.ToLower()
            }
        let ts = new TokenStream<string>(ref someStrings, 0)
        match snd <| runParser (some p) ts () with
        | Result.Ok s ->
            let zipped = Array.zip someLowerCase (Array.ofList s)
            Assert.True(Array.forall (fun (x,y) -> x=y) zipped)
        | Result.Error e ->
            Assert.True(false, sprintf "%A" e)
    
    type CharParser<'a> = Parser<char, unit, 'a>
    type CharStream = TokenStream<char>

    let chr (c: char) =
        parse {
            let! x = pop1
            if x = c then
                return c
            else
                return! error ("Expected " + (string c) + " but got " + (string x))
        }
    let digit : CharParser<char> =
        oneOf <| List.map chr [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]
    let listToString xs = new string(Array.ofList(xs))
    let integer : CharParser<int> = 
        parse {
            let! sign = optional (chr '+' <|> chr '-') '+'
            let! digits = many digit
            return Int32.Parse(listToString (sign::digits))
        }
    let real : CharParser<double> =
        parse {
            let! intPart = double <@> integer
            let! dot = chr '.'
            let! digits = many digit
            let fracPart = Double.Parse(listToString (dot::digits))
            return intPart + ((double <| Math.Sign(intPart)) * fracPart)
        }

    open Regex    
    let testRegex = "a(bcdef)*(ef|g)+cd"
    let regexTestMatch = "abcdefefgggefefcd"

    [<Fact>]
    let regexTest () =
        let compiled = compileRegex testRegex
        let matched = matchRegex compiled regexTestMatch
        Assert.True(matched.IsSome)
        Assert.Equal(regexTestMatch, matched.Value)
    
    [<Fact>]
    let invalidRegexTest () =
        let invalidRegex = "a(bc"
        Assert.ThrowsAny<Exception>(fun () -> ignore <| compileRegex invalidRegex)
    
    [<Fact>]
    let regexTest2 () =
        let regex = "a(ab|)c"
        let matching = "ac"
        let matching2 = "aabc"
        let compiled = compileRegex regex
        let matched = matchRegex compiled matching
        let matched2 = matchRegex compiled matching2
        Assert.True(matched.IsSome)
        Assert.True(matched2.IsSome)
        Assert.Equal(matching, matched.Value)
        Assert.Equal(matching2, matched2.Value)
    
    [<Fact>]
    let noMatchTest () =
        let notMatching = "abcdefcd"
        let compiled = compileRegex testRegex
        let matched = matchRegex compiled notMatching
        Assert.False(matched.IsSome)

    [<Fact>]
    let matchTest () =
        let regex = "(0|1|2|3|4|5|6|7|8|9)+"
        let matching = "987654321"
        let compiled = compileRegex regex
        let matched = matchRegex compiled matching
        Assert.True(matched.IsSome)
        Assert.Equal(matching, matched.Value)

    [<Fact>]
    let partialMatchTest () =
        let matching = "abcdefbcdefefgefgcd" + "abcdefghijklmnop"
        let compiled = compileRegex testRegex
        let matched = matchRegex compiled matching
        Assert.True(matched.IsSome)
        Assert.Equal("abcdefbcdefefgefgcd", matched.Value)
