module Tests

open System
open Xunit

open Parser

module SimpleTests =

    let someStrings = [| "The quick brown fox"; "jumped over"; "the lazy dog." |]
    let someLowerCase = Array.map (fun (s:string) -> s.ToLower()) someStrings
    [<Fact>]
    let someStringTest () =
        let p =
            parse {
                let! (s: string) = pop
                return s.ToLower()
            }
        let ts = new TokenStream<string>(ref someStrings, 0)
        match snd <| runParser (some p) ts () with
        | Result.Ok s ->
            let zipped = Array.zip someLowerCase (Array.ofList s)
            Assert.True(Array.forall (fun (x,y) -> x=y) zipped)
        | Result.Error e ->
            Assert.True(false, e)
    
    type CharParser<'a> = Parser<char, unit, 'a>
    type CharStream = TokenStream<char>

    let chr (c: char) =
        parse {
            let! x = pop
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

    module Regex =
        // Regular expression grammar:
        // <S> -> <regex>*
        // <regex> -> ( <subexpr> | <alternation> | <repitition> | <dot> | <char> )+
        // <subexpr> -> '(' <regex> ')'
        // <alternation> -> <alternation'> '|' <regex>
        // <alternation'> -> ( <subexpr> | <repitition> | <dot> | <char> )+
        // <repitition> -> <repitition'> ( '*' | '+' )
        // <repitition'> -> <subexpr> | <dot> | <char>
        // <dot> -> '.'
        // <char> -> <any unicode character other than '*', '+', '|', '(', ')', or '.'>

        module Matching =
            type Regex = Parser<char, char list, unit>

            let private pushChar (c: char) =
                modify (fun cs -> c::cs)

            let matchChar (c: char) =
                parse {
                    do! ignore <@> chr c
                    do! pushChar c
                }
            let matchAnyChar =
                parse {
                    let! c = pop
                    do! pushChar c
                }
            let matchSequence ps : Regex = ignore <@> sequence ps
            let getMatch =
                parse {
                    let! cs = List.rev <@> get
                    return new string(List.toArray cs)
                }
            let empty : Regex = inject ()

        module Parsing =
            open Matching

            type RegexParser<'a> = Parser<char, unit, 'a>

            let rec S : RegexParser<Regex> =
                matchSequence <@> parseUntil isEOF (repitition <|> subexpr <|> alternation <|> dot <|> regexChr)
            and regex : RegexParser<Regex> =
                matchSequence <@> some (repitition <|> subexpr <|> alternation <|> dot <|> regexChr)
            and subexpr : RegexParser<Regex> =
                parse {
                    do! ignore <@> chr '('
                    let! expr = regex
                    do! ignore <@> (chr ')' <|> abort "Expected ')'")
                    return expr
                }
            and alternation : RegexParser<Regex> =
                let alternation' = matchSequence <@> many (subexpr <|> repitition <|> regexChr <|> dot)
                parse {
                    let! left = alternation' <|> (inject empty)
                    do! ignore <@> chr '|'
                    // If there is another alternative following this one, we need to match it
                    // *before* another parser matches its contents.
                    // E.g. for (a|b|c) we've so far parsed (a|, now we need to parse b|c rather than
                    // parsing b, then |c.  The former parse would look like this:
                    //           |
                    //          / \
                    //         a   |
                    //            / \
                    //           b   c
                    // while the latter parse would look like this: (++ indicates concatenation/sequencing)
                    //            ++
                    //          /    \
                    //         |      |
                    //        / \    / \
                    //       a   b  c  <empty>
                    let! right = alternation <|> alternation' <|> (inject empty)
                    return left <|> right
                }
            and repitition : RegexParser<Regex> =
                parse {
                    let! expr = subexpr <|> dot <|> regexChr
                    match! pop with
                    | '*' -> return ignore <@> some expr
                    | '+' -> return ignore <@> many expr
                    | c -> return! error <| "Expected '*' or '+' but got '" + (string c) + "'."
                }
            and regexChr : RegexParser<Regex> =
                parse {
                    let! c = pop
                    if not(List.contains c [ '('; ')'; '*'; '+'; '.'; '|' ]) then
                        return matchChar c
                    else
                        return! error <| "Expected character literal but got '" + (string c) + "'."
                }
            and dot : RegexParser<Regex> =
                parse {
                    do! ignore <@> chr '.'
                    return matchAnyChar
                }
        
        let compileRegex (regex: string) : Matching.Regex =
            let arr = regex.ToCharArray()
            let (s, result) = runParser Parsing.S (new TokenStream<char>(ref arr, 0)) ()
            match result with
            | Ok compiled -> compiled
            | Error e -> failwith e

        let matchRegex (compiled: Matching.Regex) (s: string) : string option =
            let arr = s.ToCharArray()
            let (_, result) = runParser (bind (fun _ -> Matching.getMatch) compiled) (new TokenStream<char>(ref arr, 0)) []
            match result with
            | Ok matched -> Some matched
            | Error e -> None

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