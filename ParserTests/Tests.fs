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