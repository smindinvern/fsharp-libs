namespace smindinvern.Parser

open smindinvern.Utils
open smindinvern.Zipper

open Types

open smindinvern.Alternative
open smindinvern.Alternative.Monad

module Primitives =

    let inline get<'T, 'U> : Parser<'T, 'U, 'U> =
        smindinvern.Alternative.get
        >>= (fun (_, u) -> Monad.inject u)
    let inline modify (f: 'U -> 'U) : Parser<'T, 'U, unit> =
        smindinvern.Alternative.modify (fun (t, u) -> (t, f u))
        >>= Monad.inject
    let put (u: 'U) : Parser<'T, 'U, unit> =
        modify (konst u)

    let inline private getTokenStream<'T, 'U> : Parser<'T, 'U, TokenStream<'T>> =
        smindinvern.Alternative.get
        >>= fun (ts, _) -> Monad.inject ts
    let inline private putTokenStream<'T, 'U> (ts: TokenStream<'T>) : Parser<'T, 'U, unit> =
        smindinvern.Alternative.modify (fun (_, u) -> (ts, u))
        >>= Monad.inject

    module Results =
        open smindinvern.Alternative.Errors
        let liftResult r = liftResult <| Result.mapError (fun e -> { Tree.Value = e; Tree.Children = [] }) r
    
    open Results
    open Monad
    
    open smindinvern.Utils
    
    /// <summary>
    /// Return the next n tokens in the stream without consuming them.
    ///
    /// Produces an error if there are fewer than n tokens are remaining in the stream.
    /// </summary>
    /// <param name="n">The number of tokens to return.</param>
    let peek<'T, 'U> (n: int) : Parser<'T, 'U, 'T list> =
        let getToken zr tokens =
            let consToList (t: 'T) =
                Result.map (List.cons t) tokens
            let tr = Result.bind Zipper.get zr
            Result.bind consToList tr
        parse {
            if n = 0 then
                return []
            else
            let! ts = getTokenStream
            let zippers = [ for i in 0..(n-1) -> (ts .> i) ]
            let toks = List.foldBack getToken zippers (Result.Ok [])
            return! liftResult toks
        }

    /// <summary>
    /// Return the next token in the stream without consuming it.
    ///
    /// Produces an error if there are no more tokens in the stream.
    /// </summary>
    /// <remarks>Equivalent to <c>peek 1</c>.</remarks>
    /// <seealso cref="peek"/>
    let inline peek1<'T, 'U> : Parser<'T, 'U, 'T> =
        getTokenStream
        >>= (liftResult << Zipper.get)

    /// <summary>
    /// Consume n tokens and discard their values.
    ///
    /// Produces an error if there are fewer than n tokens remaining in the stream.
    /// </summary>
    /// <param name="n">The number of tokens to discard.</param>
    let discard<'T, 'U> (n: int) : Parser<'T, 'U, unit> =
        parse {
            let! ts = getTokenStream
            let! ts' = liftResult (ts .> n)
            return! putTokenStream ts'
        }

    /// <summary>
    /// Consume a token and discard its value.
    ///
    /// Produces an error if there are no more tokens remaining in the stream.
    /// </summary>
    /// <remarks>Equivalent to <c>discard 1</c>.</remarks>
    /// <seealso cref="discard"/>
    let inline discard1<'T, 'U> : Parser<'T, 'U, unit> =
        discard 1

    /// <summary>
    /// Consume the next n tokens in the stream and return their values.
    ///
    /// Produces an error if there are fewer than n tokens in the stream.
    /// </summary>
    /// <param name="n">The number of tokens to pop.</param>
    let rec pop<'T, 'U> (n: int) : Parser<'T, 'U, 'T list> =
        parse {
            let! tokens = peek n
            do! discard n
            return tokens
        }

    /// <summary>
    /// Consume the next token in the stream and return its value.
    ///
    /// Produces an error if there are no more tokens in the stream.
    /// </summary>
    /// <remarks>Equivalent to <c>pop 1</c>.</remarks>
    /// <seealso cref="pop"/>
    let inline pop1<'T, 'U> : Parser<'T, 'U, 'T> =
        parse {
            let! ts = getTokenStream
            let! token = liftResult <| Zipper.get ts
            let! ts' = liftResult (ts .> 1)
            do! putTokenStream ts'
            return token
        }

    /// <summary>
    /// Rewind the stream by n tokens.
    ///
    /// Produces an error if the stream cannot be rewound by n, i.e. if fewer than
    /// n tokens have been consumed so far.
    /// </summary>
    /// <param name="n">The number of tokens to rewind.</param>
    let rewind (n: int) : Parser<'T, 'U, unit> =
        parse {
            let! ts = getTokenStream
            let! ts' = liftResult (ts .> n)
            return! putTokenStream ts'
        }

    /// <summary>
    /// Rewind the stream by one token.
    ///
    /// Produces an error if the stream is already at its initial position.
    /// </summary>
    /// <remarks>Equivalent to <c>rewind 1</c>.</remarks>
    /// <seealso cref="rewind"/>
    let rewind1<'T, 'U> : Parser<'T, 'U, unit> =
        rewind 1

    /// <summary>
    /// Run a parser on a given token stream and return the result.
    /// </summary>
    /// <param name="m">The parser to run.</param>
    /// <param name="s">The token stream to run it on.</param>
    let runParser (m: Parser<'s, 'u, 'a>) (s: TokenStream<'s>) (u: 'u) =
        smindinvern.State.runState (m.Force()) ((s, u), false)

    type CharParser<'u, 'a> = Parser<char, 'u, 'a>
    type StringParser<'u, 'a> = Parser<string, 'u, 'a>

    module LineInfo =

        type Parser<'s, 'u, 'a> = Types.Parser<'s*int, 'u, 'a>
        let inline lineNo<'T, 'U> : Parser<'T, 'U, int> =
            snd <@> peek1
        // These definitions shadow some of those given above.
        let inline peek1<'T, 'U> : Parser<'T, 'U, 'T> =
            fst <@> peek1
        let inline pop1<'T, 'U> : Parser<'T, 'U, 'T> =
            fst <@> pop1
        let inline error (message: string) : Parser<'s, 'u, 'a> =
            parse {
                let! line_no = lineNo<'s, 'u>
                return! error (message + " on line " + (string line_no))
            }
        let inline abort (msg: string) : Parser<'s, 'u, 'a> =
            parse {
                let! line_no = lineNo<'s, 'u>
                return! abort (msg + " on line " + (string line_no))
            }

        type StringParser<'u, 'a> = Parser<string, 'u, 'a>

open Primitives
open System.IO

type Tokenization =
    static member Tokenize(stream: 't []) : TokenStream<'t> =
        new Zipper<'t>(ref stream, 0)
    static member Tokenize(stream: 't list) : TokenStream<'t> =
        Tokenization.Tokenize(Array.ofList stream)
    static member Tokenize(getter: 's -> ('s * 't option), stream: 's) : TokenStream<'t> =
        let rec tailRec (stream: 's) (ts : 't list) = 
            let (stream', o) = getter stream
            match o with
                | Option.Some(t) -> tailRec stream' (t::ts)
                | Option.None -> List.rev ts
        Tokenization.Tokenize(tailRec stream [])
    static member TokenizeString(s: string) : TokenStream<char> =
        let arr = s.ToCharArray()
        new Zipper<char>(ref arr, 0)
    static member TokenizeFile(sr: StreamReader) : TokenStream<char> =
        Tokenization.TokenizeString(sr.ReadToEnd())
    static member TokenizeFile(getToken: StreamReader -> 't option, fs: StreamReader) =
        Tokenization.Tokenize((fun x -> (x, getToken x)), fs)
    static member TokenizeFile(getToken: StreamReader -> 't option, fp: string) =
        using (File.OpenText(fp)) (fun sr -> Tokenization.TokenizeFile(getToken, sr))
    static member TokenizeFile(fp: string) : TokenStream<char> =
        using (File.OpenText(fp)) Tokenization.TokenizeFile

module LineInfo =

    type Tokenization =
        static member TokenizeString(s: string) : TokenStream<char * int> =
            let getter ((z, i): Zipper<char> * int) : (Zipper<char> * int) * (char * int) option =
                match Zipper.get z with
                    | Result.Ok(c) ->
                        let i' = if c = '\n' then i else i + 1
                        z.MoveRight(1)
                        ((z, i'), Option.Some(c, i))
                    | Result.Error(_) -> ((z, i), Option.None)
            let arr = s.ToCharArray()
            Tokenization.Tokenize(getter, (new Zipper<char>(ref arr, 0), 1))
        static member TokenizeFile(sr: StreamReader) : TokenStream<char * int> =
            Tokenization.TokenizeString(sr.ReadToEnd())
        static member TokenizeFile(getToken: StreamReader -> ('t * int) option, fs: StreamReader) =
            Tokenization.Tokenize((fun x -> (x, getToken x)), fs)
        static member TokenizeFile(getToken: StreamReader -> ('t * int) option, fp: string) =
            using (File.OpenText(fp)) (fun sr -> Tokenization.TokenizeFile(getToken, sr))
        static member TokenizeFile(fp: string) : TokenStream<char * int> =
            using (File.OpenText(fp)) Tokenization.TokenizeFile
    

