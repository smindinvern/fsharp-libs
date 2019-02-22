module Zipper

    open System

    type Zipper<'a>(xs: ('a []) ref, initial_index: int) =
        member val internal index: int = initial_index with get, set
        member val internal refCell: ('a []) ref = xs with get
        member this.MoveLeft(n: int) =
            if n > this.index then
                raise <| new IndexOutOfRangeException()
            else
                this.index <- this.index - n
        member this.MoveRight(n: int) =
            if (this.index + n) > xs.Value.Length then
                raise <| new IndexOutOfRangeException()
            else
                this.index <- this.index + n
        member this.CurrentValue =
            if this.index = xs.Value.Length then
                raise <| new IndexOutOfRangeException()
            else
                xs.Value.[this.index]
        static member (.>) (z: Zipper<'a>, n: int) : Result<Zipper<'a>, string> =
            if n = 0 then
                Result.Ok z
            else if (z.index + n) > z.refCell.Value.Length then
                Result.Error "Reached end of stream"
            else
                Result.Ok <| new Zipper<'a>(z.refCell, z.index + n)
        static member (<.) (z: Zipper<'a>, n: int) : Result<Zipper<'a>, string> =
            if n = 0 then
                Result.Ok z
            else if n > z.index then
                Result.Error "Reached beginning of stream"
            else
                Result.Ok <| new Zipper<'a>(z.refCell, z.index - n)
        static member get (z: Zipper<'a>) : Result<'a, string> =
            if z.index = z.refCell.Value.Length then
                Result.Error "At end of stream"
            else
                Result.Ok z.refCell.Value.[z.index]
