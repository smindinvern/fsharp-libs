module Utils

    open System

    let fold' f = function
    | (x::xs) -> List.fold f x xs
    | _ -> raise <| new InvalidOperationException()

    let flip f x y = f y x
    let konst x _ = x
    let cons (head: 'a) (tail: 'a list) = head::tail
    let mkPair x y = (x,y)

    let maybe (def: 'b) (f: 'a -> 'b) (a: 'a option) : 'b =
        Option.fold (fun _ -> f) def a
