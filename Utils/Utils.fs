module Utils

    open System

    let fold' f = function
    | (x::xs) -> List.fold f x xs
    | _ -> raise <| new InvalidOperationException()

    let inline flip f x y = f y x
    let inline konst x _ = x
    let inline cons (head: 'a) (tail: 'a list) = head::tail
    let mkPair x y = (x,y)

    let inline maybe (def: 'b) (f: 'a -> 'b) (a: 'a option) : 'b =
        Option.fold (fun _ -> f) def a

    let rec intersperse (a: 'a) (xs: 'a list) : 'a list =
        List.foldBack (fun x xs' -> x::a::xs') xs []
