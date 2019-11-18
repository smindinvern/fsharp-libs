namespace smindinvern

module Utils =

    open System

    module List =
        let fold' f = function
            | (x::xs) -> List.fold f x xs
            | _ -> raise <| new InvalidOperationException()

        let inline cons (head: 'a) (tail: 'a list) = head::tail    
    
    module Seq =
        let uncons xs =
            if Seq.isEmpty xs then
                raise <| InvalidOperationException()
            else
                (Seq.head xs, Seq.tail xs)
        
        let fold' f xs =
            let (head, tail) = uncons xs
            Seq.fold f head tail
        
    let inline flip f x y = f y x
    let inline konst x _ = x
    let mkPair x y = (x,y)

    let inline maybe (def: 'b) (f: 'a -> 'b) (a: 'a option) : 'b =
        Option.fold (fun _ -> f) def a

    let rec intersperse (a: 'a) (xs: 'a list) : 'a list =
        List.foldBack (fun x xs' -> x::a::xs') xs []
