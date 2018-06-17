namespace AbstractDataTypes

    type private QueueRepresentation<'a> = 
        | Empty 
        | NonEmpty of 'a * QueueRepresentation<'a> 
        with
        member this.enqueue x = 
            match this with
            | Empty -> NonEmpty(x, Empty) 
            | NonEmpty(first, rest) -> NonEmpty(first, rest.enqueue(x))

        member this.dequeue() =
            match this with
            | Empty -> Empty
            | NonEmpty(_, Empty) -> Empty
            | NonEmpty(_, rest) -> rest

        member this.item = 
            match this with
            | Empty -> None
            | NonEmpty(x, _) -> Some x


    type Queue<'a when 'a : equality> private(repr) =
        static member New = Empty |> Queue
        member __.Enqueue(x: 'a) = repr.enqueue x |> Queue
        member __.Dequeue() = repr.dequeue() |> Queue
        member __.Item: 'a Option = repr.item

        member private __.repr = repr
        override __.Equals(obj) =
            match obj with
            | :? Queue<'a> as x -> x.repr.Equals(repr)
            | _ -> false
        override __.GetHashCode() = repr.GetHashCode()
