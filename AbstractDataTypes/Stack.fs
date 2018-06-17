namespace AbstractDataTypes

    type private StackRepresentation<'a> = 
        | New 
        | Push of 'a * StackRepresentation<'a>
        with
        member this.push x = Push(x, this)
        member this.pop() = 
            match this with
            | New -> New
            | Push(_, s) -> s
        member this.item =
            match this with
                | New -> None
                | Push(x, _) -> Some x
        


    type Stack<'a when 'a : equality> private(repr) =

        static member New = New |> Stack

        member __.Push(x: 'a) = repr.push x |> Stack

        member __.Pop() = repr.pop() |> Stack

        member __.Top: 'a Option = repr.item
            
            
        member private __.repr = repr
        override __.Equals(obj) =
            match obj with
            | :? Stack<'a> as x -> x.repr.Equals(repr)
            | _ -> false
        override __.GetHashCode() = repr.GetHashCode()
