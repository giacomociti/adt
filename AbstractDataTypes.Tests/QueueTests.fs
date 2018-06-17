module QueueTests

open AbstractDataTypes
open FsCheck.Xunit

type QueueTerm<'a> = New | Enqueue of 'a * QueueTerm<'a>
let rec queue term =
    match term with
    | New -> Queue.New
    | Enqueue(x, q) -> (queue q).Enqueue(x)

[<Property>]
let equalityAxiom (s: QueueTerm<int>) =
    queue s = queue s

[<Property>]
let axiomsForItem (t: QueueTerm<int>) =
    let q = queue t
    q.Item = 
        match t with
        | New -> None
        | Enqueue(x, New) -> Some x
        | Enqueue(_, t') -> (queue t').Item

[<Property>]
let axiomsForDequeue (t: QueueTerm<int>) =
    let q = queue t
    q.Dequeue() = 
        match t with
        | New -> q
        | Enqueue(_, New) -> queue New
        | Enqueue(x, t') -> (queue t').Dequeue().Enqueue(x)

