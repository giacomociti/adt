module StackTests

open AbstractDataTypes
open FsCheck.Xunit

type StackTerm<'a> = New | Push of 'a * StackTerm<'a>

let rec stack term =
    match term with
    | New -> Stack.New
    | Push(x, s) -> (stack s).Push(x)


[<Property>]
let equalityAxiom (s: StackTerm<int>) =
    stack s = stack s

[<Property>]
let axiomsForTop (t: StackTerm<int>) =
    let s = stack t
    s.Top =
        match t with
        | New -> None
        | Push(x, _) -> Some x

[<Property>]
let axiomsForPop (t: StackTerm<int>) =
    let s = stack t
    s.Pop() =
        match t with
        | New -> s
        | Push(_, t') -> stack t'