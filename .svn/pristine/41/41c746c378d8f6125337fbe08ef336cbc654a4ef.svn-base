namespace AutoCaptureMobile.Common

[<RequireQualifiedAccess>]
[<AutoOpen>]
module Result =

    let bimap onSuccess onError xR = 
        match xR with
        | Ok x -> onSuccess x
        | Error err -> onError err

    let map f result = 
        match result with
        | Ok success -> Ok (f success)
        | Error err -> Error err 

    let mapError f result = 
        match result with
        | Ok success -> Ok success
        | Error err -> Error (f err)

    let bind f result = 
        match result with
        | Ok success -> f success
        | Error err -> Error err
        
    let iter (f : _ -> unit) result = 
        map f result |> ignore    

    let apply fR xR = 
        match fR, xR with
        | Ok f, Ok x -> Ok (f x)
        | Error err1, Ok _ -> Error err1
        | Ok _, Error err2 -> Error err2
        | Error err1, Error _ -> Error err1 
        
    let sequence aListOfResults = 
        let (<*>) = apply
        let (<!>) = map
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok []
 
        List.foldBack consR aListOfResults initialValue
    
    let lift1 f x1 = 
        let (<!>) = map
        f <!> x1

    let lift2 f x1 x2 = 
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2
        
    let lift3 f x1 x2 x3 = 
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3

    let lift4 f x1 x2 x3 x4 = 
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3 <*> x4

    let bind2 f x1 x2 = lift2 f x1 x2 |> bind id

    let bind3 f x1 x2 x3 = lift3 f x1 x2 x3 |> bind id

    let isOk = 
        function 
        | Ok _ -> true
        | Error _ -> false

    let isError xR = 
        xR |> isOk |> not

    let filter pred = 
        function 
        | Ok x -> pred x
        | Error _ -> true

    let ifError defaultVal = 
        function 
        | Ok x -> x
        | Error _ -> defaultVal

    let bindOption f xR =
        match xR with
        | Some x -> f x |> map Some
        | None -> Ok None

    let ofOption errorValue opt = 
        match opt with
        | Some v -> Ok v
        | None -> Error errorValue

    let toOption xR = 
        match xR with
        | Ok v -> Some v
        | Error _ -> None
        
    let toErrorOption = 
        function 
        | Ok _ -> None
        | Error err -> Some err

    let internal (<*>) = 
        apply

[<AutoOpen>]
module ResultComputationExpression =

    type ResultBuilder() =
        member __.Return(x) = Ok x
        member __.Bind(x, f) = Result.bind f x
    
        member __.ReturnFrom(x) = x
        member this.Zero() = this.Return ()

        member __.Delay(f) = f
        member __.Run(f) = f()

        member this.While(guard, body) =
            if not (guard()) 
            then this.Zero() 
            else this.Bind( body(), fun () -> 
                this.While(guard, body))  

        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation() 

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () -> 
                match disposable with 
                    | null -> () 
                    | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),fun enum -> 
                this.While(enum.MoveNext, 
                    this.Delay(fun () -> body enum.Current)))

        member this.Combine (a,b) = 
            this.Bind(a, fun () -> b())

    let result = new ResultBuilder()

[<AutoOpen>]
module ResultMessage =

    type Message = Message of string
    
    let error str =
        Error (Message str)

    let ok = Ok

[<AutoOpen>]
module ResultValidation =

    let validateStringType instance pattern input =
        let typeName = 
            instance.GetType().Name
        if isNullOrEmpty(input) then
            let message = sprintf "%s must not be null or empty" typeName
            error  message 
        elif isMatch pattern input then
            ok instance
        else
            let message = sprintf "%s '%s' must match the pattern '%s'" typeName input pattern
            error message