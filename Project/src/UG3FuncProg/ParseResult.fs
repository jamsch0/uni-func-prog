module ParseResult

type ParseResult<'T, 'E> =
    | Okay of 'T
    | Err of 'E
    | FatalErr of 'E

let bind f = function
    | Okay value -> f value
    | Err err -> Err err
    | FatalErr err -> FatalErr err

let map f = function
    | Okay value -> Okay (f value)
    | Err err -> Err err
    | FatalErr err -> FatalErr err

let mapErr f = function
    | Okay value -> Okay value
    | Err err -> Err (f err)
    | FatalErr err -> FatalErr (f err)

let either f g = function
    | Okay value -> f value
    | Err err -> g err
    | FatalErr err -> FatalErr err
