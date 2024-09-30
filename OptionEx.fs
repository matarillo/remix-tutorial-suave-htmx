module OptionEx

type Unwrap = Unwrap with
    static member (@!) (_: Unwrap, x: Option<string>) = Option.defaultValue "" x
    static member (@!) (_: Unwrap, x: Option<bool>) = Option.defaultValue false x

let inline (!!) x = Unwrap @! x

let inline (|||) x y  = Option.orElse x y
