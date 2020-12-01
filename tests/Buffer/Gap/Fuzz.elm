module Buffer.Gap.Fuzz exposing
    ( gap
    , mutation
    )

{-| This module contains some Fuzzing utilities for a gap buffer.


# Builders

@docs gap


# Mutating

@docs mutation

-}

import Buffer.Gap as Gap exposing (Gap)
import Fuzz exposing (Fuzzer)


{-| Returns a Fuzzer that creates a random Gap buffer, and applies random operations on top of it.
There are no guarantees over the position of the gap or the contents of this Fuzzer, so it's a
good candidate for invariants checks.
-}
gap : Fuzzer a -> Fuzzer (Gap a)
gap elem =
    Fuzz.andMap
        (Fuzz.array elem |> Fuzz.map Gap.fromArray)
        (mutation elem)


{-| Returns a Fuzzer that performs some mutations on an existing Gap buffer. This is great to check
that some invariants remain valid even if the data structure is stress tested.
-}
mutation : Fuzzer a -> Fuzzer (Gap a -> Gap a)
mutation elem =
    Fuzz.oneOf
        [ Fuzz.constant Gap.right
        , Fuzz.constant Gap.left
        , Fuzz.map Gap.put elem
        , Fuzz.constant (Gap.delete 1)
        ]
