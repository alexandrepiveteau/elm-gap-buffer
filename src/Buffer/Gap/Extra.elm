module Buffer.Gap.Extra exposing (fromString, toString)

{-| Extra functions for working with a `Gap` buffer.


# Working with String

@docs fromString, toString

-}

import Array
import Buffer.Gap as Gap exposing (Gap)



-- STRINGS


{-| Transform a `String` into a gap buffer of `Char`.
-}
fromString : String -> Gap Char
fromString text =
    Gap.fromArray <| Array.fromList <| String.toList text


{-| Transform a gap buffer of `Char` into a `String`.
-}
toString : Gap Char -> String
toString gap =
    Gap.toArray gap
        |> Array.toList
        |> String.fromList
