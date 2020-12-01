module Buffer.Gap.Invariants exposing (invariants)

import Array
import Buffer.Gap as Gap
import Buffer.Gap.Fuzz exposing (gap)
import Expect
import Fuzz exposing (char)
import Test exposing (Test, describe, fuzz)


invariants : Test
invariants =
    describe "Editor.Buffer.Gap invariants"
        [ describe "length invariants"
            [ emptyHasLengthZero
            , lengthMatchesToArrayLength
            ]
        , describe "inspection invariants"
            [ toListSameAsToArray
            , foldlAndFoldrReversed
            ]
        ]



-- LENGTH TESTS


emptyHasLengthZero : Test
emptyHasLengthZero =
    fuzz (gap char) "empty buffer always has a zero length" <|
        \buffer ->
            if Gap.isEmpty buffer then
                Expect.equal 0 (Gap.length buffer)

            else
                Expect.greaterThan 0 (Gap.length buffer)


lengthMatchesToArrayLength : Test
lengthMatchesToArrayLength =
    fuzz (gap char) "buffer length equals its array length" <|
        \buffer ->
            Expect.equal (Array.length <| Gap.toArray buffer) (Gap.length buffer)


toListSameAsToArray : Test
toListSameAsToArray =
    fuzz (gap char) "toList and toArray provide the same elements" <|
        \buffer -> Expect.equal (Gap.toArray buffer) (Array.fromList (Gap.toList buffer))


foldlAndFoldrReversed : Test
foldlAndFoldrReversed =
    fuzz (gap char) "foldl and foldr are reversed" <|
        \buffer ->
            Expect.equal
                (List.reverse <| Gap.foldlRange 0 5 (::) [] buffer)
                (Gap.foldrRange 0 5 (::) [] buffer)
