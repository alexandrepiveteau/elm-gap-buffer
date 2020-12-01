module Buffer.Gap.Tests exposing (tests)

import Array
import Buffer.Gap as Gap exposing (Gap)
import Buffer.Gap.Extra as Extra
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, fuzz2, test)


tests : Test
tests =
    describe "Edit.Buffer.Gap tests"
        [ emptyGapHasZeroLength
        , describe "one insert"
            [ oneInsertHasGoodLengthFuzz
            , oneInsertHasGoodContentsFuzz
            ]
        , describe "many inserts"
            [ manyInsertHasGoodLengthFuzz
            , manyInsertHasGoodContentsFuzz
            ]
        , describe
            "delete"
            [ deleteKeepsElements
            , deleteInsertedContentMakesEmpty
            , concatThenDelete
            ]
        , describe "moving"
            [ emptyMoveLeft
            , emptyMoveRight
            , simpleMoveRight
            , simpleMoveLeft
            , multipleMoveLeftRight
            , multipleMoveLeftRightWithGrow
            ]
        , describe "folding"
            [ foldrOverAllGap
            , foldrOverSubGap
            ]
        ]



-- empty


emptyGapHasZeroLength : Test
emptyGapHasZeroLength =
    test "empty Gap has zero length" <|
        \_ ->
            Gap.empty
                |> Gap.length
                |> Expect.equal 0



-- insert (one and many)


oneInsertHasGoodLengthFuzz : Test
oneInsertHasGoodLengthFuzz =
    fuzz (Fuzz.array Fuzz.int) "has right length" <|
        \array ->
            let
                gap =
                    Gap.empty
                        |> Gap.insert array
            in
            Expect.equal (Array.length array) (Gap.length gap)


oneInsertHasGoodContentsFuzz : Test
oneInsertHasGoodContentsFuzz =
    fuzz (Fuzz.array Fuzz.int) "has right content" <|
        \array ->
            let
                gap =
                    Gap.empty
                        |> Gap.insert array
            in
            Expect.equal array (Gap.toArray gap)


manyInsertHasGoodLengthFuzz : Test
manyInsertHasGoodLengthFuzz =
    fuzz (Fuzz.array (Fuzz.array Fuzz.int)) "has right length" <|
        \arrays ->
            Array.foldl Gap.insert Gap.empty arrays
                |> Gap.length
                |> Expect.equal
                    (Array.map Array.length arrays
                        |> Array.toList
                        |> List.sum
                    )


manyInsertHasGoodContentsFuzz : Test
manyInsertHasGoodContentsFuzz =
    fuzz (Fuzz.array (Fuzz.array Fuzz.int)) "has right contents" <|
        \arrays ->
            Array.foldl Gap.insert Gap.empty arrays
                |> Gap.toArray
                |> Array.toList
                |> Expect.equal
                    (Array.toList arrays
                        |> List.map Array.toList
                        |> List.concat
                    )



-- delete


deleteKeepsElements : Test
deleteKeepsElements =
    test "delete keeps elements" <|
        \_ ->
            Gap.empty
                |> insertString "content"
                |> Gap.delete 4
                |> Gap.length
                |> Expect.equal 3


deleteInsertedContentMakesEmpty : Test
deleteInsertedContentMakesEmpty =
    fuzz Fuzz.string "delete all content makes empty" <|
        \text ->
            Gap.empty
                |> insertString text
                |> Gap.delete (String.length text)
                |> Gap.length
                |> Expect.equal 0


concatThenDelete : Test
concatThenDelete =
    fuzz2 Fuzz.string Fuzz.string "concat then delete two elements" <|
        \a b ->
            Gap.empty
                |> insertString a
                |> insertString b
                |> Gap.delete (String.length a)
                |> Gap.toArray
                |> Array.toList
                |> String.fromList
                |> Expect.equal (String.slice 0 (String.length b) (a ++ b))



-- STEP-WISE MOVING


emptyMoveLeft : Test
emptyMoveLeft =
    test "moving left on empty buffer has no effect" <|
        \_ ->
            Gap.empty
                |> Gap.left
                |> insertString "hello"
                |> Gap.toArray
                |> Array.toList
                |> String.fromList
                |> Expect.equal "hello"


emptyMoveRight : Test
emptyMoveRight =
    test "moving right on empty buffer has no effect" <|
        \_ ->
            Gap.empty
                |> Gap.right
                |> insertString "hello"
                |> Extra.toString
                |> Expect.equal "hello"


simpleMoveRight : Test
simpleMoveRight =
    test "moving right on simple buffer does nothing" <|
        \_ ->
            Gap.empty
                |> insertString "aaa"
                |> Gap.right
                |> Gap.right
                |> insertString "bbb"
                |> Extra.toString
                |> Expect.equal "aaabbb"


simpleMoveLeft : Test
simpleMoveLeft =
    test "moving left on simple buffer moves input" <|
        \_ ->
            Gap.empty
                |> insertString "aaa"
                |> Gap.left
                |> Gap.left
                |> insertString "bbb"
                |> Extra.toString
                |> Expect.equal "abbbaa"


multipleMoveLeftRight : Test
multipleMoveLeftRight =
    test "moving left then right on buffer moves input" <|
        \_ ->
            Gap.empty
                |> insertString "aaa"
                |> Gap.left
                |> Gap.left
                |> insertString "bbb"
                |> Gap.right
                |> insertString "ccc"
                |> Extra.toString
                |> Expect.equal "abbbaccca"


multipleMoveLeftRightWithGrow : Test
multipleMoveLeftRightWithGrow =
    test "moving left then right on buffer that was grown in-between" <|
        \_ ->
            Gap.empty
                |> insertString "aaa"
                |> Gap.left
                |> Gap.left
                -- Go big or go home.
                |> insertString (String.repeat 500 "c")
                |> Gap.delete 499
                |> Gap.right
                |> insertString "bbb"
                |> Extra.toString
                |> Expect.equal "acabbba"



-- foldr


foldrOverAllGap : Test
foldrOverAllGap =
    test "foldr string accumulation works" <|
        \_ ->
            Extra.fromString "hello"
                |> Gap.foldrRange 0 5 (::) []
                |> Expect.equal [ 'h', 'e', 'l', 'l', 'o' ]


foldrOverSubGap : Test
foldrOverSubGap =
    test "foldr string accumulation works over sub-range" <|
        \_ ->
            Gap.empty
                |> insertString "hello"
                |> Gap.foldrRange 1 4 (::) []
                |> Expect.equal [ 'e', 'l', 'l' ]



-- UTILS


insertString : String -> Gap Char -> Gap Char
insertString string gap =
    Gap.insert (Array.fromList <| String.toList string) gap
