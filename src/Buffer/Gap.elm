module Buffer.Gap exposing
    ( Gap, empty, initialize
    , fromArray, fromList
    , put, insert, delete
    , length, isEmpty, at, cursor
    , foldl, foldr, foldlRange, foldrRange
    , slice, toList, toArray
    , left, right, move
    )

{-| Functions for working with a `Gap` buffer.


# Types

@docs Gap, empty, initialize
@docs fromArray, fromList


# Editing

@docs put, insert, delete


# Inspection

@docs length, isEmpty, at, cursor
@docs foldl, foldr, foldlRange, foldrRange
@docs slice, toList, toArray


# Moving in the buffer

@docs left, right, move

-}

import Array exposing (Array)



-- TYPES


{-| A `Gap` is a [gap buffer][gb] which never shrinks:

    length empty == 0

    length (Extra.fromString "hello") == 5

**Note :** You can do amortized constant-time insertions and deletions at the current cursor
position. Moving the cursor takes a linear time in the amount that it gets moved.

[gb]: https://en.wikipedia.org/wiki/Gap_buffer

-}
type Gap a
    = Gap
        { body : Array a
        , gapStart : Int
        , gapEnd : Int
        }


{-| Creates an empty gap buffer.

    length empty == 0

    toArray empty == Array.empty

-}
empty : Gap a
empty =
    Gap
        { body = Array.empty
        , gapStart = 0
        , gapEnd = 0
        }


{-| Initialize a gap buffer. `initialize n f` creates a buffer of length n with the element at
index `i` initialized to the result of `f i`

    toArray (initialize 4 identity) == Array.fromList [ 0, 1, 2, 3 ]

    toArray (initialize 4 (always 0)) == Array.fromList [ 0, 0, 0, 0 ]

-}
initialize : Int -> (Int -> a) -> Gap a
initialize count func =
    Gap
        { body = Array.initialize count func
        , gapStart = count
        , gapEnd = count
        }


{-| Create a gap buffer from an `Array`.
-}
fromArray : Array a -> Gap a
fromArray array =
    Gap
        { body = array
        , gapStart = Array.length array
        , gapEnd = Array.length array
        }


{-| Create a gap buffer from a `List`.
-}
fromList : List a -> Gap a
fromList elements =
    Array.fromList elements
        |> fromArray



-- INSPECTING


{-| Return true if a gap buffer is empty.

    isEmpty empty == True

-}
isEmpty : Gap a -> Bool
isEmpty buffer =
    length buffer == 0


{-| Return the length of a gap buffer.

    length empty == 0

    length (Extra.fromString "hello") 5

-}
length : Gap a -> Int
length (Gap buffer) =
    Array.length buffer.body - gap (Gap buffer)


{-| Return the cursor position of a gap buffer.

    cursor empty == 0

    cursor (Extra.fromString "hi") == 2

-}
cursor : Gap a -> Int
cursor (Gap buffer) =
    buffer.gapStart


{-| Return the length of the gap.

    gap empty == 0

-}
gap : Gap a -> Int
gap (Gap buffer) =
    buffer.gapEnd - buffer.gapStart


{-| Return `Just` the element at the index or `Nothing` if the index is out of range.

    at 0 (Extra.fromString "hi") == Just 'h'

    at 1 (Extra.fromString "hi") == Just 'i'

    at -1 (Extra.fromString "hi") == Nothing

    at 5 (Extra.fromString "hi") == Nothing

-}
at : Int -> Gap a -> Maybe a
at pos (Gap buffer) =
    if pos < buffer.gapStart then
        Array.get pos buffer.body

    else
        Array.get (pos + gap (Gap buffer)) buffer.body


{-| Reduce a gap from the left over all its contents. Read `foldl` as fold from the left.

    foldl (::) [] (fromList [ 1, 2, 3 ]) == [ 3, 2, 1 ]

-}
foldl : (a -> b -> b) -> b -> Gap a -> b
foldl func acc buffer =
    foldlRange 0 (length buffer) func acc buffer


{-| Reduce a gap from the left over an inclusive-exclusive range. Read `foldl` as fold from the
left.

    foldlRange 1 3 (::) [] (Extra.fromString "abcde") == [ 'c', 'b' ]

-}
foldlRange : Int -> Int -> (a -> b -> b) -> b -> Gap a -> b
foldlRange from until func acc buffer =
    foldlHelp
        (\index -> index + 1)
        from
        (max 0 (until - from))
        func
        acc
        buffer


{-| Reduce a gap from the right over all its contents. Read `foldr` as fold from the right.

    foldr (::) [] (fromList [ 1, 2, 3 ]) == [ 1, 2, 3 ]

-}
foldr : (a -> b -> b) -> b -> Gap a -> b
foldr func acc buffer =
    foldrRange 0 (length buffer) func acc buffer


{-| Reduce a gap from the left over an inclusive-exclusive range. Read `foldr` as fold from the
right.

    foldrRange 1 3 (::) [] (Extra.fromString "abcde") == [ 'b', 'c' ]

-}
foldrRange : Int -> Int -> (a -> b -> b) -> b -> Gap a -> b
foldrRange from until func acc buffer =
    foldlHelp
        (\index -> index - 1)
        (until - 1)
        (max 0 (until - from))
        func
        acc
        buffer


foldlHelp : (Int -> Int) -> Int -> Int -> (a -> b -> b) -> b -> Gap a -> b
foldlHelp step index remaining func acc buffer =
    case remaining of
        0 ->
            acc

        _ ->
            let
                next =
                    at index buffer
                        |> Maybe.map (\elem -> func elem acc)
                        |> Maybe.withDefault acc
            in
            foldlHelp step (step index) (remaining - 1) func next buffer


{-| Get a sub-section of a gap buffer: `(slice start end gap)`. The `start` is a zero-based index
where we will start our slice. The `end` is a zero-based index that indicates the slice end. The
slice extracts up to but not including `end`.

    slice 0 0 empty == Array.empty

    slice 0 3 (Extra.fromString "hello") == Array.fromList [ 'h', 'e', 'l' ]

If `start` or `end` are out of the bounds of the gap buffer, they will default to the closest value
within the bounds of the buffer.

    slice 2 1000 (Extra.fromString "hello") == Array.fromList [ 'l', 'l', 'o' ]

    slice -1 5 (Extra.fromString "hello") == Array.fromList [ 'h', 'e', 'l', 'l', 'o' ]

-}
slice : Int -> Int -> Gap a -> Array a
slice from until buffer =
    foldlRange from until Array.push Array.empty buffer


{-| Create a `List` from the elements of the buffer.

    toList (Extra.fromString "hi") == [ 'h', 'i' ]

-}
toList : Gap a -> List a
toList buffer =
    foldrRange 0 (length buffer) (::) [] buffer


{-| Create an `Array` from the elements of the buffer.

    toArray (Extra.fromString "hi") == Array.fromList [ 'h', 'i' ]

-}
toArray : Gap a -> Array a
toArray buffer =
    slice 0 (length buffer) buffer



-- EDITING


{-| Increases the size of the gap by a certain amount.
-}
grow : Int -> a -> Gap a -> Gap a
grow size default (Gap buffer) =
    let
        elem index =
            if index < buffer.gapStart then
                Array.get index buffer.body

            else if index < buffer.gapEnd + size then
                Nothing

            else
                Array.get (index - size) buffer.body
    in
    Gap
        { body =
            Array.initialize
                (Array.length buffer.body + size)
                (elem >> Maybe.withDefault default)
        , gapStart = buffer.gapStart
        , gapEnd = buffer.gapEnd + size
        }


{-| Add an element onto the cursor of the gap buffer.

    toArray (put 1 empty) == Array.fromList [ 1 ]

    Array.fromList [ 1, 3, 2 ]
        == empty
        |> put 1
        |> put 2
        |> left
        |> put 3
        |> toArray

-}
put : a -> Gap a -> Gap a
put element (Gap buffer) =
    let
        (Gap bigger) =
            if gap (Gap buffer) > 0 then
                Gap buffer

            else
                grow (max 1 (Array.length buffer.body)) element (Gap buffer)
    in
    Gap
        { body = Array.set bigger.gapStart element bigger.body
        , gapStart = bigger.gapStart + 1
        , gapEnd = bigger.gapEnd
        }


{-| Add multiple elements onto the cursor of the gap buffer.

    insert (Array.fromList [ 1, 2, 3 ]) empty == fromArray (Array.fromList [ 1, 2, 3 ])

-}
insert : Array a -> Gap a -> Gap a
insert elements buffer =
    Array.foldl put buffer elements


{-| Delete removes characters from the current cursor position: `delete n gap`. If the cursor is
at the position zero, no items will be removed.
-}
delete : Int -> Gap a -> Gap a
delete count (Gap buffer) =
    Gap
        { body = buffer.body
        , gapStart = max 0 (buffer.gapStart - count)
        , gapEnd = buffer.gapEnd
        }



-- STEP-WISE MOVING


{-| Move the cursor by one index to the left. The index will remain in the bounds of the buffer.
-}
left : Gap a -> Gap a
left (Gap buffer) =
    if buffer.gapStart == 0 then
        Gap buffer

    else
        Gap
            { body =
                Array.get (buffer.gapStart - 1) buffer.body
                    |> Maybe.map (\elem -> Array.set (buffer.gapEnd - 1) elem buffer.body)
                    |> Maybe.withDefault buffer.body
            , gapStart = buffer.gapStart - 1
            , gapEnd = buffer.gapEnd - 1
            }


{-| Move the cursor by one index to the right. The index will remain in the bounds of the buffer.
-}
right : Gap a -> Gap a
right (Gap buffer) =
    if buffer.gapEnd == Array.length buffer.body then
        Gap buffer

    else
        Gap
            { body =
                Array.get buffer.gapEnd buffer.body
                    |> Maybe.map (\elem -> Array.set buffer.gapStart elem buffer.body)
                    |> Maybe.withDefault buffer.body
            , gapStart = buffer.gapStart + 1
            , gapEnd = buffer.gapEnd + 1
            }



-- ARBITRARY MOVING


{-| Move the cursor to the given index using the `left` or `right` functions.
-}
move : Int -> Gap a -> Gap a
move pos (Gap buffer) =
    let
        position =
            clamp 0 (length (Gap buffer)) pos

        delta =
            position - buffer.gapStart
    in
    moveHelp delta (Gap buffer)


moveHelp : Int -> Gap a -> Gap a
moveHelp delta buffer =
    if delta > 0 then
        moveHelp (delta - 1) (right buffer)

    else if delta < 0 then
        moveHelp (delta + 1) (left buffer)

    else
        buffer
