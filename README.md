# elm-gap-buffer

![elm-test](https://github.com/alexandrepiveteau/elm-gap-buffer/workflows/elm-test/badge.svg?branch=master)

An array-based [gap buffer](https://en.wikipedia.org/wiki/Gap_buffer) implementation, based on
the `Array` data structure from `elm/core`.

This library tries to implement gap buffer operations in amortized
[constant time in practice](https://elm-lang.org/news/0.12.1). The backing `Array` of the gap buffer
never doesn't shrink when items are deleted.
