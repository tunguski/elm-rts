module RTS.Rng exposing
    ( Seed
    , seed
    , step
    , int
    , range
    , chance
    , pick
    , shuffle
    )

{-| A tiny deterministic pseudo-random number generator (a 32-bit linear congruential generator).

The project's `elm.json` does not depend on `elm/random`, and the whole game must be reproducible for
tests, so randomness is its own pure module: a `Seed` is just an `Int`, every draw returns the value
*and* the next seed, and the same starting seed always produces the same map and the same AI
tie-breaks. Threading the seed by hand (rather than hiding it in a `Generator` monad) keeps the
interpreter-friendly code small and the data trivially inspectable in tests.
-}


{-| The generator state. Opaque in spirit, but it is just an `Int` so callers can persist it in the
model and tests can pin it to a literal. -}
type alias Seed =
    Int


{-| Normalise an arbitrary integer into a usable seed (kept in the LCG's 32-bit range, never zero). -}
seed : Int -> Seed
seed n =
    let
        s =
            modBy 4294967296 (abs n)
    in
    if s == 0 then
        2463534242

    else
        s


{-| Advance the state once (Numerical-Recipes LCG constants, modulo 2^32). -}
step : Seed -> Seed
step s =
    modBy 4294967296 (1664525 * s + 1013904223)


{-| A non-negative integer in `[0, n)` and the next seed. The *high* bits of the state are used
(the low bits of an LCG cycle short), so successive small draws are well spread. -}
int : Int -> Seed -> ( Int, Seed )
int n s =
    if n <= 1 then
        ( 0, step s )

    else
        let
            s2 =
                step s
        in
        ( modBy n (s2 // 256), s2 )


{-| An integer in the inclusive range `[lo, hi]` and the next seed. -}
range : Int -> Int -> Seed -> ( Int, Seed )
range lo hi s =
    if hi <= lo then
        ( lo, step s )

    else
        let
            ( d, s2 ) =
                int (hi - lo + 1) s
        in
        ( lo + d, s2 )


{-| `True` with probability `p` (a percentage in `[0, 100]`), and the next seed. -}
chance : Int -> Seed -> ( Bool, Seed )
chance p s =
    let
        ( r, s2 ) =
            int 100 s
    in
    ( r < p, s2 )


{-| Pick a random element of a list (with its index dropped), returning the default for an empty
list. Also returns the next seed. -}
pick : a -> List a -> Seed -> ( a, Seed )
pick default xs s =
    case xs of
        [] ->
            ( default, step s )

        _ ->
            let
                ( i, s2 ) =
                    int (List.length xs) s
            in
            ( nth i default xs, s2 )


{-| A Fisher–Yates shuffle of the list, plus the next seed. -}
shuffle : List a -> Seed -> ( List a, Seed )
shuffle xs s =
    shuffleHelp xs [] s


shuffleHelp : List a -> List a -> Seed -> ( List a, Seed )
shuffleHelp remaining acc s =
    case remaining of
        [] ->
            ( acc, s )

        head :: _ ->
            let
                ( i, s2 ) =
                    int (List.length remaining) s

                picked =
                    nth i head remaining
            in
            shuffleHelp (removeAt i remaining) (picked :: acc) s2


nth : Int -> a -> List a -> a
nth i default xs =
    case List.head (List.drop i xs) of
        Just x ->
            x

        Nothing ->
            default


removeAt : Int -> List a -> List a
removeAt i xs =
    List.take i xs ++ List.drop (i + 1) xs
