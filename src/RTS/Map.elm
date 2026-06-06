module RTS.Map exposing (Generated, generate, startPositions)

{-| The seeded map generator. Given a map size, a player count and a `Rng.Seed`, it produces a fresh
terrain grid plus one starting position per player. The same seed always yields the same map, so the
setup screen's "Reroll" button is just "pick a different seed" and the tests can pin a layout.

The generator: fill the board with grass, grow a handful of random blobs of water, rock and forest
(random-walk flood fills), scatter a few gold mines, then *carve* each player's start — guaranteeing
a clear, buildable clearing and a nearby gold mine and forest so every player has an economy. Blobs
never overwrite the protected zone around a start, so no one is walled in.
-}

import Dict exposing (Dict)
import RTS.Model exposing (..)
import RTS.Rng as Rng


{-| The result of a generation pass: dimensions, the row-major tile list, the per-player start tiles,
and the seed left over for whatever the caller does next. -}
type alias Generated =
    { width : Int
    , height : Int
    , tiles : List Tile
    , starts : List ( Int, Int )
    , seed : Int
    }


{-| Generate a map of the given size for `playerCount` players (1–3). -}
generate : MapSize -> Int -> Rng.Seed -> Generated
generate size playerCount s0 =
    let
        ( width, height ) =
            sizeDims size

        area =
            width * height

        starts =
            startPositions playerCount width height

        protected pos =
            List.any (\st -> chebyshev pos st <= 2) starts

        -- Grow terrain blobs onto an empty (all-grass) board. Counts/sizes scale with the board area.
        ( withWater, s1 ) =
            scatter (max 2 (area // 170)) (6 + area // 220) Water protected width height Dict.empty s0

        ( withRock, s2 ) =
            scatter (max 3 (area // 110)) (3 + area // 500) Rock protected width height withWater s1

        ( withForest, s3 ) =
            scatter (max 3 (area // 95)) (4 + area // 320) Forest protected width height withRock s2

        ( withMines, s4 ) =
            scatterMines (max 2 (playerCount + area // 320)) protected width height withForest s3

        -- Carve every start: clearing + a guaranteed mine and forest beside it.
        carved =
            List.foldl (carveStart width height ( width // 2, height // 2 )) withMines starts

        tiles =
            List.concatMap
                (\y ->
                    List.map
                        (\x ->
                            { x = x
                            , y = y
                            , terrain = Maybe.withDefault Grass (Dict.get ( x, y ) carved)
                            , visible = False
                            }
                        )
                        (List.range 0 (width - 1))
                )
                (List.range 0 (height - 1))
    in
    { width = width, height = height, tiles = tiles, starts = starts, seed = s4 }


{-| Spread-out start tiles for 1–3 players: a corner each, opposite corners first so duels start far
apart. The human is always the first entry. -}
startPositions : Int -> Int -> Int -> List ( Int, Int )
startPositions playerCount width height =
    let
        inset =
            3

        tl =
            ( inset, inset )

        br =
            ( width - 1 - inset, height - 1 - inset )

        tr =
            ( width - 1 - inset, inset )
    in
    case playerCount of
        1 ->
            [ tl ]

        2 ->
            [ tl, br ]

        _ ->
            [ tl, br, tr ]



-- BLOBS ----------------------------------------------------------------------------------------


{-| Drop `count` random-walk blobs of `terrain`, each about `blobSize` cells, never touching a
protected (start-adjacent) cell. -}
scatter : Int -> Int -> Terrain -> (( Int, Int ) -> Bool) -> Int -> Int -> Dict ( Int, Int ) Terrain -> Rng.Seed -> ( Dict ( Int, Int ) Terrain, Rng.Seed )
scatter count blobSize terrain protected width height grid s =
    if count <= 0 then
        ( grid, s )

    else
        let
            ( cx, s1 ) =
                Rng.range 0 (width - 1) s

            ( cy, s2 ) =
                Rng.range 0 (height - 1) s1

            ( grown, s3 ) =
                growBlob ( cx, cy ) blobSize terrain protected width height grid s2
        in
        scatter (count - 1) blobSize terrain protected width height grown s3


{-| A bounded random walk from `start` that paints up to `n` in-bounds, unprotected cells `terrain`. -}
growBlob : ( Int, Int ) -> Int -> Terrain -> (( Int, Int ) -> Bool) -> Int -> Int -> Dict ( Int, Int ) Terrain -> Rng.Seed -> ( Dict ( Int, Int ) Terrain, Rng.Seed )
growBlob pos n terrain protected width height grid s =
    if n <= 0 then
        ( grid, s )

    else
        let
            ( x, y ) =
                pos

            grid2 =
                if inBounds width height pos && not (protected pos) then
                    Dict.insert pos terrain grid

                else
                    grid

            ( dir, s2 ) =
                Rng.range 0 3 s

            next =
                case dir of
                    0 ->
                        ( x + 1, y )

                    1 ->
                        ( x - 1, y )

                    2 ->
                        ( x, y + 1 )

                    _ ->
                        ( x, y - 1 )

            -- Stay on the board; if the walk would leave it, bounce off the edge.
            stepped =
                if inBounds width height next then
                    next

                else
                    pos
        in
        growBlob stepped (n - 1) terrain protected width height grid2 s2


{-| Scatter `count` lone gold mines onto grass cells, away from protected zones. -}
scatterMines : Int -> (( Int, Int ) -> Bool) -> Int -> Int -> Dict ( Int, Int ) Terrain -> Rng.Seed -> ( Dict ( Int, Int ) Terrain, Rng.Seed )
scatterMines count protected width height grid s =
    if count <= 0 then
        ( grid, s )

    else
        let
            ( x, s1 ) =
                Rng.range 0 (width - 1) s

            ( y, s2 ) =
                Rng.range 0 (height - 1) s1

            pos =
                ( x, y )

            grid2 =
                if not (protected pos) && Dict.get pos grid == Nothing then
                    -- only on bare grass (an unset cell)
                    Dict.insert pos GoldMine grid

                else
                    grid
        in
        scatterMines (count - 1) protected width height grid2 s2



-- CARVING STARTS -------------------------------------------------------------------------------


{-| Make a start buildable: clear a 3×3 grass clearing, then place a gold mine and a forest just
outside it (offset toward the map centre so they always land in bounds). -}
carveStart : Int -> Int -> ( Int, Int ) -> ( Int, Int ) -> Dict ( Int, Int ) Terrain -> Dict ( Int, Int ) Terrain
carveStart width height center start grid =
    let
        ( sx, sy ) =
            start

        ( dx, dy ) =
            towardCenter start center

        clearing =
            List.concatMap (\oy -> List.map (\ox -> ( sx + ox, sy + oy )) (List.range -1 1)) (List.range -1 1)

        cleared =
            List.foldl Dict.remove grid (List.filter (inBounds width height) clearing)

        -- Distinct offsets (starts are always corners, so dx and dy are both non-zero).
        minePos =
            ( sx + dx * 3, sy + dy )

        forestPos =
            ( sx + dx, sy + dy * 3 )
    in
    cleared
        |> placeIfIn width height minePos GoldMine
        |> placeIfIn width height forestPos Forest


placeIfIn : Int -> Int -> ( Int, Int ) -> Terrain -> Dict ( Int, Int ) Terrain -> Dict ( Int, Int ) Terrain
placeIfIn width height pos terrain grid =
    if inBounds width height pos then
        Dict.insert pos terrain grid

    else
        grid


{-| A unit-ish step (each component −1/0/1) pointing from `pos` toward `center`. -}
towardCenter : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
towardCenter ( px, py ) ( cx, cy ) =
    ( signOf (cx - px), signOf (cy - py) )


signOf : Int -> Int
signOf n =
    if n > 0 then
        1

    else if n < 0 then
        -1

    else
        0


inBounds : Int -> Int -> ( Int, Int ) -> Bool
inBounds width height ( x, y ) =
    x >= 0 && x < width && y >= 0 && y < height


chebyshev : ( Int, Int ) -> ( Int, Int ) -> Int
chebyshev ( ax, ay ) ( bx, by ) =
    max (abs (ax - bx)) (abs (ay - by))
