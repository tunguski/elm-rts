module RTS.Ai exposing (step, aiPeriod)

{-| The opponent AI: a plain, deterministic algorithm (no randomness, no LLM). Each AI player acts on
a staggered cadence — assign idle workers to the nearest resource, follow a simple build order
(workers → a barracks → soldiers, with a farm for extra income), and once it has mustered an army,
push it at the nearest enemy. Defense is emergent: freshly trained and returning soldiers sit idle at
home and auto-engage anything that comes within range (see `RTS.Logic.autoAcquire`).

It builds on the owner-parameterised helpers in `RTS.Logic`, so an AI "decision" is just a
`Model -> Model` transformation, exactly like a player's command — which makes the whole opponent
testable without a browser.
-}

import RTS.Logic as Logic
import RTS.Model exposing (..)


{-| How many ticks between an AI player's decisions (staggered by player id so they don't all act on
the same tick). -}
aiPeriod : Int
aiPeriod =
    12


workerTarget : Int
workerTarget =
    6


armyTarget : Int
armyTarget =
    9


{-| Muster at least this many soldiers before launching an attack. -}
attackThreshold : Int
attackThreshold =
    5


{-| Run every AI player's decision that is due this tick. -}
step : Model -> Model
step model =
    List.foldl turn model (duePlayers model)


{-| The AI player ids whose turn falls on this tick (alive, non-human, correct cadence slot). -}
duePlayers : Model -> List PlayerId
duePlayers model =
    List.filterMap
        (\p ->
            if not p.isHuman && not p.defeated && modBy aiPeriod model.tick == modBy aiPeriod p.id then
                Just p.id

            else
                Nothing
        )
        (List.sortBy .id model.players)


turn : PlayerId -> Model -> Model
turn pid model =
    model
        |> assignIdleWorkers pid
        |> economyAction pid
        |> militaryAction pid



-- ECONOMY --------------------------------------------------------------------------------------


{-| Send every idle AI worker to gather the nearest resource. -}
assignIdleWorkers : PlayerId -> Model -> Model
assignIdleWorkers pid model =
    let
        resources =
            resourceTiles model
    in
    { model
        | units =
            List.map
                (\u ->
                    if u.owner == pid && u.kind == Worker && u.assigned == Nothing && List.isEmpty u.path then
                        case nearestTile ( round u.x, round u.y ) resources of
                            Just ( rx, ry ) ->
                                { u
                                    | assigned = Just ( rx, ry )
                                    , tx = toFloat rx
                                    , ty = toFloat ry
                                    , path = Logic.findPath model ( round u.x, round u.y ) ( rx, ry )
                                }

                            Nothing ->
                                u

                    else
                        u
                )
                model.units
    }


{-| The single highest-priority build/train action this turn, following a fixed build order. -}
economyAction : PlayerId -> Model -> Model
economyAction pid model =
    case Logic.player pid model of
        Nothing ->
            model

        Just p ->
            let
                workers =
                    Logic.countUnits pid Worker model

                soldiers =
                    Logic.countUnits pid Soldier model

                hasBarracks =
                    Logic.findOwnBuilding pid Barracks model /= Nothing

                hasFarm =
                    Logic.findOwnBuilding pid Farm model /= Nothing
            in
            if workers < workerTarget && p.gold >= workerCost then
                Logic.spawnUnit pid Worker workerCost Base model

            else if not hasBarracks && workers >= 3 && p.gold >= barracksCost then
                buildNearBase pid Barracks model

            else if hasBarracks && soldiers < armyTarget && p.gold >= soldierCost then
                Logic.spawnUnit pid Soldier soldierCost Barracks model

            else if hasBarracks && not hasFarm && p.gold >= farmCost then
                buildNearBase pid Farm model

            else
                model


{-| Place a building on the first clear tile near the player's base. -}
buildNearBase : PlayerId -> BuildingKind -> Model -> Model
buildNearBase pid kind model =
    case Logic.ownBase pid model of
        Just base ->
            case clearTileNear ( base.x, base.y ) model of
                Just ( x, y ) ->
                    Logic.placeBuilding pid kind x y model

                Nothing ->
                    model

        Nothing ->
            model



-- MILITARY -------------------------------------------------------------------------------------


{-| Once the army is big enough and not already fighting, send it at the nearest enemy. -}
militaryAction : PlayerId -> Model -> Model
militaryAction pid model =
    let
        soldiers =
            Logic.countUnits pid Soldier model

        engaged =
            List.any (\u -> u.owner == pid && u.kind == Soldier && u.attack /= Nothing) model.units
    in
    if soldiers >= attackThreshold && not engaged then
        case nearestEnemyBuilding pid model of
            Just b ->
                Logic.attackMove pid (stagingTile ( b.x, b.y ) model) (stagingTileY ( b.x, b.y ) model) model

            Nothing ->
                model

    else
        model


{-| A passable tile next to a target building to stage an attack on (so the army arrives, goes idle,
and auto-engages the building and its defenders). Falls back to the tile below it. -}
stagingTile : ( Int, Int ) -> Model -> Int
stagingTile pos model =
    Tuple.first (staging pos model)


stagingTileY : ( Int, Int ) -> Model -> Int
stagingTileY pos model =
    Tuple.second (staging pos model)


staging : ( Int, Int ) -> Model -> ( Int, Int )
staging ( bx, by ) model =
    let
        ok ( x, y ) =
            Logic.inBounds x y model && Logic.passableAt x y model && not (Logic.occupiedByBuilding x y model)

        candidates =
            [ ( bx, by + 1 ), ( bx, by - 1 ), ( bx + 1, by ), ( bx - 1, by ) ]
    in
    case List.head (List.filter ok candidates) of
        Just spot ->
            spot

        Nothing ->
            ( bx, by + 1 )


{-| The enemy building nearest the player's base (a Base is preferred by being closer to victory, but
any enemy building works as a target). -}
nearestEnemyBuilding : PlayerId -> Model -> Maybe Building
nearestEnemyBuilding pid model =
    let
        origin =
            case Logic.ownBase pid model of
                Just b ->
                    ( b.x, b.y )

                Nothing ->
                    ( model.width // 2, model.height // 2 )

        enemies =
            List.filter (\b -> b.owner /= pid) model.buildings
    in
    case List.sortBy (\b -> manhattan origin ( b.x, b.y )) enemies of
        b :: _ ->
            Just b

        [] ->
            Nothing



-- SPATIAL HELPERS ------------------------------------------------------------------------------


resourceTiles : Model -> List ( Int, Int )
resourceTiles model =
    List.filterMap
        (\t ->
            case t.terrain of
                GoldMine ->
                    Just ( t.x, t.y )

                Forest ->
                    Just ( t.x, t.y )

                _ ->
                    Nothing
        )
        model.map


nearestTile : ( Int, Int ) -> List ( Int, Int ) -> Maybe ( Int, Int )
nearestTile from tiles =
    case List.sortBy (manhattan from) tiles of
        t :: _ ->
            Just t

        [] ->
            Nothing


{-| The nearest clear, passable, unoccupied tile to `center` in an outward ring search (radius 1–3). -}
clearTileNear : ( Int, Int ) -> Model -> Maybe ( Int, Int )
clearTileNear ( cx, cy ) model =
    let
        ring r =
            List.concatMap
                (\dy -> List.map (\dx -> ( cx + dx, cy + dy )) (List.range -r r))
                (List.range -r r)

        ok ( x, y ) =
            Logic.inBounds x y model
                && Logic.passableAt x y model
                && not (Logic.occupiedByBuilding x y model)
                && not (( x, y ) == ( cx, cy ))
    in
    case List.filter ok (ring 1 ++ ring 2 ++ ring 3) of
        spot :: _ ->
            Just spot

        [] ->
            Nothing


manhattan : ( Int, Int ) -> ( Int, Int ) -> Int
manhattan ( ax, ay ) ( bx, by ) =
    abs (ax - bx) + abs (ay - by)
