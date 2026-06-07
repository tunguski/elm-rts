module RTS.Logic exposing (..)

{-| Pure game mechanics, shared by the live game (`RTS.Game`) and the opponent AI (`RTS.Ai`):
pathfinding, movement and chasing, combat resolution, resource gathering, fog of war, the
owner-parameterised command/build/train helpers, and the per-player "power" used for scoring and the
results chart.

Everything here is a pure `Model -> something` function — no `update`, no AI and no effects — so the
orchestration layer can compose the steps in one place and the test suite can exercise each in
isolation. Resources are per-player, and every unit/building carries an `owner`, so all of these are
written to work for any player, human or AI.
-}

import Dict exposing (Dict)
import RTS.Model exposing (..)
import Set exposing (Set)


speed : Float
speed =
    0.18



-- GEOMETRY -------------------------------------------------------------------------------------


distance : Float -> Float -> Float -> Float -> Float
distance ax ay bx by =
    sqrt ((bx - ax) * (bx - ax) + (by - ay) * (by - ay))


unitDistance : Unit -> Unit -> Float
unitDistance a b =
    distance a.x a.y b.x b.y



-- TERRAIN / OCCUPANCY --------------------------------------------------------------------------


lookupTerrain : Int -> Int -> Model -> Maybe Terrain
lookupTerrain x y model =
    List.head
        (List.filterMap
            (\t ->
                if t.x == x && t.y == y then
                    Just t.terrain

                else
                    Nothing
            )
            model.map
        )


passableAt : Int -> Int -> Model -> Bool
passableAt x y model =
    case lookupTerrain x y model of
        Just terrain ->
            passableTerrain terrain

        Nothing ->
            False


inBounds : Int -> Int -> Model -> Bool
inBounds x y model =
    x >= 0 && x < model.width && y >= 0 && y < model.height


occupiedByBuilding : Int -> Int -> Model -> Bool
occupiedByBuilding x y model =
    List.any (\b -> b.x == x && b.y == y) model.buildings


isResourceAt : Int -> Int -> Model -> Bool
isResourceAt x y model =
    case lookupTerrain x y model of
        Just GoldMine ->
            True

        Just Forest ->
            True

        _ ->
            False


isForestAt : Int -> Int -> Model -> Bool
isForestAt x y model =
    lookupTerrain x y model == Just Forest



-- PATHFINDING ----------------------------------------------------------------------------------


{-| A breadth-first shortest path of tiles from `start` to `goal` (4-directional), excluding `start`
and ending at `goal`. Impassable terrain and tiles occupied by buildings are avoided, except the goal
itself is always allowed (so a worker can path onto a resource and a soldier onto an enemy building).
Returns `[]` when no path exists. Exploration is bounded so a sealed-off goal can't loop forever.

The blocked tiles are gathered into a `Set` *once* per call (so a per-node passability check is a
`Set` lookup, not an O(map)/O(buildings) rescan), and the search runs level-by-level with a
`came-from` map rather than appending to a list-queue, keeping the whole thing roughly O(tiles)
instead of the former O(nodes · map). -}
findPath : Model -> ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
findPath model start goal =
    if start == goal then
        []

    else
        let
            blocked =
                blockedTiles model

            walkable ( x, y ) =
                x >= 0 && x < model.width && y >= 0 && y < model.height && (( x, y ) == goal || not (Set.member ( x, y ) blocked))

            cameFrom =
                bfsLevels goal walkable start [ start ] Dict.empty 8000
        in
        if Dict.member goal cameFrom then
            reconstruct cameFrom start goal []

        else
            []


{-| Impassable tiles: terrain that blocks movement, plus every building footprint. Built once per
pathfind. -}
blockedTiles : Model -> Set ( Int, Int )
blockedTiles model =
    let
        terrainBlocked =
            List.foldl
                (\t s ->
                    if passableTerrain t.terrain then
                        s

                    else
                        Set.insert ( t.x, t.y ) s
                )
                Set.empty
                model.map
    in
    List.foldl (\b s -> Set.insert ( b.x, b.y ) s) terrainBlocked model.buildings


{-| Level-order BFS: expand the whole current frontier, recording each newly reached tile's parent in
`cameFrom`, then recurse on the next frontier. Stops once the goal is reached, the frontier empties,
or the node budget is spent. -}
bfsLevels : ( Int, Int ) -> (( Int, Int ) -> Bool) -> ( Int, Int ) -> List ( Int, Int ) -> Dict ( Int, Int ) ( Int, Int ) -> Int -> Dict ( Int, Int ) ( Int, Int )
bfsLevels goal walkable start frontier cameFrom budget =
    if Dict.member goal cameFrom || budget <= 0 then
        cameFrom

    else
        let
            ( next, cameFrom2 ) =
                List.foldl (expandFrom walkable start) ( [], cameFrom ) frontier
        in
        case next of
            [] ->
                cameFrom2

            _ ->
                bfsLevels goal walkable start next cameFrom2 (budget - List.length next)


expandFrom : (( Int, Int ) -> Bool) -> ( Int, Int ) -> ( Int, Int ) -> ( List ( Int, Int ), Dict ( Int, Int ) ( Int, Int ) ) -> ( List ( Int, Int ), Dict ( Int, Int ) ( Int, Int ) )
expandFrom walkable start cell ( acc, cameFrom ) =
    List.foldl
        (\n ( a, cf ) ->
            if n /= start && walkable n && not (Dict.member n cf) then
                ( n :: a, Dict.insert n cell cf )

            else
                ( a, cf )
        )
        ( acc, cameFrom )
        (neighbors4 cell)


{-| Walk the `came-from` map back from `goal` to `start`, producing the path forward and excluding
`start`. -}
reconstruct : Dict ( Int, Int ) ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
reconstruct cameFrom start cur acc =
    if cur == start then
        acc

    else
        case Dict.get cur cameFrom of
            Just parent ->
                reconstruct cameFrom start parent (cur :: acc)

            Nothing ->
                acc


neighbors4 : ( Int, Int ) -> List ( Int, Int )
neighbors4 ( x, y ) =
    [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]



-- MOVEMENT & CHASING ---------------------------------------------------------------------------


{-| Advance every unit one step: chasers head for (and stop in range of) their target, others follow
their stored path toward `tx`/`ty`. The id→position map is built once and shared, so a chaser's
target lookup is a `Dict` hit rather than a full units+buildings rescan. -}
moveUnits : Model -> List Unit
moveUnits model =
    let
        pos =
            posDict model
    in
    List.map (stepUnit model pos) model.units


stepUnit : Model -> Dict Int ( Float, Float ) -> Unit -> Unit
stepUnit model pos u =
    case u.attack of
        Just tid ->
            case Dict.get tid pos of
                Just ( txp, typ ) ->
                    if distance u.x u.y txp typ <= attackRange u.kind then
                        -- In range: hold position and let combat do the rest.
                        { u | path = [], tx = u.x, ty = u.y }

                    else
                        advance (ensureChasePath model u ( round txp, round typ ))

                Nothing ->
                    -- Target gone; drop the order and stand still.
                    advance { u | attack = Nothing, path = [], tx = u.x, ty = u.y }

        Nothing ->
            advance u


{-| Keep a chaser's path pointed at its (moving) target: recompute when the path is empty or on a
staggered cadence, so a long chase isn't re-planned every single tick. -}
ensureChasePath : Model -> Unit -> ( Int, Int ) -> Unit
ensureChasePath model u goal =
    let
        due =
            List.isEmpty u.path || modBy 8 (model.tick + u.id) == 0
    in
    if due then
        { u | path = findPath model ( round u.x, round u.y ) goal, tx = toFloat (Tuple.first goal), ty = toFloat (Tuple.second goal) }

    else
        u


advance : Unit -> Unit
advance u =
    case u.path of
        [] ->
            homeIn u

        ( wx, wy ) :: rest ->
            let
                gx =
                    toFloat wx

                gy =
                    toFloat wy

                d =
                    distance u.x u.y gx gy
            in
            if d <= speed then
                { u | x = gx, y = gy, path = rest }

            else
                { u | x = u.x + speed * (gx - u.x) / d, y = u.y + speed * (gy - u.y) / d }


homeIn : Unit -> Unit
homeIn u =
    let
        d =
            distance u.x u.y u.tx u.ty
    in
    if d <= speed then
        { u | x = u.tx, y = u.ty }

    else
        { u | x = u.x + speed * (u.tx - u.x) / d, y = u.y + speed * (u.ty - u.y) / d }



-- COMBAT ---------------------------------------------------------------------------------------


type alias Hit =
    { attacker : Int
    , target : Int
    , owner : PlayerId
    , dmg : Int
    }


{-| The id→position map of every unit and building, built once per tick and shared by combat and
movement so target lookups are `Dict` hits instead of full list rescans. -}
posDict : Model -> Dict Int ( Float, Float )
posDict model =
    let
        withUnits =
            List.foldl (\u d -> Dict.insert u.id ( u.x, u.y ) d) Dict.empty model.units
    in
    List.foldl (\b d -> Dict.insert b.id ( toFloat b.x, toFloat b.y ) d) withUnits model.buildings


{-| Resolve one tick of combat: every unit whose attack is off cooldown and whose target is in range
deals damage. Returns the surviving units (cooldowns ticked, dead removed), the surviving buildings,
and the players with kill counts credited for anything destroyed this tick. -}
resolveCombat : Model -> ( List Unit, List Building, List Player )
resolveCombat model =
    let
        pos =
            posDict model

        hits =
            List.filterMap (attackHit pos) model.units

        firedSet =
            Set.fromList (List.map .attacker hits)

        dmgByTarget =
            List.foldl (\h d -> Dict.update h.target (\cur -> Just (Maybe.withDefault 0 cur + h.dmg)) d) Dict.empty hits

        ownerByTarget =
            List.foldl (\h d -> Dict.insert h.target h.owner d) Dict.empty hits

        -- Units: tick cooldowns, take damage, drop the dead.
        units2 =
            List.filterMap
                (\u ->
                    let
                        cooled =
                            if Set.member u.id firedSet then
                                { u | cooldown = attackCooldown u.kind }

                            else
                                { u | cooldown = max 0 (u.cooldown - 1) }

                        hp2 =
                            cooled.hp - Maybe.withDefault 0 (Dict.get u.id dmgByTarget)
                    in
                    if hp2 <= 0 then
                        Nothing

                    else
                        Just { cooled | hp = hp2 }
                )
                model.units

        buildings2 =
            List.filterMap
                (\b ->
                    let
                        hp2 =
                            b.hp - Maybe.withDefault 0 (Dict.get b.id dmgByTarget)
                    in
                    if hp2 <= 0 then
                        Nothing

                    else
                        Just { b | hp = hp2 }
                )
                model.buildings

        deadIds =
            deaths model.units units2 ++ deaths2 model.buildings buildings2

        players2 =
            List.map
                (\p ->
                    let
                        credited =
                            List.length (List.filter (\did -> Dict.get did ownerByTarget == Just p.id) deadIds)
                    in
                    { p | kills = p.kills + credited }
                )
                model.players
    in
    ( units2, buildings2, players2 )


{-| The damage event a unit produces this tick, if any: it must have a target (looked up in the
shared position map), be off cooldown, and have that target within strike range. -}
attackHit : Dict Int ( Float, Float ) -> Unit -> Maybe Hit
attackHit pos u =
    case u.attack of
        Just tid ->
            if u.cooldown > 0 then
                Nothing

            else
                case Dict.get tid pos of
                    Just ( tx, ty ) ->
                        if distance u.x u.y tx ty <= attackRange u.kind then
                            Just { attacker = u.id, target = tid, owner = u.owner, dmg = attackDamage u.kind }

                        else
                            Nothing

                    Nothing ->
                        Nothing

        Nothing ->
            Nothing


deaths : List Unit -> List Unit -> List Int
deaths before after =
    let
        survivors =
            Set.fromList (List.map .id after)
    in
    List.filterMap
        (\u ->
            if Set.member u.id survivors then
                Nothing

            else
                Just u.id
        )
        before


deaths2 : List Building -> List Building -> List Int
deaths2 before after =
    let
        survivors =
            Set.fromList (List.map .id after)
    in
    List.filterMap
        (\b ->
            if Set.member b.id survivors then
                Nothing

            else
                Just b.id
        )
        before



-- AUTO-ACQUIRE ---------------------------------------------------------------------------------


{-| Idle soldiers (no orders, no target) auto-engage the nearest enemy within aggro range. -}
autoAcquire : Model -> List Unit
autoAcquire model =
    List.map
        (\u ->
            case ( u.kind, u.attack, u.path ) of
                ( Soldier, Nothing, [] ) ->
                    case nearestEnemyTarget model u of
                        Just tid ->
                            { u | attack = Just tid }

                        Nothing ->
                            u

                _ ->
                    u
        )
        model.units


{-| The id of the closest enemy unit or building within `aggroRange` of `u`, if any. -}
nearestEnemyTarget : Model -> Unit -> Maybe Int
nearestEnemyTarget model u =
    let
        unitTargets =
            List.filterMap
                (\e ->
                    if e.owner /= u.owner && unitDistance u e <= aggroRange then
                        Just ( e.id, unitDistance u e )

                    else
                        Nothing
                )
                model.units

        buildingTargets =
            List.filterMap
                (\b ->
                    let
                        d =
                            distance u.x u.y (toFloat b.x) (toFloat b.y)
                    in
                    if b.owner /= u.owner && d <= aggroRange then
                        Just ( b.id, d )

                    else
                        Nothing
                )
                model.buildings
    in
    closestId (unitTargets ++ buildingTargets)


closestId : List ( Int, Float ) -> Maybe Int
closestId pairs =
    case List.sortBy Tuple.second pairs of
        ( id, _ ) :: _ ->
            Just id

        [] ->
            Nothing



-- GATHERING ------------------------------------------------------------------------------------


{-| Run every worker's gather loop for one tick and return the updated units plus the players with
their gathered gold/wood added. Mirrors the single-player gather (walk to resource, fill to
`carryCap`, haul back to the owner's base, deposit) but credits the worker's *owner*. -}
runGather : Model -> ( List Unit, List Player )
runGather model =
    let
        ( units2, gainsList ) =
            List.foldr (gatherStep model) ( [], [] ) model.units

        gains =
            List.foldl (\( owner, g, w ) d -> Dict.update owner (\cur -> Just (addGain (Maybe.withDefault ( 0, 0 ) cur) g w)) d) Dict.empty gainsList

        players2 =
            List.map
                (\p ->
                    case Dict.get p.id gains of
                        Just ( g, w ) ->
                            { p | gold = p.gold + g, wood = p.wood + w }

                        Nothing ->
                            p
                )
                model.players
    in
    ( units2, players2 )


addGain : ( Int, Int ) -> Int -> Int -> ( Int, Int )
addGain ( g0, w0 ) g w =
    ( g0 + g, w0 + w )


gatherStep : Model -> Unit -> ( List Unit, List ( PlayerId, Int, Int ) ) -> ( List Unit, List ( PlayerId, Int, Int ) )
gatherStep model u ( us, gains ) =
    case u.kind of
        Soldier ->
            ( u :: us, gains )

        Worker ->
            if atOwnBase u model && u.carrying > 0 then
                let
                    ( dg, dw ) =
                        depositValue u model
                in
                ( resume u :: us, ( u.owner, dg, dw ) :: gains )

            else if u.carrying >= carryCap then
                ( sendHome u model :: us, gains )

            else if onAssignedResource u model then
                ( { u | carrying = min carryCap (u.carrying + gatherRate) } :: us, gains )

            else
                ( u :: us, gains )


depositValue : Unit -> Model -> ( Int, Int )
depositValue u model =
    case u.assigned of
        Just ( ax, ay ) ->
            if isForestAt ax ay model then
                ( 0, u.carrying )

            else
                ( u.carrying, 0 )

        Nothing ->
            ( u.carrying, 0 )


resume : Unit -> Unit
resume u =
    case u.assigned of
        Just ( ax, ay ) ->
            { u | carrying = 0, tx = toFloat ax, ty = toFloat ay, path = [] }

        Nothing ->
            { u | carrying = 0 }


sendHome : Unit -> Model -> Unit
sendHome u model =
    case ownBase u.owner model of
        Just b ->
            { u | tx = toFloat b.x, ty = toFloat (b.y + 1), path = pathTo model u ( b.x, b.y + 1 ) }

        Nothing ->
            u


pathTo : Model -> Unit -> ( Int, Int ) -> List ( Int, Int )
pathTo model u goal =
    findPath model ( round u.x, round u.y ) goal


atOwnBase : Unit -> Model -> Bool
atOwnBase u model =
    case ownBase u.owner model of
        Just b ->
            abs (round u.x - b.x) <= 1 && abs (round u.y - b.y) <= 1

        Nothing ->
            False


onAssignedResource : Unit -> Model -> Bool
onAssignedResource u model =
    case u.assigned of
        Just ( ax, ay ) ->
            round u.x == ax && round u.y == ay

        Nothing ->
            False



-- FOG OF WAR -----------------------------------------------------------------------------------


{-| Clear the human's fog around every friendly (human-owned) unit and building. The AI sees the
whole board, so only player 0's vision matters here. -}
revealFog : Model -> List Tile
revealFog model =
    let
        sources =
            List.filterMap
                (\b ->
                    if b.owner == humanId then
                        Just ( b.x, b.y )

                    else
                        Nothing
                )
                model.buildings
                ++ List.filterMap
                    (\u ->
                        if u.owner == humanId then
                            Just ( round u.x, round u.y )

                        else
                            Nothing
                    )
                    model.units

        near t ( sx, sy ) =
            abs (t.x - sx) <= revealRadius && abs (t.y - sy) <= revealRadius
    in
    List.map
        (\t ->
            if t.visible || List.any (near t) sources then
                { t | visible = True }

            else
                t
        )
        model.map



-- QUERIES --------------------------------------------------------------------------------------


player : PlayerId -> Model -> Maybe Player
player pid model =
    List.head (List.filter (\p -> p.id == pid) model.players)


unitsOf : PlayerId -> Model -> List Unit
unitsOf pid model =
    List.filter (\u -> u.owner == pid) model.units


buildingsOf : PlayerId -> Model -> List Building
buildingsOf pid model =
    List.filter (\b -> b.owner == pid) model.buildings


ownBase : PlayerId -> Model -> Maybe Building
ownBase pid model =
    List.head (List.filter (\b -> b.owner == pid && b.kind == Base) model.buildings)


findOwnBuilding : PlayerId -> BuildingKind -> Model -> Maybe Building
findOwnBuilding pid kind model =
    List.head (List.filter (\b -> b.owner == pid && b.kind == kind) model.buildings)


countUnits : PlayerId -> UnitKind -> Model -> Int
countUnits pid kind model =
    List.length (List.filter (\u -> u.owner == pid && u.kind == kind) model.units)



-- POWER ----------------------------------------------------------------------------------------


{-| A player's "power": stockpiled resources plus the value of every unit and building it owns. -}
playerPower : PlayerId -> Model -> Int
playerPower pid model =
    let
        stock =
            case player pid model of
                Just p ->
                    p.gold + p.wood

                Nothing ->
                    0

        unitVal =
            List.sum (List.map (\u -> unitPower u.kind) (unitsOf pid model))

        buildingVal =
            List.sum (List.map (\b -> buildingPower b.kind) (buildingsOf pid model))
    in
    stock + unitVal + buildingVal


{-| A power snapshot of all players (in id order) at the current tick. -}
sample : Model -> Sample
sample model =
    { tick = model.tick
    , powers = List.map (\p -> playerPower p.id model) (List.sortBy .id model.players)
    }



-- DEFEAT ---------------------------------------------------------------------------------------


{-| Mark any player with no buildings left as defeated. -}
markDefeated : Model -> List Player
markDefeated model =
    List.map
        (\p ->
            if not p.defeated && List.isEmpty (buildingsOf p.id model) then
                { p | defeated = True }

            else
                p
        )
        model.players


livingPlayers : Model -> List Player
livingPlayers model =
    List.filter (\p -> not p.defeated) model.players



-- COMMANDS (owner-parameterised) ---------------------------------------------------------------


{-| Order a set of a player's units to a tile: clicking onto an enemy makes it an attack, onto a
resource (for a worker) makes it a gather, otherwise a plain move. Units not owned by `pid` or not in
`ids` are untouched. -}
commandUnits : PlayerId -> List Int -> Int -> Int -> Model -> Model
commandUnits pid ids x y model =
    let
        enemyHere =
            enemyTargetAt pid x y model
    in
    { model
        | units =
            List.map
                (\u ->
                    if u.owner == pid && List.member u.id ids then
                        case enemyHere of
                            Just tid ->
                                { u | attack = Just tid, assigned = Nothing, path = pathTo model u ( x, y ), tx = toFloat x, ty = toFloat y }

                            Nothing ->
                                orderMove x y model u

                    else
                        u
                )
                model.units
    }


{-| The id of an enemy unit or building standing on a tile (units first), if any. -}
enemyTargetAt : PlayerId -> Int -> Int -> Model -> Maybe Int
enemyTargetAt pid x y model =
    case List.head (List.filter (\u -> u.owner /= pid && round u.x == x && round u.y == y) model.units) of
        Just u ->
            Just u.id

        Nothing ->
            case List.head (List.filter (\b -> b.owner /= pid && b.x == x && b.y == y) model.buildings) of
                Just b ->
                    Just b.id

                Nothing ->
                    Nothing


orderMove : Int -> Int -> Model -> Unit -> Unit
orderMove x y model u =
    let
        assigned =
            if u.kind == Worker && isResourceAt x y model then
                Just ( x, y )

            else
                Nothing
    in
    { u
        | tx = toFloat x
        , ty = toFloat y
        , attack = Nothing
        , assigned = assigned
        , path = pathTo model u ( x, y )
    }


{-| Point all of a player's soldiers at a tile (an attack-move): used by the player's "send army"
button and by the AI's pushes. -}
attackMove : PlayerId -> Int -> Int -> Model -> Model
attackMove pid x y model =
    let
        target =
            enemyTargetAt pid x y model
    in
    { model
        | units =
            List.map
                (\u ->
                    if u.owner == pid && u.kind == Soldier then
                        case target of
                            Just tid ->
                                { u | attack = Just tid, path = pathTo model u ( x, y ), tx = toFloat x, ty = toFloat y }

                            Nothing ->
                                orderMove x y model u

                    else
                        u
                )
                model.units
    }



-- TRAIN / BUILD --------------------------------------------------------------------------------


{-| Spend a player's gold to spawn a unit next to a building of the right kind. -}
spawnUnit : PlayerId -> UnitKind -> Int -> BuildingKind -> Model -> Model
spawnUnit pid kind cost atKind model =
    case ( findOwnBuilding pid atKind model, player pid model ) of
        ( Just b, Just p ) ->
            if p.gold < cost then
                model

            else
                let
                    spot =
                        spawnSpot b model

                    ( sx, sy ) =
                        spot

                    newUnit =
                        { id = model.nextId
                        , owner = pid
                        , x = toFloat sx
                        , y = toFloat sy
                        , tx = toFloat sx
                        , ty = toFloat sy
                        , path = []
                        , kind = kind
                        , hp = maxHp kind
                        , carrying = 0
                        , assigned = Nothing
                        , attack = Nothing
                        , cooldown = 0
                        }
                in
                { model
                    | units = newUnit :: model.units
                    , players = spend pid cost model.players
                    , nextId = model.nextId + 1
                }

        _ ->
            model


{-| A passable, unoccupied tile next to a building to spawn a unit on (falls back to just below it). -}
spawnSpot : Building -> Model -> ( Int, Int )
spawnSpot b model =
    let
        candidates =
            [ ( b.x, b.y + 1 ), ( b.x + 1, b.y ), ( b.x - 1, b.y ), ( b.x, b.y - 1 ), ( b.x + 1, b.y + 1 ) ]

        ok ( x, y ) =
            inBounds x y model && passableAt x y model && not (occupiedByBuilding x y model)
    in
    case List.head (List.filter ok candidates) of
        Just spot ->
            spot

        Nothing ->
            ( b.x, b.y + 1 )


{-| Place a building for a player on a clear tile, spending the cost. -}
placeBuilding : PlayerId -> BuildingKind -> Int -> Int -> Model -> Model
placeBuilding pid kind x y model =
    let
        cost =
            buildCost kind
    in
    case player pid model of
        Just p ->
            if p.gold < cost then
                model

            else if not (inBounds x y model) || not (passableAt x y model) || occupiedByBuilding x y model then
                model

            else
                { model
                    | buildings =
                        { id = model.nextId, owner = pid, x = x, y = y, kind = kind, hp = buildingHp kind }
                            :: model.buildings
                    , players = spend pid cost model.players
                    , nextId = model.nextId + 1
                }

        Nothing ->
            model


buildCost : BuildingKind -> Int
buildCost kind =
    case kind of
        Barracks ->
            barracksCost

        Farm ->
            farmCost

        Base ->
            barracksCost


spend : PlayerId -> Int -> List Player -> List Player
spend pid cost players =
    List.map
        (\p ->
            if p.id == pid then
                { p | gold = p.gold - cost }

            else
                p
        )
        players
