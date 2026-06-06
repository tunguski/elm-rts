module RTS.Game exposing (init, update, startGame, sampleEvery)

{-| The orchestration layer: the initial model, the `update` that interprets every `Msg`, and the
real-time `tick` that composes the pure steps from `RTS.Logic` with the opponent decisions from
`RTS.Ai`. This is the one module that knows the *order* things happen in a tick (move → fight →
re-target → gather → income → reveal → score), and the one that owns the pre-game setup, the win/lose
rules, and the power-history sampling that feeds the results chart.
-}

import RTS.Ai as Ai
import RTS.Logic as Logic
import RTS.Map as Map
import RTS.Model exposing (..)
import RTS.Rng as Rng


startGold : Int
startGold =
    150


startingWorkers : Int
startingWorkers =
    2


{-| Take a power snapshot every this-many ticks (≈ every 5 seconds at 5 ticks/s). -}
sampleEvery : Int
sampleEvery =
    25


{-| Passive gold per tick per farm a player owns. -}
farmRate : Int
farmRate =
    2


defaultSeed : Int
defaultSeed =
    20260606



-- INIT -----------------------------------------------------------------------------------------


{-| The app opens on the setup screen with sensible defaults; no game exists until `StartGame`. -}
init : Model
init =
    { screen = SetupScreen
    , mapSize = Medium
    , opponents = 1
    , seed = defaultSeed
    , width = 0
    , height = 0
    , map = []
    , units = []
    , buildings = []
    , players = []
    , selected = []
    , mode = Normal
    , tick = 0
    , status = Playing
    , message = ""
    , history = []
    , rng = Rng.seed defaultSeed
    , nextId = 1
    }



-- UPDATE ---------------------------------------------------------------------------------------


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            if model.screen == GameScreen && model.status == Playing then
                tick model

            else
                model

        -- setup screen ---------------------------------------------------------------------------
        SetMapSize size ->
            { model | mapSize = size }

        SetOpponents n ->
            { model | opponents = clamp 0 2 n }

        Reroll ->
            { model | seed = Rng.step (Rng.seed model.seed) }

        StartGame ->
            startGame model

        NewGame ->
            { init | mapSize = model.mapSize, opponents = model.opponents, seed = Rng.step (Rng.seed model.seed) }

        -- in-game commands -----------------------------------------------------------------------
        SelectUnit id ->
            if List.any (\u -> u.id == id && u.owner == humanId) model.units then
                { model | selected = [ id ] }

            else
                model

        SelectArmy ->
            { model
                | selected =
                    List.filterMap
                        (\u ->
                            if u.owner == humanId && u.kind == Soldier then
                                Just u.id

                            else
                                Nothing
                        )
                        model.units
                , message = "Whole army selected — click a tile or an enemy."
            }

        ClickTile x y ->
            clickTile x y model

        TrainWorker ->
            withSelectNew (Logic.spawnUnit humanId Worker workerCost Base model) model "Worker trained." "Need a Base and 50 gold."

        TrainSoldier ->
            withSelectNew (Logic.spawnUnit humanId Soldier soldierCost Barracks model) model "Soldier trained." "Need a Barracks and 60 gold."

        StartBarracks ->
            startPlacing Barracks barracksCost model

        StartFarm ->
            startPlacing Farm farmCost model

        Cancel ->
            { model | mode = Normal, selected = [], message = "" }


{-| Acknowledge a train command: if it actually added a unit, select it; otherwise show why not. -}
withSelectNew : Model -> Model -> String -> String -> Model
withSelectNew after before okMsg failMsg =
    if after.nextId > before.nextId then
        { after | selected = [ before.nextId ], message = okMsg }

    else
        { before | message = failMsg }


startPlacing : BuildingKind -> Int -> Model -> Model
startPlacing kind cost model =
    if humanGold model < cost then
        { model | message = "Need " ++ String.fromInt cost ++ " gold for a " ++ buildingLabel kind ++ "." }

    else
        { model | mode = Placing kind, message = "Click a clear tile to place the " ++ buildingLabel kind ++ "." }


humanGold : Model -> Int
humanGold model =
    case Logic.player humanId model of
        Just p ->
            p.gold

        Nothing ->
            0


clickTile : Int -> Int -> Model -> Model
clickTile x y model =
    case model.mode of
        Placing kind ->
            let
                after =
                    Logic.placeBuilding humanId kind x y model
            in
            if after.nextId > model.nextId then
                { after | mode = Normal, message = buildingLabel kind ++ " built." }

            else
                { model | message = "Can't build there — pick a clear, affordable tile." }

        Normal ->
            if List.isEmpty model.selected then
                { model | message = "Select a unit (or your army) first." }

            else
                let
                    after =
                        Logic.commandUnits humanId model.selected x y model
                in
                { after
                    | message =
                        if Logic.enemyTargetAt humanId x y model /= Nothing then
                            "Attacking!"

                        else if isResourceAt x y model then
                            "Workers sent to gather."

                        else
                            "On the move."
                }


isResourceAt : Int -> Int -> Model -> Bool
isResourceAt =
    Logic.isResourceAt



-- THE TICK -------------------------------------------------------------------------------------


tick : Model -> Model
tick model =
    model
        |> Ai.step
        |> stepWorld
        |> checkEnd


{-| One real-time world step, in order: move & chase, resolve combat, re-acquire targets, gather,
collect farm income, reveal the human's fog, mark defeats, advance the clock and (periodically)
record a power snapshot. -}
stepWorld : Model -> Model
stepWorld model =
    let
        m1 =
            { model | units = Logic.moveUnits model }

        ( u2, b2, p2 ) =
            Logic.resolveCombat m1

        m2 =
            { m1 | units = u2, buildings = b2, players = p2 }

        m3 =
            { m2 | units = Logic.autoAcquire m2 }

        ( u4, p4 ) =
            Logic.runGather m3

        m4 =
            { m3 | units = u4, players = farmIncome p4 m3 }

        m5 =
            { m4
                | map = Logic.revealFog m4
                , players = Logic.markDefeated m4
                , tick = m4.tick + 1
            }
    in
    recordSample m5


farmIncome : List Player -> Model -> List Player
farmIncome players model =
    List.map
        (\p ->
            let
                farms =
                    List.length (List.filter (\b -> b.owner == p.id && b.kind == Farm) model.buildings)
            in
            { p | gold = p.gold + farmRate * farms }
        )
        players


recordSample : Model -> Model
recordSample model =
    if modBy sampleEvery model.tick == 0 then
        { model | history = model.history ++ [ Logic.sample model ] }

    else
        model



-- WIN / LOSE -----------------------------------------------------------------------------------


checkEnd : Model -> Model
checkEnd model =
    let
        status =
            decideStatus model
    in
    if status == Playing then
        model

    else
        -- Freeze the game, jump to the results screen, and pin a final power snapshot.
        { model
            | status = status
            , screen = ResultScreen
            , history = model.history ++ [ Logic.sample model ]
            , message = endMessage status
        }


decideStatus : Model -> Status
decideStatus model =
    if model.opponents == 0 then
        -- Economic race: reach the power goal before time runs out.
        if Logic.playerPower humanId model >= sizeGoal model.mapSize then
            Won

        else if model.tick >= tickLimit then
            Lost

        else
            Playing

    else
        let
            humanAlive =
                not (defeated humanId model)

            allEnemiesDead =
                List.all (\p -> p.isHuman || p.defeated) model.players
        in
        if not humanAlive then
            Lost

        else if allEnemiesDead then
            Won

        else if model.tick >= tickLimit then
            decideOnPower model

        else
            Playing


{-| At the time limit, the highest power wins (a tie for the lead is a draw). -}
decideOnPower : Model -> Status
decideOnPower model =
    let
        humanPower =
            Logic.playerPower humanId model

        best =
            List.maximum (List.map (\p -> Logic.playerPower p.id model) model.players)
                |> Maybe.withDefault humanPower

        leaders =
            List.length (List.filter (\p -> Logic.playerPower p.id model == best) model.players)
    in
    if humanPower == best && leaders == 1 then
        Won

    else if humanPower == best then
        Draw

    else
        Lost


defeated : PlayerId -> Model -> Bool
defeated pid model =
    case Logic.player pid model of
        Just p ->
            p.defeated

        Nothing ->
            True


endMessage : Status -> String
endMessage status =
    case status of
        Won ->
            "Victory!"

        Lost ->
            "Defeat."

        Draw ->
            "A draw — the lead was shared."

        Playing ->
            ""



-- START A GAME ---------------------------------------------------------------------------------


{-| Generate a fresh match from the setup choices: a map, a base and starting workers for each
player, the human's opening fog cleared, and an initial power snapshot. -}
startGame : Model -> Model
startGame model =
    let
        playerCount =
            model.opponents + 1

        gen =
            Map.generate model.mapSize playerCount (Rng.seed model.seed)

        players =
            List.map
                (\pid ->
                    { id = pid
                    , isHuman = pid == humanId
                    , gold = startGold
                    , wood = 0
                    , kills = 0
                    , defeated = False
                    }
                )
                (List.range 0 (playerCount - 1))

        ( units, buildings, nextId ) =
            placeStarts gen.starts

        prelim =
            { model
                | screen = GameScreen
                , width = gen.width
                , height = gen.height
                , map = gen.tiles
                , units = units
                , buildings = buildings
                , players = players
                , selected = []
                , mode = Normal
                , tick = 0
                , status = Playing
                , message = "Gather gold & wood, train an army, and crush every enemy base."
                , history = []
                , rng = gen.seed
                , nextId = nextId
            }
    in
    { prelim
        | map = Logic.revealFog prelim
        , history = [ Logic.sample prelim ]
    }


{-| Lay down each player's base and starting workers (player ids follow the start order, human
first), threading the shared id counter. -}
placeStarts : List ( Int, Int ) -> ( List Unit, List Building, Int )
placeStarts starts =
    let
        indexed =
            List.indexedMap Tuple.pair starts
    in
    List.foldl placeOne ( [], [], 1 ) indexed


placeOne : ( Int, ( Int, Int ) ) -> ( List Unit, List Building, Int ) -> ( List Unit, List Building, Int )
placeOne ( pid, ( sx, sy ) ) ( us, bs, nextId ) =
    let
        base =
            { id = nextId, owner = pid, x = sx, y = sy, kind = Base, hp = buildingHp Base }

        workers =
            List.map
                (\i ->
                    let
                        wx =
                            toFloat (sx - 1 + i)

                        wy =
                            toFloat (sy + 1)
                    in
                    { id = nextId + 1 + i
                    , owner = pid
                    , x = wx
                    , y = wy
                    , tx = wx
                    , ty = wy
                    , path = []
                    , kind = Worker
                    , hp = maxHp Worker
                    , carrying = 0
                    , assigned = Nothing
                    , attack = Nothing
                    , cooldown = 0
                    }
                )
                (List.range 0 (startingWorkers - 1))
    in
    ( us ++ workers, base :: bs, nextId + 1 + startingWorkers )
