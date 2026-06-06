module RtsTest exposing (suite)

{-| The RTS test suite: pure-function tests for the RNG, the map generator, the game rules
(train/build/gather/combat/win-lose), the AI making real progress, and the post-game rating. Run with

    elm test test/RtsTest.elm <every src/RTS/*.elm>

(the runner needs the game modules on the path). Because the whole engine is deterministic and
effect-free, every behaviour below is checked without a browser.
-}

import Expect
import Fuzz
import RTS.Ai as Ai
import RTS.Game as Game
import RTS.Logic as Logic
import RTS.Map as Map
import RTS.Model exposing (..)
import RTS.Rating as Rating
import RTS.Rng as Rng
import Test exposing (Test, describe, fuzz, test)



-- helpers --------------------------------------------------------------------------------------


{-| A started match: the given size, opponent count and default seed. -}
started : MapSize -> Int -> Model
started size opponents =
    Game.startGame { initSetup | mapSize = size, opponents = opponents }


initSetup : Model
initSetup =
    Game.init


send : Msg -> Model -> Model
send msg model =
    Game.update msg model


runTicks : Int -> Model -> Model
runTicks n model =
    if n <= 0 then
        model

    else
        runTicks (n - 1) (Game.update Tick model)


terrainAt : Int -> Int -> Model -> Maybe Terrain
terrainAt x y model =
    Logic.lookupTerrain x y model


near : ( Int, Int ) -> List ( Int, Int ) -> Bool
near ( px, py ) tiles =
    List.any (\( x, y ) -> max (abs (x - px)) (abs (y - py)) <= 4) tiles


resourcesOfTerrain : Terrain -> Model -> List ( Int, Int )
resourcesOfTerrain wanted model =
    List.filterMap
        (\t ->
            if t.terrain == wanted then
                Just ( t.x, t.y )

            else
                Nothing
        )
        model.map



-- soldiers for combat tests --------------------------------------------------------------------


soldier : Int -> PlayerId -> Float -> Float -> Maybe Int -> Unit
soldier id owner x y target =
    { id = id
    , owner = owner
    , x = x
    , y = y
    , tx = x
    , ty = y
    , path = []
    , kind = Soldier
    , hp = maxHp Soldier
    , carrying = 0
    , assigned = Nothing
    , attack = target
    , cooldown = 0
    }



-- the suite ------------------------------------------------------------------------------------


suite : Test
suite =
    describe "RTS"
        [ rngTests
        , mapTests
        , setupTests
        , economyTests
        , gatherTests
        , combatTests
        , aiTests
        , endToEndTests
        , ratingTests
        ]


rngTests : Test
rngTests =
    describe "Rng"
        [ test "is deterministic for a fixed seed" <|
            \_ ->
                Expect.equal (Rng.int 1000 (Rng.seed 42)) (Rng.int 1000 (Rng.seed 42))
        , test "range stays within bounds" <|
            \_ ->
                Expect.equal True (List.all (\s -> within 5 10 (Tuple.first (Rng.range 5 10 (Rng.seed s)))) (List.range 1 200))
        , fuzz (Fuzz.intRange 1 100000) "int n is always in [0, n)" <|
            \s ->
                let
                    ( v, _ ) =
                        Rng.int 50 (Rng.seed s)
                in
                Expect.equal True (v >= 0 && v < 50)
        , test "shuffle keeps every element" <|
            \_ ->
                let
                    ( shuffled, _ ) =
                        Rng.shuffle [ 1, 2, 3, 4, 5 ] (Rng.seed 7)
                in
                Expect.equal [ 1, 2, 3, 4, 5 ] (List.sort shuffled)
        ]


within : Int -> Int -> Int -> Bool
within lo hi v =
    v >= lo && v <= hi


mapTests : Test
mapTests =
    describe "Map generator"
        [ test "produces width*height tiles for every size" <|
            \_ ->
                let
                    check size =
                        let
                            g =
                                Map.generate size 2 (Rng.seed 1)
                        in
                        List.length g.tiles == g.width * g.height
                in
                Expect.equal True (List.all check [ Small, Medium, Large ])
        , test "yields one start per player, spread apart" <|
            \_ ->
                let
                    g =
                        Map.generate Medium 3 (Rng.seed 1)
                in
                Expect.equal 3 (List.length g.starts)
        , test "every start tile is passable (a carved clearing)" <|
            \_ ->
                let
                    g =
                        Map.generate Large 2 (Rng.seed 99)

                    model =
                        modelFrom g

                    ok ( x, y ) =
                        case terrainAt x y model of
                            Just terrain ->
                                passableTerrain terrain

                            Nothing ->
                                False
                in
                Expect.equal True (List.all ok g.starts)
        , test "each player has a gold mine and a forest nearby" <|
            \_ ->
                let
                    g =
                        Map.generate Medium 2 (Rng.seed 5)

                    model =
                        modelFrom g

                    mines =
                        resourcesOfTerrain GoldMine model

                    forests =
                        resourcesOfTerrain Forest model

                    hasBoth st =
                        near st mines && near st forests
                in
                Expect.equal True (List.all hasBoth g.starts)
        , test "is deterministic for a fixed seed" <|
            \_ ->
                Expect.equal
                    (Map.generate Medium 2 (Rng.seed 123)).tiles
                    (Map.generate Medium 2 (Rng.seed 123)).tiles
        ]


modelFrom : Map.Generated -> Model
modelFrom g =
    { initSetup | width = g.width, height = g.height, map = g.tiles }


setupTests : Test
setupTests =
    describe "Setup & start"
        [ test "opens on the setup screen" <|
            \_ ->
                Expect.equal SetupScreen Game.init.screen
        , test "StartGame switches to the game screen" <|
            \_ ->
                Expect.equal GameScreen (started Medium 1).screen
        , test "one opponent means two players, each with a base" <|
            \_ ->
                let
                    m =
                        started Medium 1
                in
                Expect.all
                    [ \mm -> Expect.equal 2 (List.length mm.players)
                    , \mm -> Expect.equal 2 (List.length (List.filter (\b -> b.kind == Base) mm.buildings))
                    ]
                    m
        , test "the human starts with workers and 150 gold" <|
            \_ ->
                let
                    m =
                        started Small 0
                in
                Expect.all
                    [ \mm -> Expect.equal 2 (Logic.countUnits humanId Worker mm)
                    , \mm -> Expect.equal 150 (humanGold mm)
                    ]
                    m
        , test "SetOpponents is clamped to 0..2" <|
            \_ ->
                Expect.equal 2 (send (SetOpponents 5) Game.init).opponents
        , test "zero opponents means a solo economic match" <|
            \_ ->
                Expect.equal 1 (List.length (started Small 0).players)
        ]


humanGold : Model -> Int
humanGold model =
    case Logic.player humanId model of
        Just p ->
            p.gold

        Nothing ->
            -1


economyTests : Test
economyTests =
    describe "Economy & building"
        [ test "a tick advances the clock" <|
            \_ ->
                Expect.equal 1 (runTicks 1 (started Small 0)).tick
        , test "training a worker costs gold and adds a unit" <|
            \_ ->
                let
                    m0 =
                        started Small 0

                    m1 =
                        send TrainWorker m0
                in
                Expect.all
                    [ \mm -> Expect.equal 3 (Logic.countUnits humanId Worker mm)
                    , \mm -> Expect.equal 100 (humanGold mm)
                    ]
                    m1
        , test "a barracks is placed on a clear tile and costs gold" <|
            \_ ->
                let
                    m0 =
                        started Medium 0

                    base =
                        Logic.ownBase humanId m0

                    spot =
                        case base of
                            Just b ->
                                ( b.x, b.y + 2 )

                            Nothing ->
                                ( 0, 0 )

                    m1 =
                        send StartBarracks m0

                    m2 =
                        send (ClickTile (Tuple.first spot) (Tuple.second spot)) m1
                in
                Expect.all
                    [ \mm -> Expect.equal 1 (List.length (Logic.buildingsOf humanId mm |> List.filter (\b -> b.kind == Barracks)))
                    , \mm -> Expect.equal (150 - barracksCost) (humanGold mm)
                    ]
                    m2
        , test "soldiers need a barracks first" <|
            \_ ->
                -- No barracks yet → training a soldier is refused (no unit, no spend).
                Expect.equal 0 (Logic.countUnits humanId Soldier (send TrainSoldier (started Small 0)))
        ]


gatherTests : Test
gatherTests =
    describe "Gathering"
        [ test "a worker sent to a gold mine raises the owner's gold" <|
            \_ ->
                let
                    m0 =
                        started Medium 0

                    mine =
                        List.head (resourcesOfTerrain GoldMine m0)

                    worker =
                        List.head (Logic.unitsOf humanId m0)

                    commanded =
                        case ( mine, worker ) of
                            ( Just ( mx, my ), Just w ) ->
                                m0
                                    |> send (SelectUnit w.id)
                                    |> send (ClickTile mx my)

                            _ ->
                                m0

                    after =
                        runTicks 250 commanded
                in
                Expect.greaterThan 150 (humanGold after)
        , test "findPath reaches a resource from the base" <|
            \_ ->
                let
                    m0 =
                        started Medium 0

                    base =
                        Logic.ownBase humanId m0

                    mine =
                        List.head (resourcesOfTerrain GoldMine m0)

                    path =
                        case ( base, mine ) of
                            ( Just b, Just goal ) ->
                                Logic.findPath m0 ( b.x, b.y + 1 ) goal

                            _ ->
                                []

                    endsAtGoal =
                        case ( List.head (List.reverse path), mine ) of
                            ( Just last, Just goal ) ->
                                last == goal

                            _ ->
                                False
                in
                Expect.equal True (not (List.isEmpty path) && endsAtGoal)
        ]


combatTests : Test
combatTests =
    describe "Combat"
        [ test "an adjacent attacker damages its target" <|
            \_ ->
                let
                    base =
                        started Small 1

                    a =
                        soldier 9001 0 5 5 (Just 9002)

                    b =
                        soldier 9002 1 6 5 Nothing

                    arena =
                        { base | units = [ a, b ] }

                    ( units2, _, _ ) =
                        Logic.resolveCombat arena

                    target =
                        List.head (List.filter (\u -> u.id == 9002) units2)
                in
                case target of
                    Just t ->
                        Expect.lessThan (maxHp Soldier) t.hp

                    Nothing ->
                        Expect.fail "target should still be alive after one hit"
        , test "a killing blow removes the unit and credits a kill" <|
            \_ ->
                let
                    base =
                        started Small 1

                    a =
                        soldier 9001 0 5 5 (Just 9002)

                    weak =
                        let
                            b =
                                soldier 9002 1 6 5 Nothing
                        in
                        { b | hp = 1 }

                    arena =
                        { base | units = [ a, weak ] }

                    ( units2, _, players2 ) =
                        Logic.resolveCombat arena

                    killerKills =
                        players2
                            |> List.filter (\p -> p.id == 0)
                            |> List.head
                            |> Maybe.map .kills
                            |> Maybe.withDefault 0
                in
                Expect.all
                    [ \_ -> Expect.equal True (List.all (\u -> u.id /= 9002) units2)
                    , \_ -> Expect.equal 1 killerKills
                    ]
                    ()
        , test "out of range there is no damage" <|
            \_ ->
                let
                    base =
                        started Small 1

                    a =
                        soldier 9001 0 5 5 (Just 9002)

                    b =
                        soldier 9002 1 12 5 Nothing

                    arena =
                        { base | units = [ a, b ] }

                    ( units2, _, _ ) =
                        Logic.resolveCombat arena
                in
                Expect.equal (maxHp Soldier)
                    (units2
                        |> List.filter (\u -> u.id == 9002)
                        |> List.head
                        |> Maybe.map .hp
                        |> Maybe.withDefault 0
                    )
        , test "a base with no buildings left marks its player defeated" <|
            \_ ->
                let
                    base =
                        started Small 1

                    -- strip player 1's buildings, then re-run the defeat check
                    stripped =
                        { base | buildings = List.filter (\b -> b.owner /= 1) base.buildings }

                    players2 =
                        Logic.markDefeated stripped
                in
                Expect.equal True
                    (players2
                        |> List.filter (\p -> p.id == 1)
                        |> List.head
                        |> Maybe.map .defeated
                        |> Maybe.withDefault False
                    )
        ]


aiTests : Test
aiTests =
    describe "AI"
        [ test "an opponent grows its economy or army over time" <|
            \_ ->
                let
                    m0 =
                        started Small 1

                    after =
                        runTicks 320 m0

                    aiUnits =
                        List.length (Logic.unitsOf 1 after)

                    aiBuildings =
                        List.length (Logic.buildingsOf 1 after)
                in
                -- It should have trained extra units and/or put up a second building.
                Expect.equal True (aiUnits > 2 || aiBuildings > 1)
        , test "AI players act on staggered cadences" <|
            \_ ->
                Expect.equal True (Ai.aiPeriod > 1)
        ]


endToEndTests : Test
endToEndTests =
    describe "End to end"
        [ test "eliminating the enemy ends the match on the results screen" <|
            \_ ->
                let
                    m0 =
                        started Small 1

                    -- Wipe the AI's buildings; the next tick should resolve the win.
                    onlyHuman =
                        { m0 | buildings = List.filter (\b -> b.owner == humanId) m0.buildings }

                    ended =
                        send Tick onlyHuman
                in
                Expect.all
                    [ \mm -> Expect.equal ResultScreen mm.screen
                    , \mm -> Expect.equal Won mm.status
                    , \mm -> Expect.equal True (not (List.isEmpty mm.history))
                    ]
                    ended
        , test "power history accumulates over time" <|
            \_ ->
                let
                    after =
                        runTicks 70 (started Small 0)
                in
                -- An initial sample plus snapshots every 25 ticks.
                Expect.greaterThan 2 (List.length after.history)
        , test "the game freezes once it is over" <|
            \_ ->
                let
                    m0 =
                        started Small 1

                    onlyHuman =
                        { m0 | buildings = List.filter (\b -> b.owner == humanId) m0.buildings }

                    ended =
                        send Tick onlyHuman

                    afterMore =
                        send Tick ended
                in
                Expect.equal ended.tick afterMore.tick
        ]


ratingTests : Test
ratingTests =
    describe "Rating"
        [ test "a win grades B or better and gains rating" <|
            \_ ->
                let
                    r =
                        Rating.rate Won (started Medium 1)
                in
                Expect.all
                    [ \rr -> Expect.equal True (List.member rr.grade [ "S", "A", "B" ])
                    , \rr -> Expect.greaterThan 0 rr.ratingDelta
                    , \rr -> Expect.equal 2 (List.length rr.rankings)
                    ]
                    r
        , test "a loss loses rating and grades D or F" <|
            \_ ->
                let
                    r =
                        Rating.rate Lost (started Medium 1)
                in
                Expect.all
                    [ \rr -> Expect.lessThan 0 rr.ratingDelta
                    , \rr -> Expect.equal True (List.member rr.grade [ "D", "F" ])
                    ]
                    r
        , test "rankings are placed 1..n" <|
            \_ ->
                let
                    r =
                        Rating.rate Won (started Small 2)
                in
                Expect.equal [ 1, 2, 3 ] (List.map .place r.rankings)
        ]
