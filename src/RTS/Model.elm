module RTS.Model exposing
    ( Model
    , Screen(..)
    , Msg(..)
    , MapSize(..)
    , Tile
    , Terrain(..)
    , Unit
    , UnitKind(..)
    , Building
    , BuildingKind(..)
    , Player
    , PlayerId
    , Sample
    , Mode(..)
    , Status(..)
    , sizeLabel
    , sizeDims
    , sizeGoal
    , tileSize
    , tickLimit
    , revealRadius
    , aggroRange
    , workerCost
    , barracksCost
    , farmCost
    , soldierCost
    , carryCap
    , gatherRate
    , maxHp
    , attackDamage
    , attackRange
    , attackCooldown
    , unitPower
    , buildingPower
    , buildingHp
    , humanId
    , playerColor
    , playerName
    , buildingLabel
    , buildingColor
    , terrainColor
    , passableTerrain
    )

{-| Data model for the real-time strategy game. Pure data only — map generation lives in `RTS.Map`,
game rules in `RTS.Logic`, opponent AI in `RTS.Ai`, scoring in `RTS.Rating` and rendering in
`RTS.View`.

The model carries the whole application: a `Screen` discriminator selects the pre-game setup, the
live game, or the post-game results, and the game fields (map, units, buildings, players) are shared
across all three. Several map dimensions that used to be top-level constants now live *in the model*
because the player picks the map size (S/M/L) before each match.
-}


{-| Which player a unit or building belongs to. `0` is always the human; `1` and `2` are AI. -}
type alias PlayerId =
    Int


{-| The whole application state. -}
type alias Model =
    { screen : Screen

    -- pre-game choices (kept across games so the setup screen remembers them)
    , mapSize : MapSize
    , opponents : Int
    , seed : Int

    -- the live game
    , width : Int
    , height : Int
    , map : List Tile
    , units : List Unit
    , buildings : List Building
    , players : List Player
    , selected : List Int
    , mode : Mode
    , tick : Int
    , status : Status
    , message : String
    , history : List Sample
    , rng : Int
    , nextId : Int
    }


{-| Which part of the app is showing. -}
type Screen
    = SetupScreen
    | GameScreen
    | ResultScreen


{-| The three map sizes offered before a match. -}
type MapSize
    = Small
    | Medium
    | Large


{-| How the match is going. With opponents it is a fight to the last base; with zero opponents it is
an economic race to a power goal. `Draw` covers a tie on the tick limit. -}
type Status
    = Playing
    | Won
    | Lost
    | Draw


{-| What a click on the map should do. -}
type Mode
    = Normal
    | Placing BuildingKind


type Msg
    = Tick
      -- in-game commands
    | ClickTile Int Int
    | SelectUnit Int
    | SelectArmy
    | TrainWorker
    | TrainSoldier
    | StartBarracks
    | StartFarm
    | Cancel
      -- setup screen
    | SetMapSize MapSize
    | SetOpponents Int
    | Reroll
    | StartGame
      -- results screen
    | NewGame


{-| One map cell. `visible` is the human's fog-of-war: it flips to `True` once a friendly unit or
building reveals it and never flips back. -}
type alias Tile =
    { x : Int
    , y : Int
    , terrain : Terrain
    , visible : Bool
    }


type Terrain
    = Grass
    | Forest
    | GoldMine
    | Water
    | Rock


{-| A movable, ownable unit with combat stats. `tx`/`ty` is the move target; `path` is the remaining
tile waypoints toward it (a worker/soldier follows it and only re-plans when needed). A worker
`assigned` to a resource tile gathers it; a unit with an `attack` target (an enemy unit id) chases
and fights it. `cooldown` counts down between attacks. -}
type alias Unit =
    { id : Int
    , owner : PlayerId
    , x : Float
    , y : Float
    , tx : Float
    , ty : Float
    , path : List ( Int, Int )
    , kind : UnitKind
    , hp : Int
    , carrying : Int
    , assigned : Maybe ( Int, Int )
    , attack : Maybe Int
    , cooldown : Int
    }


type UnitKind
    = Worker
    | Soldier


type alias Building =
    { id : Int
    , owner : PlayerId
    , x : Int
    , y : Int
    , kind : BuildingKind
    , hp : Int
    }


type BuildingKind
    = Base
    | Barracks
    | Farm


{-| A competitor: the human or an AI. Resources are per-player. `kills` counts enemy units this
player has destroyed (used by the post-game rating). `defeated` is set once the player loses its
last building. -}
type alias Player =
    { id : PlayerId
    , isHuman : Bool
    , gold : Int
    , wood : Int
    , kills : Int
    , defeated : Bool
    }


{-| One snapshot of every player's "power" at a given tick, accumulated through the game and drawn as
a line chart on the results screen. `powers` is parallel to the player list (one entry per player,
in id order). -}
type alias Sample =
    { tick : Int
    , powers : List Int
    }


humanId : PlayerId
humanId =
    0



-- MAP SIZE -------------------------------------------------------------------------------------


sizeLabel : MapSize -> String
sizeLabel size =
    case size of
        Small ->
            "Small"

        Medium ->
            "Medium"

        Large ->
            "Large"


{-| Tile dimensions (width, height) for each map size. "Moderate-big" — large enough to maneuver
armies, small enough that per-tile pathfinding stays cheap in the browser. -}
sizeDims : MapSize -> ( Int, Int )
sizeDims size =
    case size of
        Small ->
            ( 24, 16 )

        Medium ->
            ( 32, 20 )

        Large ->
            ( 40, 26 )


{-| The economic power goal for a zero-opponent (sandbox) game on this map size: reach it before the
tick limit to win. Larger maps have more resources, so the bar is higher. -}
sizeGoal : MapSize -> Int
sizeGoal size =
    case size of
        Small ->
            1200

        Medium ->
            1600

        Large ->
            2200


tileSize : Int
tileSize =
    24


{-| Reach the win condition within this many ticks, or the game is decided on power (a draw if tied).
At 5 ticks/second this is four minutes. -}
tickLimit : Int
tickLimit =
    1200


{-| How far (Chebyshev distance, in tiles) a friendly unit/building clears the human's fog. -}
revealRadius : Int
revealRadius =
    3


{-| How close (Euclidean, in tiles) an enemy must be for an idle soldier to auto-engage it. -}
aggroRange : Float
aggroRange =
    4.0



-- COSTS ----------------------------------------------------------------------------------------


workerCost : Int
workerCost =
    50


barracksCost : Int
barracksCost =
    120


farmCost : Int
farmCost =
    90


soldierCost : Int
soldierCost =
    60


{-| How much a worker can carry before it must return to base to deposit. -}
carryCap : Int
carryCap =
    10


{-| How much a worker gathers per tick while standing on its assigned resource. -}
gatherRate : Int
gatherRate =
    2



-- COMBAT ---------------------------------------------------------------------------------------


maxHp : UnitKind -> Int
maxHp kind =
    case kind of
        Worker ->
            30

        Soldier ->
            60


attackDamage : UnitKind -> Int
attackDamage kind =
    case kind of
        Worker ->
            3

        Soldier ->
            9


{-| Strike range in tiles (Euclidean); soldiers reach a touch further than workers. -}
attackRange : UnitKind -> Float
attackRange kind =
    case kind of
        Worker ->
            1.2

        Soldier ->
            1.4


{-| Ticks between attacks. -}
attackCooldown : UnitKind -> Int
attackCooldown kind =
    case kind of
        Worker ->
            6

        Soldier ->
            4


buildingHp : BuildingKind -> Int
buildingHp kind =
    case kind of
        Base ->
            400

        Barracks ->
            250

        Farm ->
            120



-- POWER ----------------------------------------------------------------------------------------


{-| A unit's contribution to its owner's "power" score. -}
unitPower : UnitKind -> Int
unitPower kind =
    case kind of
        Worker ->
            25

        Soldier ->
            55


{-| A building's contribution to its owner's "power" score. -}
buildingPower : BuildingKind -> Int
buildingPower kind =
    case kind of
        Base ->
            200

        Barracks ->
            120

        Farm ->
            70



-- PLAYERS --------------------------------------------------------------------------------------


playerName : PlayerId -> String
playerName id =
    if id == humanId then
        "You"

    else
        "AI " ++ String.fromInt id


{-| Team colour for a player's units and buildings (and its power-chart line). -}
playerColor : PlayerId -> String
playerColor id =
    case id of
        0 ->
            "#38bdf8"

        1 ->
            "#f87171"

        _ ->
            "#a78bfa"



-- PALETTES -------------------------------------------------------------------------------------


buildingLabel : BuildingKind -> String
buildingLabel kind =
    case kind of
        Base ->
            "Base"

        Barracks ->
            "Barracks"

        Farm ->
            "Farm"


buildingColor : BuildingKind -> String
buildingColor kind =
    case kind of
        Base ->
            "#2563eb"

        Barracks ->
            "#b91c1c"

        Farm ->
            "#65a30d"


terrainColor : Terrain -> String
terrainColor terrain =
    case terrain of
        Grass ->
            "#4d7c3f"

        Forest ->
            "#1f5130"

        GoldMine ->
            "#caa72b"

        Water ->
            "#2b6cb0"

        Rock ->
            "#6b7280"


{-| Whether units can walk on this terrain (water and rock block movement). -}
passableTerrain : Terrain -> Bool
passableTerrain terrain =
    case terrain of
        Water ->
            False

        Rock ->
            False

        _ ->
            True
