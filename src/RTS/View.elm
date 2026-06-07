module RTS.View exposing (view)

{-| Rendering for all three screens — the pre-game **setup** (map size + opponents + seed), the live
**game** (an SVG battlefield with a side HUD of resources, live power standings, build/train commands
and a minimap), and the post-game **results** (outcome banner, grade, an Elo-style rating change, a
scoreboard, and the power-history line chart). Pure: it reads the model and emits `Html Msg`.
-}

import Html exposing (Html, button, div, span, table, td, text, th, tr)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import RTS.Chart as Chart
import RTS.Logic as Logic
import RTS.Model exposing (..)
import RTS.Rating as Rating
import Set exposing (Set)
import Svg exposing (circle, rect, svg)
import Svg.Attributes as SA


view : Model -> Html Msg
view model =
    div
        [ HA.style "font-family" "system-ui, -apple-system, Segoe UI, sans-serif"
        , HA.style "background" "radial-gradient(1200px 800px at 30% -10%, #1e293b, #020617)"
        , HA.style "color" "#e2e8f0"
        , HA.style "min-height" "100vh"
        , HA.style "padding" "20px"
        , HA.style "box-sizing" "border-box"
        ]
        [ case model.screen of
            SetupScreen ->
                setupView model

            GameScreen ->
                gameView model

            ResultScreen ->
                resultView model
        ]



-- SETUP SCREEN ---------------------------------------------------------------------------------


setupView : Model -> Html Msg
setupView model =
    div
        [ HA.style "max-width" "560px"
        , HA.style "margin" "6vh auto"
        , HA.style "background" "rgba(15,23,42,0.7)"
        , HA.style "border" "1px solid #1e293b"
        , HA.style "border-radius" "16px"
        , HA.style "padding" "28px 30px"
        , HA.style "box-shadow" "0 20px 60px rgba(0,0,0,0.45)"
        ]
        [ div [ HA.style "font-size" "30px", HA.style "font-weight" "800", HA.style "letter-spacing" "-0.5px" ] [ text "Elm RTS" ]
        , div [ HA.style "color" "#94a3b8", HA.style "margin" "4px 0 22px" ]
            [ text "Build an economy, raise an army, and outlast your rivals. Choose your battlefield." ]
        , choiceGroup "Map size"
            (List.map (sizeButton model.mapSize) [ Small, Medium, Large ])
        , dimsHint model.mapSize
        , choiceGroup "Opponents"
            (List.map (opponentButton model.opponents) [ 0, 1, 2 ])
        , opponentsHint model.opponents
        , div [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "12px", HA.style "margin" "20px 0 4px" ]
            [ span [ HA.style "color" "#94a3b8", HA.style "font-size" "13px" ] [ text ("Map seed: " ++ String.fromInt model.seed) ]
            , smallButton "Reroll" Reroll
            ]
        , button
            [ onClick StartGame
            , HA.style "margin-top" "22px"
            , HA.style "width" "100%"
            , HA.style "padding" "14px"
            , HA.style "border" "none"
            , HA.style "border-radius" "10px"
            , HA.style "background" "linear-gradient(90deg,#2563eb,#38bdf8)"
            , HA.style "color" "white"
            , HA.style "font-size" "16px"
            , HA.style "font-weight" "700"
            , HA.style "cursor" "pointer"
            ]
            [ text "Start game" ]
        ]


choiceGroup : String -> List (Html Msg) -> Html Msg
choiceGroup title buttons =
    div [ HA.style "margin-bottom" "6px" ]
        [ div [ HA.style "font-size" "13px", HA.style "color" "#cbd5e1", HA.style "font-weight" "600", HA.style "margin" "14px 0 8px" ] [ text title ]
        , div [ HA.style "display" "flex", HA.style "gap" "10px" ] buttons
        ]


sizeButton : MapSize -> MapSize -> Html Msg
sizeButton current size =
    toggle (current == size) (sizeLabel size) (SetMapSize size)


opponentButton : Int -> Int -> Html Msg
opponentButton current n =
    toggle (current == n) (String.fromInt n) (SetOpponents n)


toggle : Bool -> String -> Msg -> Html Msg
toggle active label msg =
    button
        [ onClick msg
        , HA.style "flex" "1"
        , HA.style "padding" "12px"
        , HA.style "border" "1px solid #1e293b"
        , HA.style "border-radius" "10px"
        , HA.style "cursor" "pointer"
        , HA.style "font-size" "14px"
        , HA.style "font-weight" "600"
        , HA.style "background"
            (if active then
                "#2563eb"

             else
                "#0b1220"
            )
        , HA.style "color"
            (if active then
                "white"

             else
                "#94a3b8"
            )
        ]
        [ text label ]


dimsHint : MapSize -> Html Msg
dimsHint size =
    let
        ( w, h ) =
            sizeDims size
    in
    hint (String.fromInt w ++ " × " ++ String.fromInt h ++ " tiles")


opponentsHint : Int -> Html Msg
opponentsHint n =
    hint
        (if n == 0 then
            "Solo: race to a power goal before the clock runs out."

         else if n == 1 then
            "One AI rival — destroy its base to win."

         else
            "Two AI rivals — be the last base standing."
        )


hint : String -> Html Msg
hint t =
    div [ HA.style "color" "#64748b", HA.style "font-size" "12px", HA.style "margin-top" "6px" ] [ text t ]


smallButton : String -> Msg -> Html Msg
smallButton label msg =
    button
        [ onClick msg
        , HA.style "padding" "6px 12px"
        , HA.style "border" "1px solid #334155"
        , HA.style "border-radius" "8px"
        , HA.style "background" "#0b1220"
        , HA.style "color" "#e2e8f0"
        , HA.style "cursor" "pointer"
        , HA.style "font-size" "12px"
        ]
        [ text label ]



-- GAME SCREEN ----------------------------------------------------------------------------------


gameView : Model -> Html Msg
gameView model =
    let
        -- Explored-tile set computed once and shared by the battlefield and the minimap.
        seen =
            visibleCoords model
    in
    div [ HA.style "display" "flex", HA.style "gap" "16px", HA.style "flex-wrap" "wrap", HA.style "align-items" "flex-start" ]
        [ battlefield seen model
        , hud seen model
        ]


battlefield : Set ( Int, Int ) -> Model -> Html Msg
battlefield seen model =
    svg
        [ SA.width (px (model.width * tileSize))
        , SA.height (px (model.height * tileSize))
        , HA.style "background" "#020617"
        , HA.style "border-radius" "10px"
        , HA.style "box-shadow" "0 10px 40px rgba(0,0,0,0.5)"
        , HA.style "max-width" "100%"
        ]
        (List.map tileRect model.map
            ++ List.map buildingRect (List.filter (shownBuilding seen) model.buildings)
            ++ List.concatMap (unitShapes model) (List.filter (shownUnit seen) model.units)
        )


{-| The set of tiles the human has explored (fog cleared). -}
visibleCoords : Model -> Set ( Int, Int )
visibleCoords model =
    List.foldl
        (\t s ->
            if t.visible then
                Set.insert ( t.x, t.y ) s

            else
                s
        )
        Set.empty
        model.map


{-| Your own units/buildings are always drawn; an enemy's is hidden until you've explored its tile,
so nothing shows through the fog of war. -}
shownUnit : Set ( Int, Int ) -> Unit -> Bool
shownUnit seen u =
    u.owner == humanId || Set.member ( round u.x, round u.y ) seen


shownBuilding : Set ( Int, Int ) -> Building -> Bool
shownBuilding seen b =
    b.owner == humanId || Set.member ( b.x, b.y ) seen


tileRect : Tile -> Html Msg
tileRect t =
    rect
        [ SA.x (px (t.x * tileSize))
        , SA.y (px (t.y * tileSize))
        , SA.width (px tileSize)
        , SA.height (px tileSize)
        , SA.fill
            (if t.visible then
                terrainColor t.terrain

             else
                "#0a0f1c"
            )
        , SA.stroke "#0b1220"
        , SA.strokeWidth "1"
        , onClick (ClickTile t.x t.y)
        ]
        []


buildingRect : Building -> Html Msg
buildingRect b =
    rect
        [ SA.x (px (b.x * tileSize + 2))
        , SA.y (px (b.y * tileSize + 2))
        , SA.width (px (tileSize - 4))
        , SA.height (px (tileSize - 4))
        , SA.rx "4"
        , SA.fill (buildingColor b.kind)
        , SA.stroke (playerColor b.owner)
        , SA.strokeWidth "3"
        , onClick (ClickTile b.x b.y)
        ]
        []


{-| A unit: a team-coloured disc (workers smaller than soldiers), a selection ring when selected, and
a thin health bar when hurt. -}
unitShapes : Model -> Unit -> List (Html Msg)
unitShapes model u =
    let
        cx =
            u.x * toFloat tileSize + toFloat tileSize / 2

        cy =
            u.y * toFloat tileSize + toFloat tileSize / 2

        r =
            case u.kind of
                Worker ->
                    toFloat tileSize * 0.28

                Soldier ->
                    toFloat tileSize * 0.38

        selected =
            List.member u.id model.selected

        body =
            circle
                [ SA.cx (fpx cx)
                , SA.cy (fpx cy)
                , SA.r (fpx r)
                , SA.fill (playerColor u.owner)
                , SA.stroke
                    (if selected then
                        "#fde047"

                     else
                        "#0b1220"
                    )
                , SA.strokeWidth
                    (if selected then
                        "3"

                     else
                        "1.5"
                    )
                , onClick (SelectUnit u.id)
                ]
                []

        kindMark =
            case u.kind of
                Soldier ->
                    [ circle [ SA.cx (fpx cx), SA.cy (fpx cy), SA.r (fpx (r * 0.4)), SA.fill "#0b1220", onClick (SelectUnit u.id) ] [] ]

                Worker ->
                    if u.carrying > 0 then
                        [ circle [ SA.cx (fpx cx), SA.cy (fpx (cy - r - 2)), SA.r "2.5", SA.fill "#facc15" ] [] ]

                    else
                        []

        hpBar =
            if u.hp < maxHp u.kind then
                healthBar cx (cy - r - 5) (toFloat u.hp / toFloat (maxHp u.kind))

            else
                []
    in
    body :: kindMark ++ hpBar


healthBar : Float -> Float -> Float -> List (Html Msg)
healthBar cx cy frac =
    let
        w =
            16.0
    in
    [ rect [ SA.x (fpx (cx - w / 2)), SA.y (fpx cy), SA.width (fpx w), SA.height "3", SA.fill "#1e293b", SA.rx "1" ] []
    , rect [ SA.x (fpx (cx - w / 2)), SA.y (fpx cy), SA.width (fpx (w * frac)), SA.height "3", SA.fill (healthColor frac), SA.rx "1" ] []
    ]


healthColor : Float -> String
healthColor frac =
    if frac > 0.5 then
        "#4ade80"

    else if frac > 0.25 then
        "#facc15"

    else
        "#f87171"



-- HUD ------------------------------------------------------------------------------------------


hud : Set ( Int, Int ) -> Model -> Html Msg
hud seen model =
    div
        [ HA.style "min-width" "260px"
        , HA.style "max-width" "300px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "8px"
        , HA.style "background" "rgba(15,23,42,0.7)"
        , HA.style "border" "1px solid #1e293b"
        , HA.style "border-radius" "12px"
        , HA.style "padding" "16px"
        ]
        [ div [ HA.style "font-size" "18px", HA.style "font-weight" "800" ] [ text "Command" ]
        , resourceRow model
        , div [ HA.style "color" "#94a3b8", HA.style "font-size" "12px" ] [ text (clock model) ]
        , divider
        , sectionTitle "Standings"
        , standings model
        , divider
        , sectionTitle "Build & train"
        , btn ("Train Worker (" ++ String.fromInt workerCost ++ "g)") TrainWorker
        , btn ("Train Soldier (" ++ String.fromInt soldierCost ++ "g)") TrainSoldier
        , btn ("Build Barracks (" ++ String.fromInt barracksCost ++ "g)") StartBarracks
        , btn ("Build Farm (" ++ String.fromInt farmCost ++ "g, +income)") StartFarm
        , btn "Select whole army" SelectArmy
        , btn "Cancel / Deselect" Cancel
        , divider
        , div [ HA.style "min-height" "20px", HA.style "font-size" "13px", HA.style "color" "#cbd5e1" ] [ text model.message ]
        , divider
        , sectionTitle "Minimap"
        , minimap seen model
        , divider
        , legend
        ]


resourceRow : Model -> Html Msg
resourceRow model =
    let
        p =
            Logic.player humanId model
    in
    div [ HA.style "display" "flex", HA.style "gap" "16px", HA.style "font-size" "15px" ]
        [ stat "Gold" (Maybe.map .gold p |> Maybe.withDefault 0) "#facc15"
        , stat "Wood" (Maybe.map .wood p |> Maybe.withDefault 0) "#84cc16"
        ]


stat : String -> Int -> String -> Html Msg
stat label amount color =
    div []
        [ span [ HA.style "color" color, HA.style "font-weight" "700" ] [ text (String.fromInt amount) ]
        , span [ HA.style "color" "#94a3b8", HA.style "font-size" "12px", HA.style "margin-left" "4px" ] [ text label ]
        ]


clock : Model -> String
clock model =
    let
        secs =
            model.tick // 5
    in
    "Time " ++ String.fromInt secs ++ "s · tick " ++ String.fromInt model.tick


{-| Live per-player power bars, longest = strongest, defeated players struck through. -}
standings : Model -> Html Msg
standings model =
    let
        rows =
            List.sortBy .id model.players

        powers =
            List.map (\p -> Logic.playerPower p.id model) rows

        maxP =
            List.maximum powers |> Maybe.withDefault 1 |> max 1
    in
    div [ HA.style "display" "flex", HA.style "flex-direction" "column", HA.style "gap" "6px" ]
        (List.map (\p -> standingRow p (Logic.playerPower p.id model) maxP) rows)


standingRow : Player -> Int -> Int -> Html Msg
standingRow p power maxP =
    div [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "8px", HA.style "font-size" "12px" ]
        [ span
            [ HA.style "width" "54px"
            , HA.style "color" (playerColor p.id)
            , HA.style "font-weight" "700"
            , HA.style "text-decoration"
                (if p.defeated then
                    "line-through"

                 else
                    "none"
                )
            ]
            [ text (playerName p.id) ]
        , div [ HA.style "flex" "1", HA.style "background" "#0b1220", HA.style "border-radius" "4px", HA.style "height" "10px", HA.style "overflow" "hidden" ]
            [ div
                [ HA.style "height" "10px"
                , HA.style "border-radius" "4px"
                , HA.style "background" (playerColor p.id)
                , HA.style "width" (String.fromInt (100 * power // maxP) ++ "%")
                ]
                []
            ]
        , span [ HA.style "width" "40px", HA.style "text-align" "right", HA.style "color" "#cbd5e1" ] [ text (String.fromInt power) ]
        ]



-- MINIMAP --------------------------------------------------------------------------------------


minimap : Set ( Int, Int ) -> Model -> Html Msg
minimap seen model =
    let
        scale =
            max 3 (180 // max 1 model.width)
    in
    svg
        [ SA.width (px (model.width * scale))
        , SA.height (px (model.height * scale))
        , HA.style "background" "#020617"
        , HA.style "border-radius" "6px"
        ]
        (List.map (miniTile scale) model.map
            ++ List.map (miniBuilding scale) (List.filter (shownBuilding seen) model.buildings)
            ++ List.map (miniUnit scale) (List.filter (shownUnit seen) model.units)
        )


miniTile : Int -> Tile -> Html Msg
miniTile scale t =
    rect
        [ SA.x (px (t.x * scale))
        , SA.y (px (t.y * scale))
        , SA.width (px scale)
        , SA.height (px scale)
        , SA.fill
            (if t.visible then
                terrainColor t.terrain

             else
                "#0a0f1c"
            )
        ]
        []


miniBuilding : Int -> Building -> Html Msg
miniBuilding scale b =
    rect
        [ SA.x (px (b.x * scale))
        , SA.y (px (b.y * scale))
        , SA.width (px (scale + 1))
        , SA.height (px (scale + 1))
        , SA.fill (playerColor b.owner)
        ]
        []


miniUnit : Int -> Unit -> Html Msg
miniUnit scale u =
    circle
        [ SA.cx (px (round u.x * scale + scale // 2))
        , SA.cy (px (round u.y * scale + scale // 2))
        , SA.r (px (max 1 (scale // 2)))
        , SA.fill (playerColor u.owner)
        ]
        []



-- RESULTS SCREEN -------------------------------------------------------------------------------


resultView : Model -> Html Msg
resultView model =
    let
        result =
            Rating.rate model.status model

        ids =
            List.sort (List.map .id model.players)
    in
    div
        [ HA.style "max-width" "720px"
        , HA.style "margin" "4vh auto"
        , HA.style "background" "rgba(15,23,42,0.75)"
        , HA.style "border" "1px solid #1e293b"
        , HA.style "border-radius" "16px"
        , HA.style "padding" "28px 30px"
        , HA.style "box-shadow" "0 20px 60px rgba(0,0,0,0.45)"
        ]
        [ banner model.status
        , gradeRow result
        , divider
        , sectionTitle "Scoreboard"
        , scoreboard result
        , divider
        , sectionTitle "Power history"
        , div [ HA.style "margin-top" "8px" ] [ Chart.view 640 240 ids model.history ]
        , chartLegend model
        , button
            [ onClick NewGame
            , HA.style "margin-top" "22px"
            , HA.style "width" "100%"
            , HA.style "padding" "14px"
            , HA.style "border" "none"
            , HA.style "border-radius" "10px"
            , HA.style "background" "linear-gradient(90deg,#2563eb,#38bdf8)"
            , HA.style "color" "white"
            , HA.style "font-size" "16px"
            , HA.style "font-weight" "700"
            , HA.style "cursor" "pointer"
            ]
            [ text "New game" ]
        ]


banner : Status -> Html Msg
banner status =
    let
        ( label, color ) =
            case status of
                Won ->
                    ( "Victory", "#4ade80" )

                Lost ->
                    ( "Defeat", "#f87171" )

                Draw ->
                    ( "Draw", "#facc15" )

                Playing ->
                    ( "", "#e2e8f0" )
    in
    div [ HA.style "font-size" "40px", HA.style "font-weight" "900", HA.style "color" color, HA.style "letter-spacing" "-1px" ] [ text label ]


gradeRow : Rating.Result -> Html Msg
gradeRow result =
    div [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "20px", HA.style "margin-top" "10px" ]
        [ div
            [ HA.style "width" "64px"
            , HA.style "height" "64px"
            , HA.style "border-radius" "14px"
            , HA.style "background" "#0b1220"
            , HA.style "border" "2px solid #334155"
            , HA.style "display" "flex"
            , HA.style "align-items" "center"
            , HA.style "justify-content" "center"
            , HA.style "font-size" "34px"
            , HA.style "font-weight" "900"
            , HA.style "color" "#38bdf8"
            ]
            [ text result.grade ]
        , div []
            [ div [ HA.style "font-size" "14px", HA.style "color" "#94a3b8" ] [ text "Score" ]
            , div [ HA.style "font-size" "24px", HA.style "font-weight" "800" ] [ text (String.fromInt result.score) ]
            ]
        , div []
            [ div [ HA.style "font-size" "14px", HA.style "color" "#94a3b8" ] [ text "Rating" ]
            , div
                [ HA.style "font-size" "24px"
                , HA.style "font-weight" "800"
                , HA.style "color"
                    (if result.ratingDelta >= 0 then
                        "#4ade80"

                     else
                        "#f87171"
                    )
                ]
                [ text (signed result.ratingDelta) ]
            ]
        ]


signed : Int -> String
signed n =
    if n >= 0 then
        "+" ++ String.fromInt n

    else
        String.fromInt n


scoreboard : Rating.Result -> Html Msg
scoreboard result =
    table [ HA.style "width" "100%", HA.style "border-collapse" "collapse", HA.style "font-size" "13px", HA.style "margin-top" "8px" ]
        (headerRow :: List.map scoreRow result.rankings)


headerRow : Html Msg
headerRow =
    tr [ HA.style "color" "#94a3b8", HA.style "text-align" "left" ]
        [ hcell "#", hcell "Player", hcell "Power", hcell "Kills", hcell "Units", hcell "Bld", hcell "Status" ]


hcell : String -> Html Msg
hcell t =
    th [ HA.style "padding" "6px 8px", HA.style "border-bottom" "1px solid #1e293b", HA.style "font-weight" "600" ] [ text t ]


scoreRow : Rating.Ranking -> Html Msg
scoreRow r =
    tr []
        [ cell (String.fromInt r.place)
        , cellColored (playerName r.id) (playerColor r.id)
        , cell (String.fromInt r.power)
        , cell (String.fromInt r.kills)
        , cell (String.fromInt r.units)
        , cell (String.fromInt r.buildings)
        , cell
            (if r.alive then
                "alive"

             else
                "defeated"
            )
        ]


cell : String -> Html Msg
cell t =
    td [ HA.style "padding" "6px 8px", HA.style "border-bottom" "1px solid #111c30" ] [ text t ]


cellColored : String -> String -> Html Msg
cellColored t color =
    td [ HA.style "padding" "6px 8px", HA.style "border-bottom" "1px solid #111c30", HA.style "color" color, HA.style "font-weight" "700" ] [ text t ]


chartLegend : Model -> Html Msg
chartLegend model =
    div [ HA.style "display" "flex", HA.style "gap" "16px", HA.style "margin-top" "8px", HA.style "font-size" "12px", HA.style "color" "#94a3b8" ]
        (List.map
            (\p ->
                div [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "6px" ]
                    [ span [ HA.style "width" "14px", HA.style "height" "3px", HA.style "background" (playerColor p.id), HA.style "display" "inline-block" ] []
                    , text (playerName p.id)
                    ]
            )
            (List.sortBy .id model.players)
        )



-- SHARED ---------------------------------------------------------------------------------------


sectionTitle : String -> Html Msg
sectionTitle t =
    div [ HA.style "font-size" "12px", HA.style "font-weight" "700", HA.style "color" "#cbd5e1", HA.style "text-transform" "uppercase", HA.style "letter-spacing" "0.5px" ] [ text t ]


btn : String -> Msg -> Html Msg
btn label msg =
    button
        [ onClick msg
        , HA.style "padding" "9px 10px"
        , HA.style "border" "1px solid #1e293b"
        , HA.style "border-radius" "8px"
        , HA.style "background" "#0b1220"
        , HA.style "color" "#e2e8f0"
        , HA.style "cursor" "pointer"
        , HA.style "text-align" "left"
        , HA.style "font-size" "13px"
        ]
        [ text label ]


legend : Html Msg
legend =
    div [ HA.style "font-size" "12px", HA.style "color" "#94a3b8", HA.style "display" "flex", HA.style "flex-direction" "column", HA.style "gap" "3px" ]
        [ sectionTitle "Legend"
        , swatch (terrainColor GoldMine) "Gold mine"
        , swatch (terrainColor Forest) "Forest (wood)"
        , swatch (terrainColor Water) "Water (impassable)"
        , swatch (playerColor humanId) "You"
        , swatch (playerColor 1) "Enemy"
        ]


swatch : String -> String -> Html Msg
swatch color label =
    div [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "6px" ]
        [ span [ HA.style "width" "12px", HA.style "height" "12px", HA.style "border-radius" "3px", HA.style "display" "inline-block", HA.style "background" color ] []
        , text label
        ]


divider : Html Msg
divider =
    div [ HA.style "height" "1px", HA.style "background" "#1e293b", HA.style "margin" "6px 0" ] []


px : Int -> String
px n =
    String.fromInt n


fpx : Float -> String
fpx f =
    String.fromInt (round f)
