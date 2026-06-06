module RTS.Rating exposing (Result, Ranking, rate)

{-| Post-game scoring. From the final model and outcome it builds a ranked scoreboard (one row per
player: power, kills, surviving units/buildings) plus the human's overall result — a numeric score, a
letter grade, and an Elo-style rating change as if every match were rated against an even field.

Kept separate from the game rules so the scoring formula is easy to read, tweak and test in isolation.
-}

import RTS.Logic as Logic
import RTS.Model exposing (..)


{-| One row of the end-game scoreboard. -}
type alias Ranking =
    { id : PlayerId
    , place : Int
    , power : Int
    , kills : Int
    , units : Int
    , buildings : Int
    , alive : Bool
    }


{-| The full result: the ranked table, and the human's headline numbers. -}
type alias Result =
    { rankings : List Ranking
    , grade : String
    , score : Int
    , ratingDelta : Int
    , status : Status
    }


rate : Status -> Model -> Result
rate status model =
    let
        rows =
            rankings model

        humanRow =
            List.head (List.filter (\r -> r.id == humanId) rows)

        score =
            humanScore status model humanRow

        delta =
            ratingDelta status model
    in
    { rankings = rows
    , grade = grade status score
    , score = score
    , ratingDelta = delta
    , status = status
    }


{-| Every player as a scoreboard row, sorted survivors-first then by power (so the winner leads), with
1-based places assigned. -}
rankings : Model -> List Ranking
rankings model =
    let
        rows =
            List.map
                (\p ->
                    { id = p.id
                    , place = 0
                    , power = Logic.playerPower p.id model
                    , kills = p.kills
                    , units = List.length (Logic.unitsOf p.id model)
                    , buildings = List.length (Logic.buildingsOf p.id model)
                    , alive = not p.defeated
                    }
                )
                model.players

        sorted =
            List.sortBy (\r -> ( aliveRank r.alive, negate r.power )) rows
    in
    List.indexedMap (\i r -> { r | place = i + 1 }) sorted


aliveRank : Bool -> Int
aliveRank alive =
    if alive then
        0

    else
        1


{-| The human's score: final power, a bounty per kill, a win bonus, and a speed bonus for winning
with time to spare. -}
humanScore : Status -> Model -> Maybe Ranking -> Int
humanScore status model humanRow =
    let
        power =
            case humanRow of
                Just r ->
                    r.power

                Nothing ->
                    0

        kills =
            case humanRow of
                Just r ->
                    r.kills

                Nothing ->
                    0

        winBonus =
            case status of
                Won ->
                    500

                Draw ->
                    150

                _ ->
                    0

        speedBonus =
            if status == Won then
                max 0 ((tickLimit - model.tick) // 2)

            else
                0
    in
    power + kills * 20 + winBonus + speedBonus


{-| A letter grade from the outcome and score. -}
grade : Status -> Int -> String
grade status score =
    case status of
        Won ->
            if score >= 2600 then
                "S"

            else if score >= 1900 then
                "A"

            else
                "B"

        Draw ->
            "C"

        Lost ->
            if score >= 1000 then
                "D"

            else
                "F"

        Playing ->
            "-"


{-| An Elo-style rating change versus an even (1000) field: win +16, draw 0, loss −16 in the simple
balanced case, nudged by how the human's final power compared with the average. -}
ratingDelta : Status -> Model -> Int
ratingDelta status model =
    let
        outcome =
            case status of
                Won ->
                    1.0

                Draw ->
                    0.5

                Lost ->
                    0.0

                Playing ->
                    0.5

        k =
            32.0
    in
    round (k * (outcome - 0.5))
