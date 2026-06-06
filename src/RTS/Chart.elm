module RTS.Chart exposing (view)

{-| A small, dependency-free SVG line chart of "player power over time", drawn on the results screen
from the `Sample` history collected during the match. One coloured polyline per player (in the
player's team colour), a couple of gridlines, and axis labels for the peak power and the final tick.

Pure: it takes the data and returns `Svg`, so it is trivially previewable and never holds state.
-}

import Html.Attributes as HA
import RTS.Model exposing (..)
import Svg exposing (Svg, line, polyline, svg, text_)
import Svg.Attributes as SA


padL : Float
padL =
    44


padR : Float
padR =
    12


padT : Float
padT =
    14


padB : Float
padB =
    26


{-| Render the chart at the given pixel size from the ordered player ids and the sample history. -}
view : Float -> Float -> List PlayerId -> List Sample -> Svg msg
view width height ids history =
    let
        n =
            List.length history

        plotW =
            width - padL - padR

        plotH =
            height - padT - padB

        maxPower =
            history
                |> List.concatMap .powers
                |> List.maximum
                |> Maybe.withDefault 1
                |> max 1

        lastTick =
            history
                |> List.reverse
                |> List.head
                |> Maybe.map .tick
                |> Maybe.withDefault 0

        xAt i =
            padL + plotW * toFloat i / toFloat (max 1 (n - 1))

        yAt p =
            padT + plotH * (1 - toFloat p / toFloat maxPower)

        series =
            List.indexedMap (\seriesIdx pid -> seriesLine seriesIdx pid xAt yAt history) ids
    in
    svg
        [ SA.width (ftoi width)
        , SA.height (ftoi height)
        , HA.style "background" "#0b1220"
        , HA.style "border-radius" "8px"
        ]
        (frame plotW plotH maxPower lastTick xAt yAt ++ series)


{-| The polyline for one player's series, in that player's colour. -}
seriesLine : Int -> PlayerId -> (Int -> Float) -> (Int -> Float) -> List Sample -> Svg msg
seriesLine seriesIdx pid xAt yAt history =
    let
        pts =
            List.indexedMap
                (\i sample ->
                    ftoi (xAt i) ++ "," ++ ftoi (yAt (nth seriesIdx 0 sample.powers))
                )
                history
    in
    polyline
        [ SA.points (String.join " " pts)
        , SA.fill "none"
        , SA.stroke (playerColor pid)
        , SA.strokeWidth "2.5"
        ]
        []


{-| Axes, two faint gridlines and the labels (peak power, final time). -}
frame : Float -> Float -> Int -> Int -> (Int -> Float) -> (Int -> Float) -> List (Svg msg)
frame plotW plotH maxPower lastTick xAt yAt =
    let
        axisColor =
            "#334155"

        x0 =
            padL

        y0 =
            padT + plotH
    in
    [ axis x0 padT x0 y0 axisColor
    , axis x0 y0 (padL + plotW) y0 axisColor
    , gridline x0 (padL + plotW) (padT + plotH / 2) "#1e293b"
    , label 6 (padT + 4) "#94a3b8" "start" (String.fromInt maxPower)
    , label 6 (y0 + 4) "#94a3b8" "start" "0"
    , label (padL + plotW) (y0 + 18) "#94a3b8" "end" ("tick " ++ String.fromInt lastTick)
    , label x0 (y0 + 18) "#94a3b8" "start" "0"
    , label (padL + plotW / 2) (padT + 2) "#cbd5e1" "middle" "Power over time"
    ]


axis : Float -> Float -> Float -> Float -> String -> Svg msg
axis x1 y1 x2 y2 color =
    line
        [ SA.x1 (ftoi x1), SA.y1 (ftoi y1), SA.x2 (ftoi x2), SA.y2 (ftoi y2), SA.stroke color, SA.strokeWidth "1" ]
        []


gridline : Float -> Float -> Float -> String -> Svg msg
gridline x1 x2 y color =
    line
        [ SA.x1 (ftoi x1), SA.y1 (ftoi y), SA.x2 (ftoi x2), SA.y2 (ftoi y), SA.stroke color, SA.strokeWidth "1" ]
        []


label : Float -> Float -> String -> String -> String -> Svg msg
label x y color anchor content =
    text_
        [ SA.x (ftoi x)
        , SA.y (ftoi y)
        , SA.fill color
        , SA.fontSize "10"
        , SA.textAnchor anchor
        , SA.fontFamily "system-ui, sans-serif"
        ]
        [ Svg.text content ]


ftoi : Float -> String
ftoi f =
    String.fromInt (round f)


nth : Int -> a -> List a -> a
nth i default xs =
    case List.head (List.drop i xs) of
        Just x ->
            x

        Nothing ->
            default
