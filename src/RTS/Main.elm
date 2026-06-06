module RTS.Main exposing (main)

{-| The RTS frontend: a `Browser.element` program wiring the initial model and pure `update` from
`RTS.Game`, the screen-aware `view` from `RTS.View`, and a real-time clock that fires `Tick` five
times a second while a match is in progress. Build it with

    elm make src/RTS/Main.elm --project=elm.json -o build/rts.html

-}

import Browser
import RTS.Game as Game
import RTS.Model exposing (..)
import RTS.View as View
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Game.init, Cmd.none )
        , update = \msg model -> ( Game.update msg model, Cmd.none )
        , view = View.view
        , subscriptions = \_ -> Time.every 200 (\_ -> Tick)
        }
