# elm-rts — a tiny real-time strategy game in Elm

A small but functional RTS (in the spirit of early WarCraft / Command & Conquer): build buildings,
train units, gather resources and uncover the whole map. There is **no enemy AI and no multiplayer** —
the objective is to explore the entire map.

Written for the [elm-lang](https://github.com/tunguski/elm-lang) implementation of Elm and cleanly
split so the **model**, the **logic** and the **view** live in separate modules, with a **frontend**
(a browser app) and an optional **backend** (a server-side handler) that share one model module.

> Canonical home: <https://github.com/tunguski/elm-rts>. Built and showcased by elm-lang's example
> gallery.

## Layout

| Module | File | Role |
|---|---|---|
| `RTS.Model` | [src/RTS/Model.elm](src/RTS/Model.elm) | Data only: tiles, terrain, units, buildings, the `Msg` type and shared constants (map size, costs, colours). |
| `RTS.Logic` | [src/RTS/Logic.elm](src/RTS/Logic.elm) | Pure game rules: `init` (the generated map) and `update` — movement, fog-of-war reveal, gathering, training and building placement. No rendering, no effects. |
| `RTS.View` | [src/RTS/View.elm](src/RTS/View.elm) | Renders the model: the tile map (with fog), buildings and units as **SVG**, plus an HTML HUD (resources, build/train buttons, legend, messages). |
| `RTS.Main` | [src/RTS/Main.elm](src/RTS/Main.elm) | The frontend `Browser.element` program — wires `init`/`update`/`view` and a real-time clock (`Tick` 5×/second). |
| `RTS.Backend` | [backend/RTS/Backend.elm](backend/RTS/Backend.elm) | A server-side `handle : Request -> Response` sharing `RTS.Model`: a landing page, `/ping`, and `/api/map` (JSON world description). |

The frontend (`src/`) is the buildable game. The backend lives under `backend/` because it is a
*second* `main` (a server program) that cannot share one build with the browser `main`; see below.

## How to play

- **Click a unit** to select it, then **click a tile** to move it there (impassable water/rock is refused).
- Move **workers** onto a **gold mine** or **forest** to gather gold / wood (income while standing on the resource).
- **Train a worker** at the base (50 gold); **build a barracks** (120 gold) then **train soldiers** (60 gold).
  Buildings and units clear the fog around them.
- **Win** by revealing every tile.

## Build & run

You need the [elm-lang](https://github.com/tunguski/elm-lang) CLI. Point `ELM` at it (the `elm.sh`
wrapper, `java -jar elm.jar`, or the native binary).

### Frontend (the playable game)

```sh
elm make src/RTS/Main.elm --project=elm.json -o build/rts.html
# then open build/rts.html in a browser
```

Dependencies (`elm/browser`, `elm/html`, `elm/svg`, `elm/time`, `elm/core`) are listed in
[elm.json](elm.json) and were added with the elm-lang package manager (`elm install elm/svg --elm`).

### Backend (optional)

The backend is a pure `handle : Request -> Response` that shares `RTS.Model`. It uses the `Server`
module, which elm-lang's `elm server` runner supplies. A copy of that interface is vendored at
[backend/Server.elm](backend/Server.elm) so the handler **type-checks in isolation**:

```sh
elm check backend/Server.elm src/RTS/Model.elm backend/RTS/Backend.elm
```

Its routes:

- `GET /` — an HTML landing page with the rules,
- `GET /ping` — `pong`,
- `GET /api/map` — a JSON description of the map size, costs and terrain palette.

Because the browser frontend and the server backend each expose a `main`, they are kept in separate
source roots (`src/` vs `backend/`) and built separately.
