# elm-rts — a real-time strategy game in Elm

A small but genuinely playable RTS (in the spirit of early WarCraft / Command & Conquer): pick a map
size and a number of AI opponents, generate a fresh map, build an economy, raise an army, and fight
to be the last base standing. After the match you get a **rating** and a **line chart of every
player's power over the course of the game**.

Written for the [elm-lang](https://github.com/tunguski/elm-lang) implementation of Elm and cleanly
split so the data, the rules, the AI, the scoring and the rendering each live in their own module —
with a **frontend** (the browser game) and an optional **backend** (a server-side handler) that share
the `RTS.Model` module.

> Canonical home: <https://github.com/tunguski/elm-rts>. Built and showcased by elm-lang's example
> gallery.

## Features

- **Pre-game setup** — choose the **map size** (Small / Medium / Large) and **0–2 AI opponents**.
- **Procedural maps** — a seeded generator grows water, rock and forest, scatters gold mines, and
  carves a fair, buildable start (with a nearby gold mine and forest) for every player. "Reroll" for
  a new seed; the same seed always gives the same map.
- **Algorithmic AI** (no LLM) — each opponent assigns workers, follows a build order
  (workers → barracks → soldiers, plus a farm for income) and pushes an army at the nearest enemy
  once it's mustered one. Defense is emergent: idle soldiers auto-engage anything in range.
- **Combat** — units and buildings have hit points; soldiers (and, weakly, workers) deal damage on a
  cooldown. Lose your last building and you're eliminated.
- **Two win conditions** — with opponents, destroy every enemy base; solo (0 opponents), race to a
  power goal before the clock runs out. The time limit is decided on power (a tie is a draw).
- **Rating & power chart** — a post-game scoreboard, a letter grade, an Elo-style rating change, and
  an SVG line chart of each player's power sampled throughout the match.
- **Fog of war**, a minimap, and live power standings in the HUD.

## Layout

| Module | File | Role |
|---|---|---|
| `RTS.Model` | [src/RTS/Model.elm](src/RTS/Model.elm) | Data only: tiles, units, buildings, players, the `Msg`/`Screen`/`Status` types and the shared constants (sizes, costs, combat stats, colours). |
| `RTS.Rng` | [src/RTS/Rng.elm](src/RTS/Rng.elm) | A tiny deterministic PRNG (a seeded LCG) — no `elm/random` dependency, fully reproducible. |
| `RTS.Map` | [src/RTS/Map.elm](src/RTS/Map.elm) | The seeded map generator: terrain blobs, gold mines, and carved player starts. |
| `RTS.Logic` | [src/RTS/Logic.elm](src/RTS/Logic.elm) | Pure mechanics shared by the game and the AI: BFS pathfinding, movement/chasing, combat, gathering, fog, power, and the owner-parameterised command/build/train helpers. |
| `RTS.Ai` | [src/RTS/Ai.elm](src/RTS/Ai.elm) | The opponent: a deterministic economy + build-order + attack algorithm built on `RTS.Logic`. |
| `RTS.Rating` | [src/RTS/Rating.elm](src/RTS/Rating.elm) | Post-game scoring: the ranked scoreboard, a letter grade and an Elo-style rating delta. |
| `RTS.Game` | [src/RTS/Game.elm](src/RTS/Game.elm) | Orchestration: `init`, `update`, the real-time `tick` (the order of the world step), setup, win/lose, and power-history sampling. |
| `RTS.Chart` | [src/RTS/Chart.elm](src/RTS/Chart.elm) | A dependency-free SVG line chart of player power over time. |
| `RTS.View` | [src/RTS/View.elm](src/RTS/View.elm) | Rendering for all three screens: setup, the SVG battlefield + HUD, and the results screen. |
| `RTS.Main` | [src/RTS/Main.elm](src/RTS/Main.elm) | The `Browser.element` program — wires `init`/`update`/`view` and a 5×/second clock. |
| `RTS.Backend` | [backend/RTS/Backend.elm](backend/RTS/Backend.elm) | A server-side `handle` sharing `RTS.Model`: a landing page, `/ping`, `/api/map` (JSON world data) and in-memory `/api/save`+`/api/load`. |

## How to play

1. **Setup** — pick a map size and how many AI rivals (0–2), reroll the seed if you like, then
   **Start game**.
2. **Gather** — click one of your workers, then click a **gold mine** (gold) or **forest** (wood).
   Workers haul loads back to your base automatically.
3. **Grow** — **Train Worker** (50g) at the base; **Build Barracks** (120g), then **Train Soldier**
   (60g); a **Farm** (90g) adds passive gold income.
4. **Fight** — select a unit (or **Select whole army**), then click a tile to move or click an
   **enemy unit/building** to attack. Idle soldiers defend automatically.
5. **Win** — destroy every enemy base (or, solo, reach the power goal before time runs out). The
   results screen shows your grade, rating change and the power chart.

## Build & run

You need the [elm-lang](https://github.com/tunguski/elm-lang) CLI (`elm.sh`, `java -jar elm.jar`, or
the native binary).

### Frontend (the playable game)

```sh
elm make src/RTS/Main.elm --project=elm.json -o build/rts.html
# then open build/rts.html in a browser
```

### Tests

The whole engine is pure and deterministic, so it is tested without a browser. The suite covers the
RNG, the map generator, the game rules (train/build/gather/combat/win-lose), the AI making real
progress, and the rating:

```sh
elm test test/RtsTest.elm \
  src/RTS/Rng.elm src/RTS/Model.elm src/RTS/Map.elm src/RTS/Logic.elm \
  src/RTS/Ai.elm src/RTS/Rating.elm src/RTS/Game.elm
```

(elm-lang's own build additionally runs the game under its interpreter — see `RtsGameTest` — to guard
the example against language changes.)

### Backend (optional)

The backend is a `handle : Request -> Response` (plus a stateful `onRequest` for save/load) that
shares `RTS.Model`. It type-checks in isolation against the vendored [backend/Server.elm](backend/Server.elm):

```sh
elm check backend/Server.elm src/RTS/Model.elm backend/RTS/Backend.elm
```

Routes: `GET /` (landing page), `GET /ping` (`pong`), `GET /api/map` (JSON: map sizes, costs, terrain
palette), `POST /api/save` + `GET /api/load` (in-memory game state).

Because the browser frontend and the server backend each expose a `main`, they live in separate
source roots (`src/` vs `backend/`) and build separately.
