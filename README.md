# NFL DFS projections, injury checks and lineup optimizer (R) — DraftKings & FanDuel

Three layers, one workflow:

1. **Projections** you build yourself from free nflverse data (`project_slate()`), with a
   walk-forward backtest so you can see how good they really are.
2. **Injuries**: the official NFL report + FanDuel's flag + your own late-news file, turned into a
   *chance of playing* that scales each projection (`injury_report()` lists who to double-check).
3. **Cross-check** against FanDuel/DraftKings/any other projections (`compare_projections()`), and
   **score everyone after the games** (`save_snapshot()` / `evaluate_snapshots()`).

Then the optimizer builds *n* different high-projection lineups using integer linear programming:
one engine, two rule sets (exact single-lineup optimization, then a loop that produces a diversified set).

```r
source("run_projections_example.R")       # whole flow on a demo slate (real players, made-up salaries)
# or:  Rscript run_projections_example.R fanduel FanDuel-NFL-....csv
```

## Quick start

```r
setwd("dfs-optimizer")                      # project root
source("run_projections_example.R")         # REAL projections -> injury check -> optimizer -> lineups CSV
```

or from a shell: `Rscript run_projections_example.R fanduel` (or `draftkings`). With no salary file it uses
a demo slate (real players for the upcoming NFL week, made-up salaries), builds projections from nflverse
data, applies the injury adjustment, and feeds those projections straight into the optimizer. With your
own file: `Rscript run_projections_example.R fanduel FanDuel-NFL-....csv`. The first run downloads about
50 MB of data into `cache/`.

`run_example.R` is a separate, optimizer-only demo on fake players (no projections are built); use it to see
the optimizer's options in isolation. `Rscript tests/run_tests.R` runs all tests, `Rscript run_backtest.R`
reproduces the accuracy numbers.

**Packages:** base R plus one solver. `install.packages("Rglpk")` (preferred, faster) and/or
`install.packages("lpSolve")`. The code picks whichever is installed (`solver = "auto"`).

## Using your own data

```r
slate <- load_dk_salaries("DKSalaries.csv")         # or: load_fd_players("FanDuel-NFL-....csv", drop_out = FALSE)
proj  <- project_slate(slate, "draftkings")         # builds the projections (see next section)
res   <- optimize_lineups(playable_slate(proj), "draftkings", n = 20, min_unique = 3,
                          randomness = 0.12, max_exposure = 0.5, bringback_min = 1)
res                                                 # preview + most-used players
write_lineups_csv(res, "output/dk_upload.csv")      # player ids, one lineup per row
```

The projections come from `project_slate()`: it builds them itself, so no projections file is needed.
If you would rather use someone else's numbers (or a blend) instead of, or on top of, the model's,
`read_projections("their_projections.csv")` + `merge_projections(slate, proj)` still work: the file needs
`name, position, proj`, plus `team` for defenses (they match by team). Optional columns: `stdev` (scales
the noise per player), `max_exp` (per-player exposure cap, 0–1).

If you'd rather skip the loaders, any data frame with
`id, name, position, team, opponent, salary, proj` works. See `data/sample_slate_*.csv`.

## Options for `optimize_lineups()`

| Option | Default | What it does |
|---|---|---|
| `n` | 20 | Number of lineups |
| `min_unique` | 2 | Every lineup differs from every other by at least this many players |
| `randomness` | 0.10 | Noise on projections before each solve, as a fraction of the projection (0 = deterministic). Higher → more varied, lower-projection lineups |
| `first_lineup_optimal` | TRUE | Lineup 1 uses the unperturbed projections |
| `max_exposure` | 1 | Max share of lineups any one player can be in (override per player with a `max_exp` column) |
| `stack_min`, `stack_positions` | 1, WR/TE | QB must be paired with this many of those positions from his team |
| `bringback_min`, `bringback_positions` | 0, WR/TE | ...and this many from the opposing team |
| `no_dst_vs_offense` | TRUE | No defense alongside offensive players it is facing |
| `lock`, `ban` | none | Player ids or names forced into / kept out of every lineup |
| `min_salary` | none | Spend at least this much |
| `max_per_team` | site rule | Override the per-team limit (e.g. cap DraftKings stacks at 5) |
| `seed` | none | Reproducible noise |

The result has `$lineups` (names + salary/projection), `$ids` (upload ids), `$exposure`
(usage per player), plus the config used. `validate_result(res)` re-checks every lineup
against the site rules with code that is separate from the optimizer.

## Projections, injuries and checking them

### The workflow

```r
for (f in list.files("R", full.names = TRUE)) source(f)

slate <- load_fd_players("FanDuel-NFL-....csv", drop_out = FALSE)   # or load_dk_salaries("DKSalaries.csv")
                                                       # (drop_out = FALSE keeps FanDuel's "O" players: their absence lifts teammates)
proj  <- project_slate(slate, "fanduel",               # first run downloads nflverse data into ./cache
                       overrides = "data/overrides.csv")   # your late news (optional)
proj                                                   # top players by position + warnings
injury_report(proj, min_proj = 6)                      # who is not clearly healthy, most important first

cmp <- compare_projections(proj, list(fanduel_research = "fd_research.csv"))   # optional cross-check
cmp                                                    # agreement stats + the biggest disagreements
save_snapshot(cmp)                                     # keep this week's numbers for scoring later

res <- optimize_lineups(playable_slate(proj), "fanduel", n = 20, min_unique = 3, bringback_min = 1)
# ... after the games:  evaluate_snapshots()           # who was actually closer to reality?
```

`project_slate()` returns your slate plus: `proj` (expected points **including** the chance he doesn't
play, the number to optimize with), `proj_active` (points *if he plays*, comparable with outside
projections), `p_play`, `status`, `practice`, `injury`, `flags` (things a human should look at).
Defenses are projected too. Players with no NFL history are flagged; a player who can't be matched to nflverse gets a salary-based estimate and a flag.

### How the projections are made

One linear regression per position, refit every week on the previous ~3 seasons of games, using
only information available before kickoff:

- recency-weighted averages of the player's fantasy points and of his usage (targets, carries,
  pass attempts), shrunk toward the position average when he has little history;
- his team's implied points and the point spread (from the Vegas lines in the nflverse schedule), home/away;
- how many fantasy points the opponent has allowed to that position;
- usage that is *missing* because teammates are Out/Doubtful (a WR1 out lifts the others);
- interactions (a good player's output moves more with the game environment).

Defenses use their own recent form plus the opponent's sack rate, scoring and the spread.
Everything is computed by the same code for past weeks (backtest) and the upcoming week, and the
tests prove that changing a week's results does not change that week's projections.

### How accurate is it? (honest numbers)

Walk-forward backtest, 2025 season through week 8 of 2026 (27 weeks, never touched while choosing
settings; those were chosen on 2023-24). Players averaging 5+ points:

| | Model | Plain rolling average | |
|---|---|---|---|
| FanDuel scoring, RMSE | 6.83 | 6.91 | 1.2% better |
| DraftKings scoring, RMSE | 7.61 | 7.69 | 1.1% better |
| Correlation with results | 0.53 (FD), 0.52 (DK) | 0.51, 0.50 | |
| Defenses, correlation / mean absolute error | 0.30 / 4.03 | 0.10 / 4.23 | clearly better |

What this means, plainly:

- Weekly fantasy output is mostly noise. A typical projection misses by ~5 points; the *best*
  public models are not far ahead of a good rolling average. This model is modestly better than
  that baseline, **not** dramatically better, and I would not expect it to beat paid projections.
- It is **well calibrated** (projections around 14.6 averaged 14.3 actual points; see
  `calibration_table()`), so the numbers can be fed to the optimizer as expectations.
- What you get that a rolling average does not: game environment, injury-aware usage, a chance-of-playing
  adjustment, defenses, and transparency (you can see why).
- A first attempt looked 7% better than baseline; that turned out to be an artefact of how I filtered
  the evaluation players, found and removed before this table. The real gain is the small number above.
- Reproduce: `Rscript run_backtest.R fanduel 2025 2026` (or `draftkings`).

### Comparing with FanDuel / DraftKings (and what it can and cannot tell you)

DraftKings does not offer a projections download that I could find, and FanDuel Research shows
projections as a web table. So: copy the table into a CSV with columns **name, position, team, proj**
(common variants such as Player/Pos/FPTS are recognized; defenses can be keyed by team or team name),
or export from any other provider. Pass several to get a consensus:
`compare_projections(proj, list(fanduel = "fd.csv", other = "other.csv"))`.

You get: per-source agreement (bias, typical gap, correlation, rank correlation), and a list of
players where you differ by `max(3 points, 25%)`. Special flag: **a source has a player at ~0 while
you have him at 5+** (they probably know he's inactive: check news). It compares `proj_active`
(points *if he plays*), which is what outside projections mean.

**Agreement is not accuracy.** If you and FanDuel agree, you may both be wrong. Disagreement is a
prompt to look, not proof you're wrong. The only way to learn who is *accurate* is to score the
numbers against results, week after week:

1. Before kickoff: `save_snapshot(cmp)` (writes `snapshots/`).
2. After the games, once nflverse has the week: `evaluate_snapshots()` gives RMSE/MAE/bias per source,
   weekly and overall. It shows both "mine (if plays)" (comparable with outside numbers) and "mine
   (injury-weighted)", so the gap between them is what the injury layer is worth.
3. After a few weeks you know whether to trust yours, theirs or a blend (`blend_projections()`),
   based on evidence. Snapshots must be saved *before* the games: re-running `project_slate()`
   afterwards shows actual results for finished games, and those rows are skipped as circular.

### Injuries

Every player gets a **status** and a **chance of playing**:

- Sources: the official NFL injury report (game status and final practice status), FanDuel's injury
  indicator if you loaded a FanDuel file, and **your overrides** (`data/overrides.csv`:
  `name, status, p_play, note`; see `data/overrides_example.csv`). Overrides always win and also
  update teammates' projections (WR1 ruled out → the other receivers go up).
- Otherwise the **worst** status across the NFL report and FanDuel wins (wrongly keeping an Out
  player costs a whole roster spot; wrongly dropping a healthy one costs little).
- **Out / IR = 0.** Questionable and Doubtful players are scaled by how often players with that exact
  status *and* that final practice level actually played (the last five seasons): about 61% of Questionable, under
  1% of Doubtful. A player with a practice-report entry but no designation plays about 85%. These
  rates are estimated from the data when you run it, not hard-coded.
- "Played" means the player has a stat line, so a player who dressed but recorded nothing counts as
  not playing; this slightly lowers the rates, and is the same thing a roster spot would earn (zero).
- `injury_report()` lists everyone Out/Doubtful/Questionable **and** anyone with under 95% chance of
  playing (e.g. didn't practise) - sorted by status and by points at stake.

**Limits you should know about:**

- Official designations are published Friday (earlier before Thursday games). If you run this earlier,
  the free feed has only practice participation and `project_slate()` prints a **warning** when the
  week's report looks incomplete (it compares the share of players with a designation against a normal
  week). "Active" then means "no news yet".
- **Final inactives come about 90 minutes before kickoff**, and the free feed will not have them in
  time for you. For anything that matters, check a news source right before lock and put what you find
  in the overrides file (re-run takes seconds). Late scratches are the biggest injury risk in DFS and
  no data feed makes them go away.
- For games already played, `proj` is the actual result (marked `actual (game played)`); use
  `playable_slate()` to drop those, Out players and teams on bye before optimizing.
- A possible improvement I did not build: snap-count data as an additional usage signal.

## Site rules built in (confirmed Nov 2026; re-check each season)

| | DraftKings | FanDuel |
|---|---|---|
| Salary cap | $50,000 | $60,000 |
| Roster | QB, 2 RB, 3 WR, TE, FLEX (RB/WR/TE), DST | QB, 2 RB, 3 WR, TE, FLEX (RB/WR/TE), DEF |
| Team rules | ≥ 2 teams and ≥ 2 games | ≤ 4 players per team, ≥ 3 teams |

Rules live in `R/sites.R` if the sites change anything.

## How it works

One 0/1 variable per player; maximize total (noisy) projection subject to the cap, position
ranges (FLEX is expressed as "one of RB/WR/TE may exceed its base count"), site team/game
rules, and your stacking options. For lineup *k*, extra constraints say "share at most
`9 − min_unique` players with each earlier lineup" and "skip anyone already at their exposure cap".
Each solve takes milliseconds to a fraction of a second.

## What was tested

`Rscript tests/run_tests.R` (142 checks, about 20 seconds; the data-dependent ones are skipped if nflverse can't be reached):

- Both sites × both solvers: 40-lineup runs satisfy every rule, uniqueness, exposure, stack and bring-back.
- **Brute force:** on a tiny slate, every legal lineup is enumerated and the solver's 8 sequential
  lineups (with uniqueness cuts) match the true optimum at every step, for both sites, with
  strategy rules on and off. I also deliberately broke the optimizer three ways (looser cap,
  missing DST rule, off-by-one uniqueness) to confirm the suite fails when it should.
- Locks, bans, per-player caps, seed reproducibility, graceful stop when constraints are infeasible.
- File loaders and projection merge on synthetic files shaped like the site downloads.
- **Scoring** against hand-computed stat lines for both sites (bonuses, fumbles, defense tiers).
- **No look-ahead:** inflating a week's player stats, team stats and game scores leaves that week's
  features and predictions *exactly* unchanged (and does change the following week, so the test is
  meaningful); a panel built from data that stops before the week reproduces the backtest features.
- Decayed averages, teammates-out usage, injury status reconciliation, play-rate lookup, overrides,
  report-coverage warning, benchmark parsing and comparison, and source scoring: all with hand-computed answers.
- End to end: with a week's results hidden, `project_slate()` forecasts it (correlation ~0.5 with what
  happened, mean error ~0), and an Out override zeroes a player and raises his teammates.

Speed: 150 lineups on a 384-player, 16-game slate in about 4 s (Rglpk) / 6 s (lpSolve).
First `project_slate()` run downloads ~80 MB of nflverse files (then cached; the injury file
refreshes hourly); a projection run takes a few seconds.

## Known limits — please read

- **Loaders and upload files are untested against real site files.** They follow the published
  layout of each site's salary download, but I had no real files to test with; the tests use
  look-alikes. If a column isn't found you'll get a clear error and the fix is a few lines in `R/data.R`.
  `write_lineups_csv()` writes 9 columns of player ids under each site's roster header
  (DraftKings ends in `DST`, FanDuel in `DEF`); paste them into the entry template the site gives
  you and try **one entry first** before uploading a batch.
- Only full-slate **Classic** contests. No Showdown/captain mode, no kickers.
- The HiGHS solver (`highs` package) isn't supported — it could not be installed in the build
  environment, so it couldn't be tested. Rglpk and lpSolve are.
- Noise is independent per player. It does not model correlation (shootouts, game script) beyond the
  stack/bring-back rules. That is the natural next step — Stage 3: simulate correlated slates, use
  this engine to generate a large candidate pool, then pick the *n* lineups that do best across simulations.
- The sample/demo slates are fake (the demo uses real players with made-up salaries); lineups from them are meaningless.
- Projections come from free data only: no depth charts, weather, snap counts, or live news. Late
  role changes, coaching decisions and surprise inactives are not in the numbers; that is what the overrides file is for.
- The nflverse injury and stats files are community-maintained and occasionally lag by hours; check
  the "data is N min old" line on the injury report.
- Loaders for the FanDuel/DraftKings files (including the FanDuel injury indicator) have not been run
  against real downloads.
- This is not financial advice; DFS is negative-expectation for most players. Stake accordingly.
