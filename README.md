# nhl-play-for-contract

A player-level study of whether NHL players perform differently around contract timing, segmented by player archetype.

## What this studies

The question is whether NHL players perform differently near the end of their contracts, and whether the pattern varies by player type. The framework places each contract on a spectrum from overpay (paid for a past or for a ceiling that did not arrive) through market rate to discount (team-first, below market).

This is a player-level study, one row per contract. It is distinct from the team-level NHL Free Agency Research project in the same portfolio.

## Headline finding

The data shows a loyalty discount, not a loyalty tax. Teams that re-sign their own players deliver more time on ice per cap share than teams that sign new players from elsewhere.

- Same-team mean overpay residual: +0.32 (n=1,115)
- New-team mean overpay residual: -0.45 (n=592)
- Tier-controlled difference: +0.70 minutes per game (p<0.0001)

The going-in expectation was that teams overpay to retain (the loyalty-tax hypothesis). The data shows the opposite.

The full findings, including the segmented walk-year effect by archetype and the honest null on the captain-as-discount question, are on the site.

## Live site

https://rmallorybpc.github.io/nhl-play-for-contract/

## Data sources

Contract data is a GitHub-hosted community dataset (Chief-Zach Sports-Data), accessed through a pluggable source adapter (the contract source seam). The current window spans 2012 through 2025.

Player performance and biographical data come from the NHL API via the nhlscraper R package, including time on ice, production, and birthdate. The earliest reliable performance season in this build is 2009-2010.

Team captaincy comes from Wikipedia's "List of [team] captains" pages, used under Creative Commons attribution (CC BY-SA). Captains only; alternates are not available from this source.

## Method in brief

The core metric is expected versus actual time on ice given AAV, normalized to cap share, evaluated at the contract level. A negative residual indicates an overpay (delivered less time on ice than the cost implied). A positive residual indicates a discount.

Segmentation runs across three dimensions:

- Tier: TOI percentiles within position and season (top, middle, fringe)
- Trajectory: TOI slope over up to three prior seasons (rising, stable, declining)
- Retention status: same-team, new-team, or entry

Full methodology and the honest-limitations section live on the site's Methods page.

## Repo structure

```
nhl-play-for-contract/
├── R/
│   ├── 01_data_extraction/        Contracts, performance, and captaincy extractors
│   ├── 02_data_cleaning/          Identity crosswalk and source cleaning
│   └── 03_feature_engineering/    Panel assembly, features, overpay metric
├── data/
│   ├── raw/                       Raw extracted CSVs
│   └── processed/                 Cleaned and panel CSVs
├── output/
│   └── tables/                    Analysis outputs and findings summary
├── dashboard/
│   └── src/                       Static site (TMG design system)
├── .github/
│   └── workflows/                 GitHub Actions Pages deploy
├── README.md
├── ROADMAP.md
├── TMG-BRAND-GUIDE.md             Reference (design source of truth)
└── tmg.css                        Reference (design system stylesheet)
```

## Pipeline

The pipeline runs in numbered stages:

1. Extraction (`R/01_data_extraction/`) pulls contracts, performance, and captaincy
2. Cleaning (`R/02_data_cleaning/`) standardizes formats, builds the name-to-player_id crosswalk, resolves name collisions, and applies manual overrides
3. Panel assembly (`R/03_feature_engineering/01_assemble_panel.R`) assembles the per-contract panel with extension flags, retention status, age, and captaincy
4. Feature engineering (`R/03_feature_engineering/02_*` through `04_*`) computes walk-year windows, trajectory, tier, and the overpay metric
5. Analysis produces the findings tables and the plain-language summary in `output/tables/`
6. Site (`dashboard/src/`) presents the findings

The contract data source sits behind a pluggable adapter (the source seam). The source can be swapped without changing any downstream code.

## Status

V1 is live. The pipeline runs end to end. The analysis has produced the findings tables and the plain-language summary. The static site presents them.

A future iteration may expand the contract source to capture more pre-2015 deals if data access permits, which would strengthen the discount and captaincy analyses.

## Scope and limitations

- Skaters only; goalies excluded for this version
- Entry contracts and extensions are flagged and excluded from the walk-year analysis
- Players with no NHL performance data are out of scope, because the analysis requires time on ice
- Captaincy is captains only (no alternates), a data-source limitation
- Some iconic historical contracts (notably Patrice Bergeron's 2013 extension) predate the contract source window and are narrative references, not data points
- Earlier years (2012-2014) and 2025 are more sparsely covered than the middle years
- Time on ice is the value proxy; it captures coaching usage, not everything a player contributes
- The captain-as-discount archetype was tested empirically and did not generalize from the available sample

## Portfolio and deployment

This is part of the TMG (The Mallory Group) research portfolio. The design system is referenced via the shared CDN: https://rmallorybpc.github.io/tmg-brand-guide/dist/tmg.css

GitHub Pages deploys via GitHub Actions. The Pages source must be set to "GitHub Actions" in repo settings for the workflow to publish.
