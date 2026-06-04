# NHL play-for-contract

A player-level study of whether NHL skaters perform differently around contract timing, segmented by player archetype.

## What this studies

This project asks one question: do skaters perform differently near contract end, and does that vary by player type.

The archetype spectrum runs from overpay to market rate to discount.
Overpay means pay landed above observed delivery.
Discount means observed delivery landed above expected value.
Each contract is placed on that spectrum.

This is a player-level contract study, with one row per contract.
It is distinct from the team-level NHL Free Agency Research project.

## Data sources

- Contract data: a GitHub-hosted community dataset snapshot from Chief-Zach Sports-Data, accessed through the contract source seam adapter in extraction. Current signing-year window is 2012 to 2025.
- Performance and player bios: NHL API data via the nhlscraper R package, including time on ice, production, and birth date fields.
- Captaincy: Wikipedia captaincy lists, captains only.

Wikipedia captaincy data is Creative Commons licensed and requires attribution.
This repository attributes Wikipedia as the captaincy source in the raw extraction layer.

## Method in brief

The core metric is expected versus actual time on ice at contract level.
Expected time on ice is modeled from AAV normalized to cap share.
Actual time on ice is post-signing observed usage.
Negative residual means overpay.
Positive residual means discount.

Segmentation runs across three axes.
Tier is TOI-based: top, middle, fringe.
Trajectory is prior-season trend: rising, stable, declining.
Retention status is same-team, new-team, or entry.

Full method details and limitations will live on the site Methods page after site launch.

## Repo structure

```text
R/
	01_data_extraction/      # raw extraction scripts and contract schema notes
	02_data_cleaning/        # team crosswalk, identity reconciliation, source cleaning
	03_feature_engineering/  # panel assembly, windows, overpay metric, analysis panel build
	04_analysis/             # phase analysis script writing output tables and findings summary draft
dashboard/
	src/                     # static site pages, shared styles, client JS, placeholder explorer data
data/
	raw/                     # extracted source files
	processed/               # cleaned tables and engineered features
output/
	tables/                  # analysis output tables and findings summary markdown
```

## How the pipeline works

1. Extraction: pull contract, performance, and captaincy source files.
2. Cleaning and identity reconciliation: standardize teams, match names to player_id, and produce cleaned source tables.
3. Panel assembly: create one-row-per-contract panel with contract timing fields and eligibility flags.
4. Feature engineering: build walk-year and post-signing windows, trajectory and tier buckets, and cap-share overpay residuals.
5. Analysis: produce segmented output tables from the analysis panel.
6. Site: publish static overview, methods, findings, explorer, and audit pages.

The contract source runs behind a pluggable seam adapter.
Source can be swapped without downstream pipeline changes if the schema contract is preserved.

## Status

Built now:

- Extraction scripts for contracts, performance, and captaincy.
- Cleaning and identity reconciliation pipeline.
- Panel assembly and feature engineering pipeline.
- Contract source seam with source dispatch and schema validation.

In progress or pending:

- Analysis interpretation and final narrative conclusions.
- Static site content population for findings and explorer outputs.
- Public launch packaging.

This repository does not claim final findings yet.

## Scope and limitations

- Scope is skaters only. Goalies are excluded in this version.
- Entry contracts and extensions are flagged and excluded from walk-year effect analysis.
- Players without NHL performance rows are out of scope because the analysis requires time on ice.
- Captaincy is captains only. Alternate captains are not covered by the current source.
- Some iconic historical contracts predate the data window and are narrative references, not model rows.
- Time on ice is a value proxy. It captures coaching usage, not every contribution type.

## Portfolio and deployment notes

This project is part of the TMG (The Mallory Group) research portfolio.
The TMG design system is shared through the portfolio CDN, and this repo currently includes a local copy of that stylesheet for site scaffolding.

GitHub Pages deploys through GitHub Actions via .github/workflows/deploy-pages.yml.
In repository settings, Pages source must be set to GitHub Actions.

Live site link: [https://rmallorybpc.github.io/nhl-play-for-contract/index.html]
