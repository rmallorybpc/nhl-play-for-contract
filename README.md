# NHL Play-for-Contract

NHL behavioral economics research project studying whether player performance changes around contract timing.

This project is part of the **TMG (The Mallory Group)** portfolio.

## Data sources

- **Spotrac** for player contract history and contract timing
- **NHL / ESPN APIs via nhlscraper** for player performance and player bio data
- **Wikipedia** for team captaincy history

> Wikipedia captaincy data is licensed under Creative Commons and requires attribution. This repository credits Wikipedia as the captaincy source for the initial raw extraction layer.

## Project status

This project is in early development. The current build focuses on repository scaffolding and raw data extraction only.

## Planned workflow

1. Extract raw source data
2. Clean and standardize source records
3. Engineer features, including the overpay metric
4. Run analysis
5. Publish the site/dashboard

## Live site

https://rmallorybpc.github.io/nhl-play-for-contract/

## Deployment

This repository deploys GitHub Pages via GitHub Actions using `.github/workflows/deploy-pages.yml`.

Manual one-time repository setting:
- In GitHub repository settings, go to Pages.
- Set Source to `GitHub Actions`.
