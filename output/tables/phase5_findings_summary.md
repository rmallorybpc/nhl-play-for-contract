# Phase 5 Findings: Play-for-Contract Analysis

## Part 0 - Coverage and sample transparency
- Contracts in panel: 3175.
- Contract count by signing year: 2012=72, 2013=89, 2014=106, 2015=136, 2016=177, 2017=188, 2018=232, 2019=267, 2020=268, 2021=395, 2022=377, 2023=385, 2024=443, 2025=40.
- Eligible walk-year sample: 1624.
- Eligible overpay sample: 1760.
- Captain-contract count (captains only): 29; eligible overpay captains: 22.
- Retention distribution: same_team=1507, new_team=857, entry=728, unknown=83.
- Thin tier x trajectory buckets (n < 20): none.

## Part 1 - Walk-year effect by archetype
- Aggregate eligible walk-year effect is muted: mean post-signing TOI change = 0.36, median = 0.22; mean points change = 0.01.
- Aggregate walk-year trend delta (walk year minus expected from prior trend) = -0.15; spike-above-trend share = 31.1%.
- Buckets with the strongest post-signing drop (n >= 20):
  - top x stable (n=87): post-signing TOI change -0.58, walk-year trend delta 0.62
  - middle x declining (n=142): post-signing TOI change -0.26, walk-year trend delta 3.41
  - top x declining (n=74): post-signing TOI change -0.24, walk-year trend delta 3.45
- Buckets with flat-to-positive post-signing outcomes (n >= 20):
  - fringe x insufficient_history (n=281): post-signing TOI change 1.86, walk-year trend delta NA
  - fringe x stable (n=43): post-signing TOI change 1.11, walk-year trend delta -1.33
  - fringe x declining (n=128): post-signing TOI change 0.87, walk-year trend delta 2.57
- Interpretable TOI-change model (post_signing_toi_change ~ tier + trajectory + age): age coefficient = -0.17 (p=0.0000). Full coefficients: output/tables/walk_year_toi_change_model.csv.

## Part 2 - Retention-overpay loyalty-tax test
- same_team mean overpay_residual = 0.32 (n=1115), new_team mean = -0.45 (n=592).
- Mean difference (same_team - new_team) = 0.78. Negative supports loyalty-tax; positive does not.
- Tier-controlled model coefficient for same_team (vs new_team) = 0.70 (p=0.0000).

## Part 3 - Discount profile (descriptive)
- Positive-residual contracts in eligible set: 905.
- Most common discount tier: middle (323, 35.7%).
- Most common discount trajectory: insufficient_history (319, 35.2%).
- Most common discount retention status: same_team (648, 71.6%).
- Discount age profile: mean 25.01, median 24.00, IQR [23.00, 27.00].
- Material discount examples (cap-share >= 0.03; games >= 82 or TOI >= 1000): TJ Brodie (2013: 6.63), Valeri Nichushkin (2020: 5.11), Neal Pionk (2019: 4.97), Darnell Nurse (2018: 4.87), Robert Thomas (2021: 4.82).
- Recognizable discount captain check: Bergeron is not present in the current contract window/source pull.

## Part 4 - Captaincy lens (descriptive, thin sample)
- Captain contracts mean overpay_residual = -0.35 (n=22), non-captain mean = 0.00 (n=1738).
- Captain contracts listed with residuals: Mark Stone (2015: 4.28), Ryan O'Reilly (2023: 3.45), Gabriel Landeskog (2021: 1.63), Jamie Benn (2013: 1.34), Jordan Staal (2023: 1.16), Claude Giroux (2022: 1.09), Ryan McDonagh (2013: 1.09), Gabriel Landeskog (2013: 0.70), Mark Stone (2018: 0.63), Sidney Crosby (2024: 0.54), Bo Horvat (2023: -0.08), Dylan Larkin (2023: -0.20), Steven Stamkos (2024: -1.11), Max Pacioretty (2018: -1.45), Anders Lee (2019: -1.73), Alex Ovechkin (2021: -1.91), Mark Stone (2019: -2.14), Claude Giroux (2013: -2.18), Alex Pietrangelo (2020: -2.30), Nick Foligno (2021: -2.88), Steven Stamkos (2016: -2.89), John Tavares (2018: -4.80).
- Caveat: captain sample is small; captaincy data is captains-only (no alternates); captaincy is association, not causation.

## Part 5 - Overpay/discount extremes (raw vs material)
- Raw extremes include low-usage edge cases by design. Rows: 50.
- Material lens floors used for story-ready ranking: cap-share >= 0.03, and usage >= 82 games or >= 1000 TOI minutes. Rows: 50.
- Material overpay examples: Brendan Smith (2017: -6.13), Marc-Edouard Vlasic (2017: -6.08), Dmitry Orlov (2023: -5.84), Corey Perry (2013: -5.82), Dougie Hamilton (2021: -5.28).
- Material discount examples: TJ Brodie (2013: 6.63), Valeri Nichushkin (2020: 5.11), Neal Pionk (2019: 4.97), Darnell Nurse (2018: 4.87), Robert Thomas (2021: 4.82).
- Spot-check traces for hockey-sense sanity:
  - Overpay trace: Brendan Smith (2017: -6.13).
  - Discount trace: TJ Brodie (2013: 6.63).

## Honest limitations
- Coverage is limited to contracts in the current data window. Some iconic historical deals predate the extracted contract source window and are narrative anchors, not panel rows.
- Players without NHL performance records are out of scope because TOI outcomes cannot be computed.
- Captaincy source is captains-only from Wikipedia captaincy histories (no alternates).
- Intent is unmeasurable; this captures revealed outcomes (TOI delivered versus cap-share cost), not motivation.
- Contract data: a GitHub-hosted community dataset snapshot from Chief-Zach Sports-Data, accessed through the contract source seam adapter in extraction. Current signing-year window is 2012 to 2025.
- TOI is the value proxy and reflects coaching usage, not every dimension of player value.

## Output files
- output/tables/walk_year_effect_by_bucket.csv
- output/tables/retention_overpay_comparison.csv
- output/tables/discount_profile.csv
- output/tables/captaincy_lens.csv
- output/tables/overpay_extremes_raw.csv
- output/tables/overpay_extremes_material.csv
- output/tables/phase5_findings_summary.md
