# Audit Summary

## Falsification test
- Real same-team coefficient: 0.703.
- Null mean from shuffled treatment labels: 0.001.
- Null standard deviation: 0.116.
- Empirical p-value: <0.0001 (0 of 2000 permutations as or more extreme).

## Subsample robustness
- Position, trajectory, tier, and era subgroup models were run on the eligible same-team/new-team sample (n=1707).
- Era split used contract signing years early_2012_2021 and late_2022_2024.
- Tier groups with holds=TRUE: 3 of 4.
- Trajectory groups with holds=TRUE: 2 of 4.
- Non-holding subgroups: trajectory=declining; trajectory=stable; tier=unknown.
- The within-tier and within-trajectory cuts are the selection-relevant checks.

## Outlier sensitivity
- No-trim coefficient: 0.703.
- 1 percent trim coefficient: 0.593.
- 5 percent trim coefficient: 0.503.
- The same-team coefficient stays positive and significant after 1 percent and 5 percent tail trimming.
