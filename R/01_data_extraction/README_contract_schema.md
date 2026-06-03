# Contract Source Adapter Schema

The extraction step must produce a canonical contracts file consumed by downstream cleaning, panel assembly, feature engineering, and analysis scripts.

Any contract source adapter must emit exactly these columns:

- player_name
- position
- signing_team
- previous_team (may be NA)
- contract_value
- aav
- contract_years
- signing_year
- signing_date
- contract_type

Current canonical output path:

- data/raw/spotrac_contracts_raw.csv

Adapters are source-specific, but downstream scripts are source-agnostic and should only rely on this schema.
