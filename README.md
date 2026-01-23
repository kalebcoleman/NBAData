# NBAData

Package structure initialized with modular endpoint and utility folders.

## Parsed table workflow

```r
tables <- espn_nba_parse_raw_dir(2023, raw_dir = "data/raw")
v <- validate_parsed_tables(tables, warn_only = TRUE)
write_parsed_tables(tables, out_dir = "data/parsed", format = "sqlite")
```

SQLite is the default source of truth for analytics; CSVs are an optional export.

## Database quickstart

```r
con <- nba_db_connect(db = "data/sql/nbadata.sqlite")
nba_db_init(con)

tables <- espn_nba_parse_raw_dir(2023, raw_dir = "data/raw")
write_parsed_tables(tables, format = "sqlite", mode = "upsert", con = con)
nba_db_disconnect(con)
```

Schema versioning is tracked in `nbadata_meta` via `NBA_SCHEMA_VERSION` and migrations in `R/migrations.R`.
TODO: add a `player_game_status` table for explicit DNP/absence rows.

## Raw collection

```r
collect_raw_season(2024, raw_dir = "data/raw")
collect_raw_season(2025, raw_dir = "data/raw")

# Raw files are stored in data/raw/<season>/summary_<season>_<YYYYMMDD>_<game_id>.json
tables <- espn_nba_parse_raw_dir(2023, raw_dir = "data/raw")
v <- validate_parsed_tables(tables, warn_only = TRUE)
write_parsed_tables(tables, out_dir = "data/parsed", format = "sqlite")
```

## Manifest + missing reports

```r
m_2024 <- espn_nba_manifest(2024, season_type = "regular", raw_dir = "data/raw")
m_2025 <- espn_nba_manifest(2025, season_type = "regular", raw_dir = "data/raw")
```

This writes:
- `manifest_<season>_<season_type>.csv`
- `missing_boxscore_<season>_<season_type>.csv` (postponed/canceled excluded)

## One-call pipeline

```r
out_2024 <- collect_parse_store(2024, raw_dir = "data/raw")
out_2025 <- collect_parse_store(2025, raw_dir = "data/raw")
```

## Incremental DB ingest

```r
nba_db_ingest_incremental(2024:2025, raw_dir = "data/raw", db_path = "data/sql/nbadata.sqlite")
```

## RDS DB ingest

```r
nba_db_ingest_rds(2026, parsed_dir = "data/parsed", db_path = "data/sql/nbadata.sqlite")
```

## Live integration test

Run with:

```sh
NBDATA_LIVE=true Rscript -e "testthat::test_file('tests/testthat/test-integration-live.R')"
```

```r
espn_nba_scrape_summaries_for_season(2023, raw_dir = "data/raw", progress = TRUE)
```
