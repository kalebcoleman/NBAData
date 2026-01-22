# NBAData

Package structure initialized with modular endpoint and utility folders.

## Parsed table workflow

```r
tables <- espn_nba_parse_raw_dir(2023, raw_dir = "data/raw")
v <- validate_parsed_tables(tables, warn_only = TRUE)
write_parsed_tables(tables, out_dir = "data/parsed", format = "sqlite")
```

SQLite is the default source of truth for analytics; CSVs are an optional export.
