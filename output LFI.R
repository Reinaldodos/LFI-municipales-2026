scores_LFI <-
  candidats_nuances |>
  filter(nuance == "LFI") |>
  semi_join(x = input) |>
  left_join(y = inscrits) |>
  mutate(municipales_2026 = voix / inscrits)

targets::tar_load_globals()
con <- DBI::dbConnect(duckdb::duckdb(),
  dbdir = "~/Documents/These elections/medallion/gold_data/elections_nationales_gold.duckdb",
  read_only = TRUE
)

precedents_LFI <-
  dplyr::tbl(con, "gold_fact_votes") |>
  collect() |>
  tidyr::unite(col = "liste", scrutin, bulletin_id) |>
  fetch_nom_listes(
    extracted_col = "liste",
    db_path = targets::tar_read("duckdb_path")
  ) |>
  filter(
    str_detect(string = nom, pattern = "INSOUMIS") |
      str_detect(string = nom, pattern = "ENCHON") |
      str_detect(string = nom, pattern = "LFI")
  )

precedents_LFI <-
  dplyr::tbl(con, "gold_fact_votes") |>
  inner_join(x = dplyr::tbl(con, "gold_dim_bureau")) |>
  group_by(scrutin, geo_id) |>
  summarise(
    inscrits = sum(voix),
    .groups = "drop"
  ) |>
  left_join(
    x = precedents_LFI,
    copy = TRUE
  ) |>
  mutate(part_inscrits = voix / inscrits)

output_LFI <-
  precedents_LFI |>
  select(geo_id, scrutin, part_inscrits) |>
  spread(scrutin, part_inscrits) |>
  inner_join(x = scores_LFI)
