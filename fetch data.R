library(tidyverse)
resultats_municipales_2026 <-
  "https://object.files.data.gouv.fr/hydra-parquet/hydra-parquet/1428132c-ad5e-437e-a928-7c2a254e40eb.parquet" |>
  arrow::read_parquet() |>
  janitor::clean_names() |>
  mutate(code_commune = str_sub(code_commune, start = -3)) |>
  tidyr::unite(col = geo_id, code_departement, code_commune, code_bv, remove = FALSE) |>
  filter(inscrits > 0)

bureaux <-
  resultats_municipales_2026 |>
  select(
    geo_id,
    ends_with("departement"),
    ends_with("commune"),
    code_bv
  )

inscrits <-
  resultats_municipales_2026 |>
  select(geo_id, inscrits)

non_exprimes <-
  resultats_municipales_2026 |>
  select(geo_id, abstentions, blancs, nuls) |>
  pivot_longer(cols = -geo_id, names_to = "liste", values_to = "voix")

candidats <-
  resultats_municipales_2026 |>
  select(geo_id, starts_with("libelle_de_liste")) |>
  pivot_longer(
    cols = -geo_id,
    names_to = "toto",
    values_to = "liste", values_drop_na = TRUE
  ) |>
  tidyr::extract(
    col = toto,
    into = c("index"), regex = ".*_([0-9]*)$"
  )

voix <-
  resultats_municipales_2026 |>
  select(geo_id, starts_with("voix")) |>
  pivot_longer(
    cols = -geo_id,
    names_to = "toto",
    values_to = "voix",
    values_drop_na = TRUE
  ) |>
  tidyr::extract(
    col = toto,
    into = c("index"), regex = ".*_([0-9]*)$"
  )

nuances <-
  resultats_municipales_2026 |>
  select(geo_id, starts_with("nuance")) |>
  pivot_longer(
    cols = -geo_id,
    names_to = "toto",
    values_to = "nuance",
    values_drop_na = TRUE
  ) |>
  tidyr::extract(
    col = toto,
    into = c("index"), regex = ".*_([0-9]*)$"
  )

exprimes <-
  list(
    candidats, nuances, voix
  ) |>
  reduce(
    .f = full_join,
    by = join_by(geo_id, index)
  )

input <- bind_rows(exprimes, non_exprimes)

candidats_nuances <-
  exprimes |>
  inner_join(
    x = bureaux,
    by = join_by(geo_id)
  ) |>
  distinct(code_departement, code_commune, liste, nuance)
