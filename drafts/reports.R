library(targets)
tar_load_globals()
tar_load(muni_data)
tar_load(muni_t2_data)

villes <-
  muni_t2_data$bureaux |>
  select(code_ville, contains("departement"), contains("commune")) |>
  distinct()

resultats <-
  list(
    "T1" = muni_data,
    "T2" = muni_t2_data
  ) |>
  map(.f = ~ .$input) |>
  reduce(
    .f = full_join,
    by = join_by(
      geo_id, code_ville, code_departement, libelle_departement, code_commune, libelle_commune, code_bv,
      index, liste, nuance, liste_upper, nuance_upper, type_vote
    )
  ) |>
  pivot_longer(
    cols = c(voix.x, voix.y), names_to = "tour", values_to = "voix",
    values_drop_na = TRUE
  ) |>
  mutate(tour = case_match(
    .x = tour,
    "voix.x" ~ "t1",
    "voix.y" ~ "t2"
  ))

resultats_LFI <-
  resultats |>
  filter(type_vote == "liste") |>
  summarise(
    voix = sum(voix),
    .by = c(tour, code_ville, liste, nuance)
  ) |>
  group_by(tour, code_ville) |>
  mutate(part = voix / sum(voix)) |>
  ungroup() |>
  filter(nuance == "LFI")

qualifiables <-
  resultats_LFI |>
  filter(
    tour == "t1",
    part >= .1
  )

qualifies <-
  resultats_LFI |>
  filter(tour == "t2")

anti_join(
  x = qualifiables,
  y = qualifies,
  by = join_by(code_ville)
) |>
  inner_join(x = resultats |> distinct(code_ville, libelle_commune)) |>
  arrange(-voix)

nb_bdv_par_ville <-
  resultats |>
  filter(tour == "t2") |>
  summarise(
    nb_bdv = n_distinct(geo_id),
    .by = c(code_ville, libelle_commune)
  )

selection <-
  nb_bdv_par_ville |>
  slice_max(order_by = nb_bdv, n = 30)

source("drafts/functions_report.R")
future::plan(
  strategy = "multisession",
  workers = future::availableCores() - 1
)

output <-
  resultats |>
  semi_join(y = nb_bdv_par_ville, by = join_by(code_ville)) |>
  # semi_join(y = selection, by = join_by(code_ville)) |>
  group_nest(code_ville, keep = TRUE) |>
  mutate(mod_flux = furrr::future_map(
    .x = data,
    .f = estimate_flows_regularized,
    bureau_col = "geo_id", tour1_value = "t1", tour2_value = "t2",
    weight_mode = "sqrt",
    .progress = TRUE
  ))

future::plan(strategy = "sequential")

output |>
  semi_join(y = nb_bdv_par_ville) |>
  mutate(
    # reports = map(.x = mod_flux, .f = get_reports),
    flux_voix = pmap(
      .f = get_flux_voix,
      .l = list(
        resultats = data,
        mod_flux = mod_flux
      ),
      .progress = TRUE
    )
  ) |>
  inner_join(x = villes, by = join_by(code_ville)) |>
  saveRDS(file = "sankey_app/data/reports.rds")
