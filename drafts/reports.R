tar_load(muni_data)
tar_load(muni_t2_data)

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
  arrange(-voix) |>
  view()

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

future::plan(strategy = "multisession", workers = future::availableCores())

output <-
  resultats |>
  semi_join(y = selection, by = join_by(code_ville)) |>
  group_nest(code_ville, keep = TRUE) |>
  mutate(mod_flux = furrr::future_map(
    .x = data,
    .f = estimate_flows_regularized,
    bureau_col = "geo_id", tour1_value = "t1", tour2_value = "t2",
    weight_mode = "sqrt",
    .progress = TRUE
  )) |>
  mutate(
    reports = map(.x = mod_flux, .f = get_reports),
    flux_voix = pmap(.f = get_flux_voix, .l = list(
      resultats = data,
      mod_flux = mod_flux
    ))
  )

future::plan(strategy = "sequential")

muni_t2_data$bureaux |>
  select(code_ville, contains("departement"), contains("commune")) |>
  distinct() |>
  inner_join(y = output, by = join_by(code_ville)) |>
  saveRDS(file = "output/reports.rds")

selection |>
  filter(libelle_commune == "Grenoble") |>
  semi_join(
    x = output,
    by = join_by(code_ville)
  ) |>
  pluck("flux_voix", 1) |>
  plot_sankey_networkD3(
    palette_nuances = palette_bloc,
    seuil_flux = 300,
    regrouper_non_exprimes = TRUE
  )

get_reports <- function(mod_flux) {
  tidy_transition_matrix(mod_flux) |>
    mutate(part_report = round(part_report, digits = 2)) |>
    filter(part_report > 0) |>
    pivot_wider(names_from = "liste_t2", values_from = "part_report") %>%
    bind_cols(code_ville = ville, .)
}

get_flux_voix <- function(resultats, mod_flux) {
  liste_nuancees <-
    resultats |>
    distinct(tour, liste, nuance) |>
    mutate(
      liste_nuance =
        case_when(
          is.na(nuance) ~ glue::glue("{tour} : {liste}"),
          .default = glue::glue("{tour} : {liste} ({nuance})")
        )
    ) |>
    mutate(liste_nuance) %>%
    split(f = .$tour)

  flux_voix <- tidy_aggregate_flows(mod_flux)

  flux_voix |>
    right_join(
      x = liste_nuancees$t1,
      by = join_by(liste == liste_t1)
    ) |>
    rename(
      liste_t1 = liste_nuance,
      nuance_t1 = nuance
    ) |>
    select(-tour, -liste) |>
    right_join(
      x = liste_nuancees$t2,
      by = join_by(liste == liste_t2)
    ) |>
    rename(liste_t2 = liste_nuance) |>
    select(-tour, -liste, -nuance) |>
    mutate(
      nuance_t1 = recode_bloc_nuance(nuance_t1)
    )
}



palette_nuances <- c(
  "NA"   = "#BDBDBD",

  # Gauche / extrême gauche
  "LEXG" = "#8B0000",
  "LFI"  = "red",
  "LCOM" = "red",
  "LUG"  = "#E57373",
  "LSOC" = "pink",
  "LDVG" = "pink",

  # Écologistes / régionalistes / divers sensibilité verte
  "LECO" = "#43A047",
  "LVEC" = "#66BB6A",
  "LREG" = "#7CB342",

  # Centre / macronie / divers centre
  "LUC"  = "#FBC02D",
  "LDVC" = "#FFD54F",
  "LREN" = "#FFCA28",
  "LMDM" = "#FFB300",

  # Droite modérée / centre droit / divers droite
  "LLR"  = "#1E88E5",
  "LUDI" = "#42A5F5",
  "LUDI" = "#42A5F5", # volontaire si vous harmonisez mal en amont
  "LUD"  = "#64B5F6",
  "LUDR" = "#5C6BC0",
  "LDVD" = "#90CAF9",
  "LREC" = "#1565C0",
  "LHOR" = "#3949AB",
  "LDSV" = "#7986CB",

  # Extrême droite / droite radicale
  "LRN"  = "black",
  "LEXD" = "black",
  "LUXD" = "black",

  # Divers / inclassables
  "LDIV" = "#9E9E9E"
)

palette_bloc <- c(
  "gauche_radicale" = "#B71C1C",
  "gauche"          = "#E57373",
  "ecologistes"     = "#66BB6A",
  "centre"          = "#FBC02D",
  "droite"          = "#42A5F5",
  "extreme_droite"  = "#283593",
  "divers"          = "#9E9E9E"
)

recode_bloc_nuance <- function(x) {
  dplyr::case_when(
    # Gauche radicale
    x %in% c("LEXG", "LFI", "LCOM") ~ "gauche_radicale",

    # Gauche
    x %in% c("LUG", "LSOC", "LDVG") ~ "gauche",

    # Écologistes / régionalistes
    x %in% c("LECO", "LVEC", "LREG") ~ "ecologistes",

    # Centre (inclut LUDI comme vous l’indiquez)
    x %in% c("LUC", "LDVC", "LREN", "LMDM", "LUDI") ~ "centre",

    # Droite classique
    x %in% c("LLR", "LUD", "LDVD", "LREC", "LHOR", "LDSV") ~ "droite",

    # Extrême droite (inclut LUDR)
    x %in% c("LRN", "LEXD", "LUXD", "LUDR") ~ "extreme_droite",

    # Divers / NA
    x %in% c("LDIV", "NA") ~ "divers",
    TRUE ~ "divers"
  )
}
