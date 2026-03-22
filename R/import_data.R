# R/import_municipales.R

# =========================================================
# Lecture et normalisation de la source brute
# =========================================================

read_resultats_municipales_2026 <- function(source) {
  source |>
    arrow::read_parquet()
}

read_resultats_europeennes_2024 <- function(source) {
  source |>
    readr::read_delim(
      delim = ";",
      col_types = readr::cols(.default = "c"),
      locale = readr::locale(encoding = "UTF-8"),
      progress = TRUE,
      show_col_types = FALSE
    )
}

raw_clean <- function(raw_data) {
  raw_data |>
    janitor::clean_names() |>
    mutate(
      code_departement = as.character(code_departement),
      code_commune = str_sub(as.character(code_commune), start = -3),
      code_bv = as.character(code_bv)
    ) |>
    tidyr::unite(
      col = geo_id,
      code_departement, code_commune, code_bv,
      remove = FALSE
    ) |>
    mutate(
      code_ville = paste(code_departement, code_commune, sep = "_")
    ) |>
    mutate(inscrits = as.integer(inscrits)) |>
    filter(inscrits > 0)
}

# =========================================================
# Tables de base utiles en aval
# =========================================================

build_bureaux <- function(resultats_municipales_2026) {
  resultats_municipales_2026 |>
    distinct(
      geo_id,
      code_ville,
      code_departement,
      libelle_departement,
      code_commune,
      libelle_commune,
      code_bv
    )
}

build_inscrits <- function(resultats_municipales_2026) {
  resultats_municipales_2026 |>
    select(geo_id, inscrits)
}

# =========================================================
# Dépliage générique des colonnes indexées
# =========================================================

build_indexed_long <- function(data, prefix, value_name) {
  data |>
    select(geo_id, starts_with(prefix)) |>
    pivot_longer(
      cols = -geo_id,
      names_to = "tmp",
      values_to = value_name,
      values_drop_na = TRUE
    ) |>
    tidyr::extract(
      col = tmp,
      into = c("index"),
      regex = ".*_([0-9]+)$"
    )
}

build_exprimes <- function(resultats_municipales_2026) {
  specs <- list(
    list(prefix = "libelle_de_liste", value_name = "liste"),
    list(prefix = "voix_", value_name = "voix"),
    list(prefix = "nuance_liste", value_name = "nuance")
  )

  specs |>
    purrr::map(
      ~ build_indexed_long(
        data = resultats_municipales_2026,
        prefix = .x$prefix,
        value_name = .x$value_name
      )
    ) |>
    reduce(
      .f = full_join,
      by = join_by(geo_id, index)
    )
}

# =========================================================
# Non exprimés
# =========================================================

build_non_exprimes <- function(resultats_municipales_2026) {
  resultats_municipales_2026 |>
    select(geo_id, abstentions, blancs, nuls) |>
    pivot_longer(
      cols = -geo_id,
      names_to = "liste",
      values_to = "voix"
    ) |>
    mutate(
      index = case_when(
        liste == "abstentions" ~ "A",
        liste == "blancs" ~ "B",
        liste == "nuls" ~ "N",
        TRUE ~ NA_character_
      ),
      nuance = NA_character_
    )
}

# =========================================================
# Table longue finale
# =========================================================

.classify_vote_type <- function(liste) {
  case_when(
    liste == "abstentions" ~ "abstention",
    liste == "blancs" ~ "blanc",
    liste == "nuls" ~ "nul",
    TRUE ~ "liste"
  )
}

build_input <- function(resultats_municipales_2026, bureaux) {
  exprimes <- build_exprimes(resultats_municipales_2026)
  non_exprimes <- build_non_exprimes(resultats_municipales_2026)

  bind_rows(exprimes, non_exprimes) |>
    left_join(
      bureaux |>
        select(
          geo_id,
          code_ville,
          code_departement,
          libelle_departement,
          code_commune,
          libelle_commune,
          code_bv
        ),
      by = "geo_id"
    ) |>
    mutate(
      liste = as.character(liste),
      nuance = as.character(nuance),
      voix = as.integer(voix),
      index = as.character(index),
      liste_upper = str_to_upper(liste),
      nuance_upper = str_to_upper(nuance),
      type_vote = .classify_vote_type(liste)
    ) |>
    select(
      geo_id,
      code_ville,
      code_departement,
      libelle_departement,
      code_commune,
      libelle_commune,
      code_bv,
      index,
      liste,
      nuance,
      liste_upper,
      nuance_upper,
      type_vote,
      voix
    )
}

# =========================================================
# Couples liste/nuance utiles en aval
# =========================================================

build_candidats_nuances <- function(resultats_municipales_2026, bureaux) {
  build_exprimes(resultats_municipales_2026) |>
    inner_join(
      bureaux,
      by = join_by(geo_id)
    ) |>
    distinct(code_departement, code_commune, liste, nuance)
}

# =========================================================
# Orchestrateur principal
# =========================================================

build_objects <- function(raw_data) {
  resultats <-
    raw_data |>
    raw_clean()

  bureaux <- build_bureaux(resultats)

  list(
    bureaux = bureaux,
    inscrits = build_inscrits(resultats),
    input = build_input(resultats, bureaux),
    candidats_nuances = build_candidats_nuances(resultats, bureaux)
  )
}

fusion_SD <- function(raw_data) {
  pierrefitte_raw <-
    raw_data |>
    filter(
      `Code département` == "93",
      str_detect(string = `Libellé commune`, pattern = "^Pierrefitte")
    )

  reste_raw <-
    anti_join(
      x = raw_data,
      y = pierrefitte_raw,
      by = join_by(`Code département`, `Code commune`, `Code BV`)
    )

  pierrefitte_raw |>
    mutate(
      `Code commune` =
        case_match(
          .x = `Code commune`,
          .default = `Code commune`,
          "93059" ~ "93066"
        ),
      `Libellé commune` =
        case_match(
          .x = `Libellé commune`,
          .default = `Libellé commune`,
          "Pierrefitte-sur-Seine" ~ "Saint-Denis"
        ),
      `Code BV` =
        case_match(
          .x = `Code BV`,
          .default = `Code BV`,
          "0001" ~ "0051",
          "0002" ~ "0052",
          "0003" ~ "0053",
          "0004" ~ "0054",
          "0005" ~ "0055",
          "0006" ~ "0056",
          "0007" ~ "0057",
          "0008" ~ "0058",
          "0009" ~ "0059",
          "0010" ~ "0060",
          "0011" ~ "0061",
          "0012" ~ "0062"
        )
    ) |>
    bind_rows(reste_raw)
}

build_objects_and_fusion_SD <- function(raw_data){
  raw_data |>
    fusion_SD() |>
    build_objects()
}
