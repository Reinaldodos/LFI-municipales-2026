# R/build_base_cf.R

# =========================================================
# Helpers
# =========================================================

standardize_numeric <- function(x) {
  as.numeric(scale(x))
}

# =========================================================
# Recodage des blocs
# =========================================================

recode_bloc_euro <- function(nuance_upper) {
  case_when(
    nuance_upper %in% c("LFI", "LEXG", "LUG", "LSOC", "LDVG") ~ "gauche",
    nuance_upper %in% c("LDVC", "LUDI", "LMDM", "LUC", "LHOR") ~ "centre",
    nuance_upper %in% c("LLR", "LUD", "LDVD") ~ "droite",
    nuance_upper %in% c("LRN", "LREC", "LUXD", "LEXD", "LUDR") ~ "ext_droite",
    TRUE ~ "autres"
  )
}

# =========================================================
# Générique : score en part des inscrits
# =========================================================

build_vote_share <- function(data, filter_expr) {
  data$input |>
    filter(type_vote == "liste") |>
    filter({{ filter_expr }}) |>
    left_join(
      data$inscrits,
      by = join_by(geo_id)
    ) |>
    transmute(
      geo_id,
      share = voix / inscrits
    ) |>
    group_by(geo_id) |>
    summarise(
      share = sum(share, na.rm = TRUE),
      .groups = "drop"
    )
}

# =========================================================
# Spécialisations
# =========================================================

build_lfi_score <- function(data, output_name) {
  build_vote_share(
    data = data,
    filter_expr = nuance_upper == "LFI"
  ) |>
    rename(!!output_name := share)
}

build_left_score <- function(data, output_name) {
  data$input |>
    filter(type_vote == "liste") |>
    mutate(bloc = recode_bloc_euro(nuance_upper)) |>
    filter(bloc == "gauche") |>
    left_join(
      data$inscrits,
      by = join_by(geo_id)
    ) |>
    transmute(
      geo_id,
      share = voix / inscrits
    ) |>
    group_by(geo_id) |>
    summarise(
      share = sum(share, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(!!output_name := share)
}

# =========================================================
# Dominance LFI dans la gauche
# =========================================================

build_dominance_lfi <- function(lfi_scores, left_scores, lfi_col, left_col) {
  lfi_sym <- rlang::ensym(lfi_col)
  left_sym <- rlang::ensym(left_col)

  lfi_scores |>
    inner_join(
      left_scores,
      by = join_by(geo_id)
    ) |>
    mutate(
      dominance_LFI_euro = if_else(
        !!left_sym > 0,
        !!lfi_sym / !!left_sym,
        NA_real_
      )
    ) |>
    select(geo_id, dominance_LFI_euro)
}

# =========================================================
# Taille des communes
# =========================================================

build_taille_commune <- function(muni_data) {
  muni_data$bureaux |>
    select(geo_id, code_ville) |>
    inner_join(
      muni_data$inscrits,
      by = join_by(geo_id)
    ) |>
    group_by(code_ville) |>
    summarise(
      inscrits_commune = sum(inscrits, na.rm = TRUE),
      .groups = "drop"
    )
}

# =========================================================
# Base contrefactuelle
# =========================================================

build_base_cf <- function(muni_data, euro_data) {
  lfi_muni <- build_lfi_score(
    data = muni_data,
    output_name = "municipales_2026"
  )

  lfi_euro <- build_lfi_score(
    data = euro_data,
    output_name = "europeennes_2024"
  )

  gauche_euro <- build_left_score(
    data = euro_data,
    output_name = "gauche_euro"
  )

  dominance_lfi_euro <- build_dominance_lfi(
    lfi_scores = lfi_euro,
    left_scores = gauche_euro,
    lfi_col = europeennes_2024,
    left_col = gauche_euro
  )

  taille_commune <- build_taille_commune(muni_data)

  muni_data$bureaux |>
    select(geo_id, code_ville, libelle_commune) |>
    inner_join(
      muni_data$inscrits,
      by = join_by(geo_id)
    ) |>
    inner_join(
      lfi_muni,
      by = join_by(geo_id)
    ) |>
    inner_join(
      lfi_euro,
      by = join_by(geo_id)
    ) |>
    left_join(
      gauche_euro,
      by = join_by(geo_id)
    ) |>
    left_join(
      dominance_lfi_euro,
      by = join_by(geo_id)
    ) |>
    left_join(
      taille_commune,
      by = join_by(code_ville)
    ) |>
    mutate(
      gauche_euro = coalesce(gauche_euro, 0),
      dominance_LFI_euro = coalesce(dominance_LFI_euro, 0),
      log_bureau = log(inscrits),
      log_commune = log(inscrits_commune),
      municipales_s = standardize_numeric(municipales_2026),
      europeennes_s = standardize_numeric(europeennes_2024),
      gauche_euro_s = standardize_numeric(gauche_euro),
      dominance_LFI_euro_s = standardize_numeric(dominance_LFI_euro),
      log_bureau_s = standardize_numeric(log_bureau),
      log_commune_s = standardize_numeric(log_commune)
    ) |>
    mutate(
      gauche_orth = resid(lm(gauche_euro_s ~ europeennes_s)),
      LFI_orth    = resid(lm(dominance_LFI_euro_s ~ europeennes_s))
    ) |>
    create_no_gauche(muni_data = muni_data)
}
