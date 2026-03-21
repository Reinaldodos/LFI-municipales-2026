# R/analyze_ranef.R

# =========================================================
# Extraction des effets aléatoires communaux
# =========================================================

extract_ranef_commune <- function(model, base_cf) {
  re <- ranef(model)$code_ville |>
    tibble::rownames_to_column("code_ville") |>
    rename(
      intercept = `(Intercept)`,
      slope = europeennes_s
    )

  meta_commune <- base_cf |>
    group_by(code_ville, libelle_commune) |>
    summarise(
      inscrits_commune = sum(inscrits, na.rm = TRUE),
      .groups = "drop"
    )

  re |>
    left_join(
      meta_commune,
      by = join_by(code_ville)
    )
}

# =========================================================
# Tables top / bottom
# =========================================================

top_ranef_communes <- function(re_all, n = 20, effect_col = "intercept") {
  re_all |>
    slice_max(
      order_by = .data[[effect_col]],
      n = n
    )
}

bottom_ranef_communes <- function(re_all, n = 20, effect_col = "intercept") {
  re_all |>
    slice_min(
      order_by = .data[[effect_col]],
      n = n
    )
}

# =========================================================
# Quantiles et concentration
# =========================================================

summarise_ranef_distribution <- function(re_all, effect_col = "intercept") {
  x <- re_all[[effect_col]]

  tibble::tibble(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    q01 = as.numeric(quantile(x, 0.01, na.rm = TRUE)),
    q05 = as.numeric(quantile(x, 0.05, na.rm = TRUE)),
    median = median(x, na.rm = TRUE),
    q95 = as.numeric(quantile(x, 0.95, na.rm = TRUE)),
    q99 = as.numeric(quantile(x, 0.99, na.rm = TRUE)),
    max = max(x, na.rm = TRUE)
  )
}

summarise_ranef_tail_weights <- function(re_all,
                                         effect_col = "intercept",
                                         weight_col = "inscrits_commune") {
  x <- re_all[[effect_col]]
  w <- re_all[[weight_col]]

  tibble::tibble(
    part_95 = sum(w[x > quantile(x, 0.95, na.rm = TRUE)], na.rm = TRUE) / sum(w, na.rm = TRUE),
    part_99 = sum(w[x > quantile(x, 0.99, na.rm = TRUE)], na.rm = TRUE) / sum(w, na.rm = TRUE),
    part_gt1 = sum(w[x > 1], na.rm = TRUE) / sum(w, na.rm = TRUE),
    part_lt_minus1 = sum(w[x < -1], na.rm = TRUE) / sum(w, na.rm = TRUE)
  )
}

# =========================================================
# Préparation pour graphiques
# =========================================================

prepare_ranef_rank_plot <- function(re_all, effect_col = "intercept") {
  re_all |>
    arrange(.data[[effect_col]]) |>
    mutate(rang = row_number())
}

prepare_top_ranef_plot <- function(re_all, n = 20, effect_col = "intercept") {
  re_all |>
    slice_max(order_by = .data[[effect_col]], n = n) |>
    mutate(
      libelle_commune = reorder(libelle_commune, .data[[effect_col]])
    )
}

prepare_bottom_ranef_plot <- function(re_all, n = 20, effect_col = "intercept") {
  re_all |>
    slice_min(order_by = .data[[effect_col]], n = n) |>
    mutate(
      libelle_commune = reorder(libelle_commune, .data[[effect_col]])
    )
}

# =========================================================
# Graphiques
# =========================================================

plot_ranef_distribution <- function(re_all, effect_col = "intercept") {
  re_ranked <- prepare_ranef_rank_plot(re_all, effect_col = effect_col)

  ggplot(
    re_ranked,
    aes(
      x = rang,
      y = .data[[effect_col]],
      size = inscrits_commune
    )
  ) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Rang des communes",
      y = "Effet aléatoire communal",
      size = "Inscrits",
      title = "Distribution des effets aléatoires communaux"
    ) +
    theme_publication()
}

plot_top_ranef_communes <- function(re_all, n = 20, effect_col = "intercept") {
  re_top <- prepare_top_ranef_plot(re_all, n = n, effect_col = effect_col)

  ggplot(
    re_top,
    aes(
      x = libelle_commune,
      y = .data[[effect_col]],
      size = inscrits_commune
    )
  ) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    labs(
      x = NULL,
      y = "Effet aléatoire communal",
      size = "Inscrits",
      title = "Top communes en surperformance relative"
    ) +
    theme_publication()
}

plot_bottom_ranef_communes <- function(re_all, n = 20, effect_col = "intercept") {
  re_bottom <- prepare_bottom_ranef_plot(re_all, n = n, effect_col = effect_col)

  ggplot(
    re_bottom,
    aes(
      x = libelle_commune,
      y = .data[[effect_col]],
      size = inscrits_commune
    )
  ) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    labs(
      x = NULL,
      y = "Effet aléatoire communal",
      size = "Inscrits",
      title = "Top communes en sous-performance relative"
    ) +
    theme_publication()
}
