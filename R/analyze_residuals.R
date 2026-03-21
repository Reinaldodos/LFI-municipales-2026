# R/analyze_residuals.R

# =========================================================
# Helpers
# =========================================================

add_predictions <- function(base_cf, model, pred_col = "pred") {
  base_cf |>
    mutate(
      !!pred_col := predict(model)
    )
}

add_residuals <- function(base_cf, model,
                          pred_col = "pred",
                          resid_col = "resid") {
  base_cf |>
    add_predictions(model = model, pred_col = pred_col) |>
    mutate(
      !!resid_col := municipales_s - .data[[pred_col]]
    )
}

# =========================================================
# Résumés des résidus
# =========================================================

summarise_residuals <- function(data, resid_col = "resid") {
  resid_vec <- data[[resid_col]]

  tibble::tibble(
    mean = mean(resid_vec, na.rm = TRUE),
    sd = sd(resid_vec, na.rm = TRUE),
    min = min(resid_vec, na.rm = TRUE),
    q01 = as.numeric(quantile(resid_vec, 0.01, na.rm = TRUE)),
    q05 = as.numeric(quantile(resid_vec, 0.05, na.rm = TRUE)),
    median = median(resid_vec, na.rm = TRUE),
    q95 = as.numeric(quantile(resid_vec, 0.95, na.rm = TRUE)),
    q99 = as.numeric(quantile(resid_vec, 0.99, na.rm = TRUE)),
    max = max(resid_vec, na.rm = TRUE)
  )
}

summarise_residuals_weighted <- function(data,
                                         resid_col = "resid",
                                         weight_col = "inscrits") {
  tibble::tibble(
    weighted_mean = weighted.mean(
      x = data[[resid_col]],
      w = data[[weight_col]],
      na.rm = TRUE
    )
  )
}

# =========================================================
# Bureaux en sur- / sous-performance
# =========================================================

top_residual_bureaux <- function(data,
                                 n = 20,
                                 resid_col = "resid") {
  data |>
    slice_max(
      order_by = .data[[resid_col]],
      n = n
    )
}

bottom_residual_bureaux <- function(data,
                                    n = 20,
                                    resid_col = "resid") {
  data |>
    slice_min(
      order_by = .data[[resid_col]],
      n = n
    )
}

# =========================================================
# Agrégation communale des résidus
# =========================================================

build_residuals_commune <- function(data,
                                    resid_col = "resid",
                                    weight_col = "inscrits") {
  data |>
    group_by(code_ville, libelle_commune) |>
    summarise(
      resid = weighted.mean(.data[[resid_col]], w = .data[[weight_col]], na.rm = TRUE),
      euro = weighted.mean(europeennes_2024, w = .data[[weight_col]], na.rm = TRUE),
      muni = weighted.mean(municipales_2026, w = .data[[weight_col]], na.rm = TRUE),
      inscrits_commune = sum(.data[[weight_col]], na.rm = TRUE),
      .groups = "drop"
    )
}

# =========================================================
# Concentration des surperformances
# =========================================================

summarise_positive_tail <- function(re_all,
                                    intercept_col = "intercept",
                                    weight_col = "inscrits_commune") {
  intercept <- re_all[[intercept_col]]
  weights <- re_all[[weight_col]]

  tibble::tibble(
    part_95 = sum(weights[intercept > quantile(intercept, 0.95, na.rm = TRUE)], na.rm = TRUE) / sum(weights, na.rm = TRUE),
    part_99 = sum(weights[intercept > quantile(intercept, 0.99, na.rm = TRUE)], na.rm = TRUE) / sum(weights, na.rm = TRUE),
    part_gt1 = sum(weights[intercept > 1], na.rm = TRUE) / sum(weights, na.rm = TRUE),
    part_lt_minus1 = sum(weights[intercept < -1], na.rm = TRUE) / sum(weights, na.rm = TRUE)
  )
}

# =========================================================
# Graphiques exploratoires des résidus
# =========================================================

plot_residual_hist <- function(data,
                               resid_col = "resid",
                               weight_col = "inscrits",
                               bins = 50) {
  ggplot(data, aes(x = .data[[resid_col]], weight = .data[[weight_col]])) +
    geom_histogram(bins = bins) +
    labs(
      x = "Résidu",
      y = "Effectif pondéré",
      title = "Distribution pondérée des résidus"
    )
}

plot_residuals_vs_euro <- function(residus_commune) {
  ggplot(
    residus_commune,
    aes(x = euro, y = resid, size = inscrits_commune)
  ) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Score LFI aux européennes 2024",
      y = "Résidu communal pondéré",
      size = "Inscrits",
      title = "Sur- / sous-performance communale relative"
    )
}
