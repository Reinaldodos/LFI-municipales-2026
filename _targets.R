library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "dplyr",
    "tidyr",
    "stringr",
    "purrr",
    "janitor",
    "arrow",
    "ggplot2",
    "lme4",
    "tibble"
  ),
  format = "rds"
)

tar_source("R", change_directory = FALSE)

# =========================================================
# Config
# =========================================================

muni_url <- "https://object.files.data.gouv.fr/hydra-parquet/hydra-parquet/1428132c-ad5e-437e-a928-7c2a254e40eb.parquet"
muni_t2_url <- "https://object.files.data.gouv.fr/hydra-parquet/hydra-parquet/ac97f1d2-64b3-453e-90e6-485913aabd04.parquet"
euro_url <- "https://www.data.gouv.fr/api/1/datasets/r/cc1883d9-1265-4365-b754-fb6aef22d82e"

top_n <- 20

liste_models <- c("A")

model_grid <- tibble::tibble(
  model_name = liste_models,
  formula_name = liste_models
)

# =========================================================
# Pipeline
# =========================================================

list(
  # -----------------------------
  # Paramètres
  # -----------------------------
  tar_target(muni_url_target, muni_url),
  tar_target(muni_t2_url_target, muni_t2_url),
  tar_target(euro_url_target, euro_url),
  tar_target(top_n_target, top_n),

  # -----------------------------
  # Imports
  # -----------------------------
  tar_target(
    muni_raw,
    read_resultats_municipales_2026(muni_url_target)
  ),
  tar_target(
    muni_t2_raw,
    read_resultats_municipales_2026(muni_t2_url_target)
  ),
  tar_target(
    euro_raw,
    read_resultats_europeennes_2024(euro_url_target)
  ),
  tar_target(
    muni_data,
    build_objects(muni_raw)
  ),
  tar_target(
    muni_t2_data,
    build_objects(muni_t2_raw)
  ),
  tar_target(
    euro_data,
    build_objects_and_fusion_SD(euro_raw)
  ),
  tar_target(
    scores_lfi_commune,
    build_scores_lfi_commune(
      muni_data = muni_data
    )
  ),
  tar_target(
    p_intro_top_communes_lfi,
    plot_intro_top_communes_lfi(scores_lfi_commune, n = 30)
  ),

  # -----------------------------
  # Base contrefactuelle
  # -----------------------------
  tar_target(
    base_cf,
    build_base_cf(
      muni_data = muni_data,
      euro_data = euro_data
    )
  ),
  tar_target(
    sample_info,
    base_cf |>
      summarise(
        n_obs = n(),
        n_communes = n_distinct(code_ville)
      )
  ),
  tar_target(
    p_intro_compare_euro_muni,
    plot_intro_compare_euro_muni(
      scores_lfi_commune = scores_lfi_commune,
      base_cf = base_cf,
      n = 12
    )
  ),

  # -----------------------------
  # Branching statique sur les spécifications
  # -----------------------------
  tar_map(
    values = model_grid,
    names = model_name,
    unlist = TRUE,
    tar_target(
      model_fit,
      fit_model(base_cf, cf_formulas[[formula_name]])
    ),
    tar_target(
      base_cf_resid,
      add_residuals(
        base_cf = base_cf,
        model = model_fit
      )
    ),
    tar_target(
      residual_summary,
      summarise_residuals(base_cf_resid)
    ),
    tar_target(
      residual_summary_pretty,
      format_residual_summary(residual_summary)
    ),
    tar_target(
      residual_summary_weighted,
      summarise_residuals_weighted(base_cf_resid)
    ),
    tar_target(
      top_bureaux,
      top_residual_bureaux(base_cf_resid, n = top_n_target)
    ),
    tar_target(
      bottom_bureaux,
      bottom_residual_bureaux(base_cf_resid, n = top_n_target)
    ),
    tar_target(
      residus_commune,
      build_residuals_commune(base_cf_resid)
    ),
    tar_target(
      re_all,
      extract_ranef_commune(
        model = model_fit,
        base_cf = base_cf
      )
    ),
    tar_target(
      ranef_summary,
      summarise_ranef_distribution(re_all)
    ),
    tar_target(
      ranef_tail_weights,
      summarise_ranef_tail_weights(re_all)
    ),
    tar_target(
      ranef_tail_pretty,
      format_ranef_tail(ranef_tail_weights)
    ),
    tar_target(
      top_communes,
      top_ranef_communes(re_all, n = top_n_target)
    ),
    tar_target(
      bottom_communes,
      bottom_ranef_communes(re_all, n = top_n_target)
    ),
    tar_target(
      top_communes_gt1,
      dplyr::filter(top_communes, intercept > 1)
    ),
    tar_target(
      p_residual_hist,
      plot_residual_hist(base_cf_resid)
    ),
    tar_target(
      p_residuals_vs_euro,
      plot_residuals_vs_euro(residus_commune)
    ),
    tar_target(
      p_ranef_distribution,
      plot_ranef_distribution(re_all)
    ),
    tar_target(
      p_top_ranef,
      plot_top_ranef_communes(re_all, n = top_n_target)
    ),
    tar_target(
      p_bottom_ranef,
      plot_bottom_ranef_communes(re_all, n = top_n_target)
    ),
    tar_target(
      commune_random_effects,
      extract_commune_random_effects(
        model = model_fit,
        bureaux = muni_data$bureaux
      )
    ),
    tar_target(
      plot_intercept_slope,
      plot_commune_intercept_slope(
        df_re = commune_random_effects,
        n_labels = 10,
        caption_width = 65,
        highlight_communes = c("Brest", "Saint-Denis", "Toulouse", "Roubaix")
      )
    ),
    tar_target(
      publication_bundle,
      list(
        residuals = format_residual_summary(residual_summary),
        ranef = format_ranef_tail(ranef_tail_weights),
        plot_main = p_ranef_distribution
      )
    )
  )
)
