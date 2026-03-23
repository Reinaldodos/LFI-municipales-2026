library(dplyr)
library(ggplot2)
library(tibble)
library(ggrepel)
library(rlang)

# ---------------------------------------------------------
# 1. Extraction propre des effets communaux
# ---------------------------------------------------------
extract_commune_random_effects <- function(model, bureaux) {
  re_name <- names(lme4::ranef(model))[1]

  re_df <- lme4::ranef(model)[[re_name]] |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "code_ville") |>
    dplyr::rename(
      intercept_re = `(Intercept)`,
      slope_re = europeennes_s
    ) |>
    dplyr::mutate(
      intercept_total = lme4::fixef(model)[["(Intercept)"]] + intercept_re,
      slope_total = lme4::fixef(model)[["europeennes_s"]] + slope_re
    )

  communes <- bureaux |>
    dplyr::distinct(code_ville, libelle_commune)

  re_df |>
    dplyr::left_join(communes, by = "code_ville")
}

# ---------------------------------------------------------
# 2. Sélection des communes à labelliser
# ---------------------------------------------------------
select_labels_re <- function(df, n_labels = 25) {
  # score d'extrémalité simple et robuste
  df |>
    dplyr::mutate(
      z_intercept = as.numeric(scale(intercept_re)),
      z_slope = as.numeric(scale(slope_total)),
      extremeness = sqrt(z_intercept^2 + z_slope^2)
    ) |>
    dplyr::slice_max(order_by = extremeness, n = n_labels, with_ties = FALSE)
}

# ---------------------------------------------------------
# 3. Graphique slope vs intercept
# ---------------------------------------------------------
plot_commune_intercept_slope <- function(
  df_re,
  n_labels = 25,
  highlight_communes = NULL,
  use_total_intercept = FALSE,
  point_alpha = 0.85,
  point_size = 2.1,
  caption_width = 80
) {
  x_var <- if (use_total_intercept) "intercept_total" else "intercept_re"

  label_df <- select_labels_re(df_re, n_labels = n_labels)

  if (!is.null(highlight_communes)) {
    label_df <- dplyr::bind_rows(
      label_df,
      df_re |>
        dplyr::filter(libelle_commune %in% highlight_communes)
    ) |>
      dplyr::distinct(code_ville, .keep_all = TRUE)
  }

  ggplot(
    data = df_re,
    mapping = aes(
      x = .data[[x_var]],
      y = slope_total
    )
  ) +
    geom_hline(
      yintercept = unique(df_re$slope_total |> median(na.rm = TRUE)),
      linetype = "dashed",
      linewidth = 0.4,
      alpha = 0.7
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.4,
      alpha = 0.7
    ) +
    geom_point(
      alpha = point_alpha,
      size = point_size
    ) +
    ggrepel::geom_text_repel(
      data = label_df,
      mapping = aes(label = libelle_commune),
      size = 3.6,
      min.segment.length = 0,
      box.padding = 0.35,
      point.padding = 0.25,
      force = 1.2,
      max.overlaps = Inf,
      seed = 123
    ) +
    labs(
      title = "Intercepts et pentes communaux",
      subtitle = "Hétérogénéité du niveau structurel et de l’intensité de conversion",
      x = if (use_total_intercept) {
        "Intercept total communal"
      } else {
        "Intercept aléatoire communal"
      },
      y = "Pente totale européennes → municipales",
      caption = stringr::str_wrap(
        paste0(
          "Chaque point représente une commune. ",
          "Les libellés correspondent aux communes les plus extrêmes ",
          "dans l’espace intercept/pente."
        ),
        width = caption_width
      )
    ) +
    theme_publication()
}
