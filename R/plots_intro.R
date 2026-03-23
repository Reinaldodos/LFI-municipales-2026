# R/plots_intro.R

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# =========================================================
# Table communale LFI municipales
# =========================================================

build_scores_lfi_commune <- function(muni_data) {
  scores_lfi_par_commune <-
    list(
      muni_data$input |>
        filter(type_vote == "liste") |>
        group_by(code_ville, liste) |>
        summarise(
          voix = sum(voix),
          .groups = "drop_last"
        ) |>
        mutate(
          score = voix / sum(voix)
        ) |>
        ungroup(),
      muni_data$candidats_nuances |>
        filter(nuance == "LFI") |>
        tidyr::unite(
          col = code_ville,
          code_departement, code_commune,
          remove = FALSE
        )
    ) |>
    reduce(inner_join, by = join_by(code_ville, liste)) |>
    distinct(code_ville, liste, voix, score)

  commune_labels <- muni_data$bureaux |>
    distinct(code_ville, libelle_commune)

  out <- commune_labels |>
    inner_join(
      scores_lfi_par_commune,
      by = join_by(code_ville)
    )

  out
}

# =========================================================
# Graphique 1 : top 30 communes LFI aux municipales
# =========================================================

plot_intro_top_communes_lfi <- function(scores_lfi_commune, n = 30) {
  scores_lfi_commune |>
    slice_max(n = n, order_by = score) |>
    ggplot(
      mapping = aes(
        y = reorder(libelle_commune, score),
        x = score
      )
    ) +
    geom_point(mapping = aes(size = voix)) +
    geom_segment(mapping = aes(
      x = 0, xend = score
    )) +
    labs(
      title = "Communes où LFI réalise ses meilleurs scores municipaux",
      subtitle = "Part de la liste LFI dans les suffrages exprimés communaux",
      x = "Score LFI communal",
      y = NULL,
      size = "Voix"
    ) +
    theme_publication()
}

# =========================================================
# Graphique 2 : comparaison européennes/municipales
# sur les communes LFI les plus massives en voix
# =========================================================

plot_intro_compare_euro_muni <- function(scores_lfi_commune, base_cf, n = 12) {
  top_communes <- scores_lfi_commune |>
    slice_max(order_by = voix, n = n) |>
    distinct(code_ville, libelle_commune)

  base_cf |>
    semi_join(
      top_communes,
      by = join_by(code_ville, libelle_commune)
    ) |>
    ggplot(
      mapping = aes(
        x = europeennes_2024,
        y = municipales_2026
      )
    ) +
    geom_abline() +
    geom_point() +
    labs(
      title = "Européennes 2024 vs municipales 2026 dans les principaux bastions LFI",
      subtitle = "Scores par bureau de vote, en parts des inscrits",
      x = "Européennes 2024",
      y = "Municipales 2026",
      caption = "Comparaison des niveaux LFI par bureau de vote"
    ) +
    facet_wrap(~libelle_commune) +
    theme(legend.position = "none") +
    theme_publication()
}
