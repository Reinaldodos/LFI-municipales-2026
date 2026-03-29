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

create_no_gauche <- function(muni_data, base_cf) {
  blocs <-
    muni_data$candidats_nuances |>
    mutate(code_bloc = recode_bloc_nuance(nuance)) |>
    tidyr::unite(col = code_ville, code_departement, code_commune) |>
    semi_join(y = base_cf)

  base_cf_plus <-
    blocs |>
    summarise(
      nb_liste = n_distinct(liste),
      .by = c(code_ville, code_bloc)
    ) |>
    pivot_wider(
      names_from = code_bloc,
      values_from = nb_liste,
      values_fill = 0
    ) |>
    mutate(
      no_gauche =
        case_when(
          gauche == 0 & ecologistes == 0 ~ "LFI seule",
          .default = "LFI + gauche",
        )
    ) |>
    left_join(x = base_cf)
}
