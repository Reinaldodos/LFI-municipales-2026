library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(lme4)
library(broom.mixed)

source(file = "../LFI muni26/fetch data.R", echo = TRUE)
# =========================================================
# 0) Hypothèses minimales sur les objets existants
# =========================================================
# - input : résultats municipales 2026 au niveau liste x bureau
# - output_LFI : contient geo_id + europeennes_2024 (part des inscrits)
# - bureaux : contient geo_id + code_ville
#
# europeennes_2024 est supposé être déjà en part des inscrits.
# input inclut abstention, blancs et nuls.

# =========================================================
# 1) Nettoyage minimal des libellés spéciaux
# =========================================================

input_clean <- input |>
  mutate(
    liste_upper = str_to_upper(liste),
    nuance_upper = str_to_upper(nuance),
    type_vote = case_when(
      str_detect(liste_upper, "ABSTENTION") ~ "abstention",
      str_detect(liste_upper, "BLANC") ~ "blanc",
      str_detect(liste_upper, "NUL") ~ "nul",
      TRUE ~ "liste"
    )
  )

# =========================================================
# 2) Totaux bureau : inscrits, abstention, blancs+nuls, exprimés
# =========================================================
# Ici, comme input est "en inscrits", la somme des voix par bureau
# doit reconstituer le nombre d'inscrits.

totaux_bureau <- input_clean |>
  group_by(geo_id) |>
  summarise(
    inscrits = sum(voix, na.rm = TRUE),
    abstention_voix = sum(voix[type_vote == "abstention"], na.rm = TRUE),
    blancs_voix = sum(voix[type_vote == "blanc"], na.rm = TRUE),
    nuls_voix = sum(voix[type_vote == "nul"], na.rm = TRUE),
    bn_voix = blancs_voix + nuls_voix,
    exprimes_voix = sum(voix[type_vote == "liste"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    participation = (exprimes_voix + bn_voix) / inscrits,
    abstention    = abstention_voix / inscrits,
    blancs_nuls   = bn_voix / inscrits,
    exprimes_part = exprimes_voix / inscrits
  )

# =========================================================
# 3) Score LFI municipales 2026 en part des inscrits
# =========================================================
# Adaptez la règle si plusieurs nuances doivent être agrégées à LFI.
# Ici on prend la nuance LFI stricte.

score_lfi_muni <- input_clean |>
  filter(type_vote == "liste") |>
  group_by(geo_id) |>
  summarise(
    municipales_2026 = sum(voix[nuance_upper == "LFI"], na.rm = TRUE) / first(sum(voix, na.rm = TRUE)),
    .groups = "drop"
  )

# La ligne ci-dessus est fragile car first(sum(...)) dans summarise n'est pas idéal.
# On la refait proprement en joignant les inscrits.

score_lfi_muni <- input_clean |>
  filter(type_vote == "liste") |>
  group_by(geo_id) |>
  summarise(
    voix_lfi = sum(voix[nuance_upper == "LFI"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    totaux_bureau |>
      select(geo_id, inscrits),
    by = "geo_id"
  ) |>
  mutate(
    municipales_2026 = voix_lfi / inscrits
  ) |>
  select(geo_id, municipales_2026)

# =========================================================
# 4) Regroupement des nuances en blocs
# =========================================================
# Vous pouvez évidemment raffiner cette nomenclature.

input_blocs <- input_clean |>
  mutate(
    bloc = case_when(
      type_vote != "liste" ~ NA_character_,
      nuance_upper %in% c("LFI", "LSOC", "LUG", "LDVG", "LEXG") ~ "gauche",
      nuance_upper %in% c("LDVC", "LUDI", "LMDM", "LUC") ~ "centre",
      nuance_upper %in% c("LLR", "LUD", "LUDR", "LDVD", "LHOR") ~ "droite",
      nuance_upper %in% c("LRN", "LUXD", "LREC", "LEXD") ~ "ext_droite",
      TRUE ~ "autres"
    )
  )

# =========================================================
# 5) Poids des blocs en part des inscrits
# =========================================================

parts_bloc_inscrits <- input_blocs |>
  filter(type_vote == "liste") |>
  group_by(geo_id, bloc) |>
  summarise(
    voix_bloc = sum(voix, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    totaux_bureau |>
      select(geo_id, inscrits),
    by = "geo_id"
  ) |>
  mutate(
    part_bloc_inscrits = voix_bloc / inscrits
  ) |>
  select(geo_id, bloc, part_bloc_inscrits) |>
  pivot_wider(
    names_from  = bloc,
    values_from = part_bloc_inscrits,
    values_fill = 0
  )

# =========================================================
# 6) Poids des blocs en part des exprimés
# =========================================================
# Utile si vous voulez comparer les deux référentiels.

parts_bloc_exprimes <- input_blocs |>
  filter(type_vote == "liste") |>
  group_by(geo_id, bloc) |>
  summarise(
    voix_bloc = sum(voix, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    totaux_bureau |>
      select(geo_id, exprimes_voix),
    by = "geo_id"
  ) |>
  mutate(
    part_bloc_exprimes = if_else(exprimes_voix > 0, voix_bloc / exprimes_voix, 0)
  ) |>
  select(geo_id, bloc, part_bloc_exprimes) |>
  pivot_wider(
    names_from = bloc,
    values_from = part_bloc_exprimes,
    values_fill = 0,
    names_prefix = "expr_"
  )

# =========================================================
# 7) Fragmentation sur les exprimés
# =========================================================
# HHI calculé sur les listes, puis fragmentation = 1 - HHI

frag <- input_clean |>
  filter(type_vote == "liste") |>
  group_by(geo_id) |>
  mutate(
    p_expr = voix / sum(voix, na.rm = TRUE)
  ) |>
  summarise(
    hhi = sum(p_expr^2, na.rm = TRUE),
    fragmentation = 1 - hhi,
    nb_listes = n(),
    .groups = "drop"
  )


# =========================================================
# 8) Bloc arrivé en tête (optionnel)
# =========================================================

bloc_en_tete <- input_blocs |>
  filter(type_vote == "liste") |>
  group_by(geo_id, bloc) |>
  summarise(
    voix_bloc = sum(voix, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(geo_id) |>
  slice_max(order_by = voix_bloc, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(bloc_tete = bloc)

# =========================================================
# 9) Base finale bureau
# =========================================================

source(file = "../LFI muni26/output LFI.R", echo = TRUE)
source(file = "../LFI muni26/graphique villes cibles.R", echo = TRUE)

base_modele <- output_LFI |>
  select(geo_id, europeennes_2024) |>
  left_join(score_lfi_muni, by = "geo_id") |>
  left_join(totaux_bureau, by = "geo_id") |>
  left_join(parts_bloc_inscrits, by = "geo_id") |>
  left_join(parts_bloc_exprimes, by = "geo_id") |>
  left_join(frag, by = "geo_id") |>
  left_join(bloc_en_tete, by = "geo_id") |>
  left_join(
    bureaux |>
      tidyr::unite(col = "code_ville", code_departement, code_commune) |>
      select(geo_id, code_ville),
    by = "geo_id"
  ) |>
  mutate(
    progression = municipales_2026 - europeennes_2024,
    municipales_s = as.numeric(scale(municipales_2026)),
    europeennes_s = as.numeric(scale(europeennes_2024)),
    progression_s = as.numeric(scale(progression)),
    participation_s = as.numeric(scale(participation)),
    abstention_s = as.numeric(scale(abstention)),
    blancs_nuls_s = as.numeric(scale(blancs_nuls)),
    fragmentation_s = as.numeric(scale(fragmentation)),
    gauche_s = as.numeric(scale(gauche)),
    centre_s = as.numeric(scale(centre)),
    droite_s = as.numeric(scale(droite)),
    ext_droite_s = as.numeric(scale(ext_droite))
  )

# Vérification rapide
glimpse(base_modele)
summary(select(
  base_modele, municipales_2026, europeennes_2024, progression,
  participation, abstention, blancs_nuls,
  gauche, centre, droite, ext_droite, fragmentation
))
