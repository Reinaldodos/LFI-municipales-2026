library(dplyr)
library(lme4)
library(broom.mixed)
library(ggplot2)

source(file = "../LFI muni26/base modele.R", echo = TRUE)

# ---------------------------------------------------------
# 1) Taille des communes
# ---------------------------------------------------------
taille_commune <- base_modele |>
  group_by(code_ville) |>
  summarise(
    inscrits_commune = sum(inscrits, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 2) Base contrefactuelle
# ---------------------------------------------------------
base_cf <- base_modele |>
  left_join(taille_commune, by = "code_ville") |>
  mutate(
    log_inscrits_bureau  = log(inscrits),
    log_inscrits_commune = log(inscrits_commune),
    municipales_s        = as.numeric(scale(municipales_2026)),
    europeennes_s        = as.numeric(scale(europeennes_2024)),
    log_bureau_s         = as.numeric(scale(log_inscrits_bureau)),
    log_commune_s        = as.numeric(scale(log_inscrits_commune))
  ) |>
  filter(
    !is.na(municipales_s),
    !is.na(europeennes_s),
    !is.na(log_bureau_s),
    !is.na(log_commune_s)
  )

m_cf_A <- lmer(
  municipales_s ~ europeennes_s + (1 + europeennes_s || code_ville),
  data = base_cf
)

summary(m_cf_A)

m_cf_B <- lmer(
  municipales_s ~ europeennes_s +
    log_bureau_s + log_commune_s +
    (1 + europeennes_s || code_ville),
  data = base_cf
)

summary(m_cf_B)

# Exemple si ces variables existent déjà
euros_2024 <-
  dplyr::tbl(con, "gold_fact_votes") |>
  filter(
    scrutin == "europeennes_2024",
    voix > 0
  ) |>
  collect() |>
  tidyr::unite(col = "liste", scrutin, bulletin_id) |>
  fetch_nom_listes(
    extracted_col = "liste",
    db_path = targets::tar_read("duckdb_path")
  ) |>
  group_by(geo_id) |>
  mutate(part = voix / sum(voix)) |>
  ungroup()

gauche_euro <-
  euros_2024 |>
  summarise(
    voix = sum(voix),
    .by = c(nom)
  ) |>
  arrange(-voix) |>
  mutate(part = voix / sum(voix)) |>
  filter(part > 1 / 100) |>
  mutate(
    bloc = case_match(
      .x = nom,
      "REVIL EUR" ~ "gauche",
      "LFI - UP" ~ "gauche",
      "EUROPE ÉCOLOGIE" ~ "gauche",
      "GAUCHE UNIE" ~ "gauche"
    )
  ) |>
  drop_na() |>
  semi_join(
    x = euros_2024,
    by = join_by(nom)
  ) |>
  summarise(
    gauche_euro = sum(part),
    .by = c(geo_id)
  )

gauche_euro <-
  euros_2024 |>
  filter(nom == "LFI - UP") |>
  inner_join(
    y = gauche_euro,
    by = join_by(geo_id)
  ) |>
  transmute(geo_id, gauche_euro,
    dominance_LFI_euro = part / gauche_euro
  )

base_cf <-
  base_cf |>
  left_join(
    y = gauche_euro,
    by = join_by(geo_id)
  ) |>
  mutate(
    gauche_euro_s = as.numeric(scale(gauche_euro)),
    dominance_LFI_euro_s = as.numeric(scale(dominance_LFI_euro))
  )

m_cf_C <- lmer(
  municipales_s ~ europeennes_s +
    gauche_euro_s +
    dominance_LFI_euro_s +
    log_bureau_s + log_commune_s +
    europeennes_s:gauche_euro_s +
    (1 + europeennes_s || code_ville),
  data = base_cf
)

summary(m_cf_C)


anova(m_cf_A, m_cf_B)

# Si m_cf_C existe
anova(m_cf_B, m_cf_C)

broom.mixed::tidy(m_cf_A, effects = "fixed", conf.int = TRUE)
broom.mixed::tidy(m_cf_B, effects = "fixed", conf.int = TRUE)
broom.mixed::tidy(m_cf_C, effects = "fixed", conf.int = TRUE)

base_cf <- base_cf |>
  mutate(
    pred = predict(m_cf_C),
    resid = municipales_s - pred
  )

weighted.mean(base_cf$resid, w = base_cf$inscrits)

ggplot(base_cf, aes(x = resid, weight = inscrits)) +
  geom_histogram(bins = 50)

base_cf |>
  select(geo_id, resid, inscrits) |>
  slice_max(order_by = resid, n = 20) |>
  inner_join(
    x = bureaux,
    by = join_by(geo_id)
  )

library(sjPlot)

re_plot <- ranef(m_cf_C)$code_ville |>
  rownames_to_column("code_ville") |>
  rename(
    intercept = `(Intercept)`,
    slope = europeennes_s
  ) |>
  left_join(
    bureaux |>
      tidyr::unite(col = "code_ville", code_departement, code_commune, remove = FALSE) |>
      distinct(code_ville, libelle_commune),
    by = "code_ville"
  )

ggplot(re_plot, aes(x = intercept)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Effet aléatoire communal",
    y = "Nombre de communes",
    title = "Distribution des effets aléatoires communaux"
  )

re_all <- re_plot |>
  left_join(
    base_cf |>
      group_by(code_ville) |>
      summarise(inscrits_commune = sum(inscrits), .groups = "drop"),
    by = "code_ville"
  ) |>
  arrange(intercept) |>
  mutate(rang = row_number())

ggplot(re_all, aes(x = rang, y = intercept, size = inscrits_commune)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Rang des communes",
    y = "Effet aléatoire communal",
    size = "Inscrits",
    title = "Distribution des effets aléatoires communaux"
  )

re_all |>
  summarise(
    part_95 = sum(inscrits_commune[intercept > quantile(intercept, 0.95)]) / sum(inscrits_commune),
    part_99 = sum(inscrits_commune[intercept > quantile(intercept, 0.99)]) / sum(inscrits_commune),
    part_gt1 = sum(inscrits_commune[intercept > 1]) / sum(inscrits_commune)
  )
