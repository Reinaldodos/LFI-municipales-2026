# R/fit_models.R

# =========================================================
# Formules contrefactuelles
# =========================================================

cf_formulas <- list(
  A = municipales_s ~
    europeennes_s +
    (1 + europeennes_s || code_ville),
  B = municipales_s ~
    europeennes_s +
    log_bureau_s + log_commune_s +
    (1 + europeennes_s || code_ville),
  C_full = municipales_s ~
    europeennes_s +
    gauche_euro_s +
    dominance_LFI_euro_s +
    log_bureau_s + log_commune_s +
    europeennes_s:gauche_euro_s +
    (1 + europeennes_s || code_ville),
  C_parsimonious = municipales_s ~
    europeennes_s +
    gauche_euro_s +
    dominance_LFI_euro_s +
    log_bureau_s + log_commune_s +
    (1 + europeennes_s || code_ville)
)

# =========================================================
# Estimateur générique
# =========================================================

fit_model <- function(base_cf, formula) {
  lmer(
    formula = formula,
    data = base_cf
  )
}
