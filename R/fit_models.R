# R/fit_models.R

# =========================================================
# Formules contrefactuelles
# =========================================================

cf_formulas <- list(
  A = municipales_s ~
    europeennes_s +
    (1 + europeennes_s | code_ville),
  B = municipales_s ~
    europeennes_s +
    log_bureau_s + log_commune_s +
    (1 + europeennes_s | code_ville),
  C_full = municipales_s ~
    europeennes_s + gauche_euro_s + dominance_LFI_euro_s +
    log_bureau_s + log_commune_s +
    europeennes_s:gauche_euro_s +
    (1 + europeennes_s | code_ville),
  C_no_VIF = municipales_s ~
    gauche_euro_s * dominance_LFI_euro_s +
    log_bureau_s + log_commune_s +
    (1 + europeennes_s | code_ville),
  C_ortho = municipales_s ~
    europeennes_s * gauche_orth +
    log_bureau_s + log_commune_s +
    (1 + europeennes_s | code_ville),
  rand_int = municipales_s ~ 0 + europeennes_s * gauche_orth +
    log_bureau_s +
    (1 + europeennes_s + europeennes_s:gauche_orth | code_ville),
  rand_int_no_gauche = municipales_s ~ 0 + europeennes_s * gauche_orth +
    no_gauche +
    log_bureau_s +
    (1 + europeennes_s + europeennes_s:gauche_orth | code_ville)
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
