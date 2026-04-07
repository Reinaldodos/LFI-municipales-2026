# ============================================================
# Inférence écologique régularisée T1 -> T2
# - prior de fidélité auto-adaptatif
# - prior abstention
# - bornes structurelles via L-BFGS-B
# - IPF / RAS optionnel
# - sortie avec RMSE
# - structure de parallélisation multi-communes
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
})

source(file = "sankey_app/R/fonctions_recode_nuances.R")
# ------------------------------------------------------------
# 1) Préparation des matrices
# ------------------------------------------------------------

prepare_flow_matrices <- function(data,
                                  bureau_col = "bureau",
                                  liste_col = "liste",
                                  tour_col = "tour",
                                  voix_col = "voix",
                                  nuance_col = NULL,
                                  tour1_value = 1,
                                  tour2_value = 2) {
  cols <- c(bureau_col, liste_col, tour_col, voix_col)
  if (!is.null(nuance_col)) cols <- c(cols, nuance_col)

  df <- data %>%
    dplyr::select(all_of(cols)) %>%
    rename(
      bureau = all_of(bureau_col),
      liste  = all_of(liste_col),
      tour   = all_of(tour_col),
      voix   = all_of(voix_col)
    ) %>%
    mutate(
      bureau = as.character(bureau),
      liste  = as.character(liste),
      voix   = as.numeric(voix)
    )

  if (!is.null(nuance_col)) {
    df <- df %>%
      rename(nuance = all_of(nuance_col)) %>%
      mutate(nuance = as.character(nuance))
  } else {
    df <- df %>% mutate(nuance = NA_character_)
  }

  df1 <- df %>%
    filter(tour == tour1_value) %>%
    group_by(bureau, liste, nuance) %>%
    summarise(voix = sum(voix, na.rm = TRUE), .groups = "drop")

  df2 <- df %>%
    filter(tour == tour2_value) %>%
    group_by(bureau, liste, nuance) %>%
    summarise(voix = sum(voix, na.rm = TRUE), .groups = "drop")

  bureaux <- intersect(unique(df1$bureau), unique(df2$bureau))

  df1 <- df1 %>% filter(bureau %in% bureaux)
  df2 <- df2 %>% filter(bureau %in% bureaux)

  X <- df1 %>%
    select(bureau, liste, voix) %>%
    pivot_wider(names_from = liste, values_from = voix, values_fill = 0) %>%
    arrange(bureau)

  Y <- df2 %>%
    select(bureau, liste, voix) %>%
    pivot_wider(names_from = liste, values_from = voix, values_fill = 0) %>%
    arrange(bureau)

  bureau_ids <- X$bureau

  X <- as.matrix(X %>% select(-bureau))
  Y <- as.matrix(Y %>% select(-bureau))

  storage.mode(X) <- "double"
  storage.mode(Y) <- "double"

  nuances_t1 <- df1 %>%
    distinct(liste, nuance) %>%
    arrange(match(liste, colnames(X))) %>%
    tibble::deframe()

  nuances_t2 <- df2 %>%
    distinct(liste, nuance) %>%
    arrange(match(liste, colnames(Y))) %>%
    tibble::deframe()

  list(
    X = X,
    Y = Y,
    bureaux = bureau_ids,
    listes_t1 = colnames(X),
    listes_t2 = colnames(Y),
    nuances_t1 = nuances_t1[colnames(X)],
    nuances_t2 = nuances_t2[colnames(Y)]
  )
}

# ------------------------------------------------------------
# 2) Outils numériques
# ------------------------------------------------------------

row_softmax <- function(M) {
  max_row <- apply(M, 1, max)
  Z <- exp(M - max_row)
  Z / rowSums(Z)
}

vec_to_transition <- function(theta, n_from, n_to) {
  M <- matrix(theta, nrow = n_from, ncol = n_to, byrow = TRUE)
  row_softmax(M)
}

weighted_rmse <- function(obs, fit, weights = NULL) {
  err2 <- (obs - fit)^2
  if (is.null(weights)) {
    return(sqrt(mean(err2)))
  }
  sqrt(sum(err2 * weights) / sum(weights))
}

# ------------------------------------------------------------
# 3) Détection automatique des cas spéciaux
# ------------------------------------------------------------

detect_common_lists <- function(listes_t1, listes_t2) {
  intersect(listes_t1, listes_t2)
}

detect_abstention_label <- function(listes,
                                    abstention_regex = "(?i)^abst|abstention") {
  idx <- grep(abstention_regex, listes, perl = TRUE)
  if (length(idx) == 0) {
    return(NA_character_)
  }
  listes[idx[1]]
}

# ------------------------------------------------------------
# 4) Recodage en blocs idéologiques
# ------------------------------------------------------------

# Couples de blocs qu'on borne fortement par défaut.
# C'est volontairement conservateur : on bride surtout les reports
# les plus absurdes, pas tout ce qui vous déplaît intellectuellement.
is_opposed_block_pair <- function(bloc_from, bloc_to) {
  if (is.na(bloc_from) || is.na(bloc_to)) {
    return(FALSE)
  }
  opposed_pairs <- rbind(
    c("gauche_radicale", "extreme_droite"),
    c("gauche", "extreme_droite"),
    c("extreme_droite", "gauche_radicale"),
    c("extreme_droite", "gauche"),
    c("droite", "gauche_radicale"),
    c("gauche_radicale", "droite")
  )
  any(apply(opposed_pairs, 1, function(x) identical(x[1], bloc_from) && identical(x[2], bloc_to)))
}

# ------------------------------------------------------------
# 5) Priors structurels
# ------------------------------------------------------------

build_structural_priors <- function(listes_t1,
                                    listes_t2,
                                    lambda_fidelity = 15,
                                    lambda_abstention = 25,
                                    abstention_regex = "(?i)^abst|abstention") {
  n_from <- length(listes_t1)
  n_to <- length(listes_t2)

  common_lists <- detect_common_lists(listes_t1, listes_t2)

  fidelity_target <- matrix(0,
    nrow = n_from, ncol = n_to,
    dimnames = list(listes_t1, listes_t2)
  )
  fidelity_mask <- matrix(0,
    nrow = n_from, ncol = n_to,
    dimnames = list(listes_t1, listes_t2)
  )

  if (length(common_lists) > 0) {
    for (lst in common_lists) {
      fidelity_target[lst, lst] <- 1
    }
    fidelity_mask[common_lists, common_lists] <- 1
  }

  abst_t1 <- detect_abstention_label(listes_t1, abstention_regex = abstention_regex)
  abst_t2 <- detect_abstention_label(listes_t2, abstention_regex = abstention_regex)

  abstention_target <- matrix(0,
    nrow = n_from, ncol = n_to,
    dimnames = list(listes_t1, listes_t2)
  )
  abstention_mask <- matrix(0,
    nrow = n_from, ncol = n_to,
    dimnames = list(listes_t1, listes_t2)
  )

  if (!is.na(abst_t1) && !is.na(abst_t2)) {
    abstention_target[abst_t1, abst_t2] <- 1
    abstention_mask[abst_t1, ] <- 1
  }

  list(
    common_lists = common_lists,
    abstention_t1 = abst_t1,
    abstention_t2 = abst_t2,
    fidelity_target = fidelity_target,
    fidelity_mask = fidelity_mask,
    abstention_target = abstention_target,
    abstention_mask = abstention_mask,
    lambda_fidelity = lambda_fidelity,
    lambda_abstention = lambda_abstention
  )
}

# ------------------------------------------------------------
# 6) Bornes sur theta pour L-BFGS-B
# ------------------------------------------------------------

build_theta_bounds <- function(listes_t1,
                               listes_t2,
                               Y = NULL,
                               nuances_t1 = NULL,
                               nuances_t2 = NULL,
                               hard_upper = 8,
                               soft_lower = -8,
                               impossible_upper = -10,
                               tiny_target_threshold = 1e-6) {
  n_from <- length(listes_t1)
  n_to <- length(listes_t2)

  lower_mat <- matrix(soft_lower,
    nrow = n_from, ncol = n_to,
    dimnames = list(listes_t1, listes_t2)
  )
  upper_mat <- matrix(hard_upper,
    nrow = n_from, ncol = n_to,
    dimnames = list(listes_t1, listes_t2)
  )

  # Si nuances fournies, on borne les reports les plus implausibles entre blocs opposés.
  if (!is.null(nuances_t1) && !is.null(nuances_t2) &&
    any(!is.na(nuances_t1)) && any(!is.na(nuances_t2))) {
    blocs_t1 <- recode_bloc_nuance(nuances_t1)
    blocs_t2 <- recode_bloc_nuance(nuances_t2)

    for (i in seq_along(listes_t1)) {
      for (j in seq_along(listes_t2)) {
        if (is_opposed_block_pair(blocs_t1[i], blocs_t2[j])) {
          upper_mat[i, j] <- impossible_upper
        }
      }
    }
  } else if (!is.null(Y)) {
    # Garde-fou minimal si pas de nuances :
    # si une liste T2 est quasi nulle dans l'ensemble de la commune,
    # on borne son attractivité pour éviter qu'elle "aspire" du bruit.
    totals_t2 <- colSums(Y)
    tiny_cols <- names(totals_t2)[totals_t2 <= tiny_target_threshold]
    if (length(tiny_cols) > 0) {
      upper_mat[, tiny_cols] <- impossible_upper
    }
  }

  list(
    lower = as.vector(lower_mat),
    upper = as.vector(upper_mat),
    lower_mat = lower_mat,
    upper_mat = upper_mat
  )
}

# ------------------------------------------------------------
# 7) Initialisation utile des theta
# ------------------------------------------------------------

build_theta_init <- function(listes_t1,
                             listes_t2,
                             common_lists = NULL,
                             abstention_t1 = NA_character_,
                             abstention_t2 = NA_character_,
                             base = -1,
                             bonus_common = 2,
                             bonus_abstention = 3) {
  M <- matrix(base,
    nrow = length(listes_t1), ncol = length(listes_t2),
    dimnames = list(listes_t1, listes_t2)
  )

  if (!is.null(common_lists) && length(common_lists) > 0) {
    for (lst in common_lists) {
      M[lst, lst] <- M[lst, lst] + bonus_common
    }
  }

  if (!is.na(abstention_t1) && !is.na(abstention_t2)) {
    M[abstention_t1, abstention_t2] <- M[abstention_t1, abstention_t2] + bonus_abstention
  }

  as.vector(M)
}

# ------------------------------------------------------------
# 8) Fonction de perte
# ------------------------------------------------------------

flow_loss_structural <- function(theta,
                                 X,
                                 Y,
                                 priors,
                                 lambda_ridge = 0.25,
                                 weight_mode = c("sqrt", "none", "size")) {
  weight_mode <- match.arg(weight_mode)

  n_from <- ncol(X)
  n_to <- ncol(Y)

  P <- vec_to_transition(theta, n_from, n_to)
  Y_hat <- X %*% P
  R <- Y - Y_hat

  weights <- switch(weight_mode,
    "none" = rep(1, nrow(X)),
    "sqrt" = sqrt(pmax(rowSums(X), 1)),
    "size" = pmax(rowSums(X), 1)
  )

  fit_term <- sum((R^2) * weights)

  fidelity_term <- priors$lambda_fidelity *
    sum(((P - priors$fidelity_target)^2) * priors$fidelity_mask)

  abstention_term <- priors$lambda_abstention *
    sum(((P - priors$abstention_target)^2) * priors$abstention_mask)

  ridge_term <- lambda_ridge * sum(theta^2)

  fit_term + fidelity_term + abstention_term + ridge_term
}

# ------------------------------------------------------------
# 9) IPF / RAS pour coller exactement aux marges
# ------------------------------------------------------------

ipf_balance_flows <- function(F,
                              target_row_sums = rowSums(F),
                              target_col_sums,
                              maxit = 1000,
                              tol = 1e-10,
                              eps = 1e-12) {
  G <- pmax(F, eps)

  for (iter in seq_len(maxit)) {
    row_scale <- target_row_sums / pmax(rowSums(G), eps)
    G <- G * row_scale

    col_scale <- target_col_sums / pmax(colSums(G), eps)
    G <- sweep(G, 2, col_scale, `*`)

    row_gap <- max(abs(rowSums(G) - target_row_sums))
    col_gap <- max(abs(colSums(G) - target_col_sums))

    if (max(row_gap, col_gap) < tol) {
      return(list(F = G, converged = TRUE, iterations = iter))
    }
  }

  list(F = G, converged = FALSE, iterations = maxit)
}

# ------------------------------------------------------------
# 10) Estimation commune par commune
# ------------------------------------------------------------

estimate_flows_regularized <- function(data,
                                       bureau_col = "bureau",
                                       liste_col = "liste",
                                       tour_col = "tour",
                                       voix_col = "voix",
                                       nuance_col = "nuance",
                                       tour1_value = 1,
                                       tour2_value = 2,
                                       lambda_fidelity = 15,
                                       lambda_abstention = 25,
                                       lambda_ridge = 0.25,
                                       weight_mode = c("sqrt", "none", "size"),
                                       use_ipf = TRUE,
                                       ipf_maxit = 1000,
                                       maxit = 5000,
                                       reltol = 1e-10,
                                       abstention_regex = "(?i)^abst|abstention") {
  weight_mode <- match.arg(weight_mode)

  mats <- prepare_flow_matrices(
    data = data,
    bureau_col = bureau_col,
    liste_col = liste_col,
    tour_col = tour_col,
    voix_col = voix_col,
    nuance_col = nuance_col,
    tour1_value = tour1_value,
    tour2_value = tour2_value
  )

  X <- mats$X
  Y <- mats$Y

  n_from <- ncol(X)
  n_to <- ncol(Y)

  priors <- build_structural_priors(
    listes_t1 = mats$listes_t1,
    listes_t2 = mats$listes_t2,
    lambda_fidelity = lambda_fidelity,
    lambda_abstention = lambda_abstention,
    abstention_regex = abstention_regex
  )

  bounds <- build_theta_bounds(
    listes_t1 = mats$listes_t1,
    listes_t2 = mats$listes_t2,
    Y = Y,
    nuances_t1 = mats$nuances_t1,
    nuances_t2 = mats$nuances_t2
  )

  theta_init <- build_theta_init(
    listes_t1 = mats$listes_t1,
    listes_t2 = mats$listes_t2,
    common_lists = priors$common_lists,
    abstention_t1 = priors$abstention_t1,
    abstention_t2 = priors$abstention_t2
  )

  theta_init <- pmin(pmax(theta_init, bounds$lower), bounds$upper)

  opt <- optim(
    par = theta_init,
    fn = flow_loss_structural,
    X = X,
    Y = Y,
    priors = priors,
    lambda_ridge = lambda_ridge,
    weight_mode = weight_mode,
    method = "L-BFGS-B",
    lower = bounds$lower,
    upper = bounds$upper,
    control = list(maxit = maxit, factr = 1e7, pgtol = reltol)
  )

  P <- vec_to_transition(opt$par, n_from, n_to)
  rownames(P) <- mats$listes_t1
  colnames(P) <- mats$listes_t2

  Y_hat <- X %*% P
  rownames(Y_hat) <- mats$bureaux
  colnames(Y_hat) <- mats$listes_t2

  rownames(Y) <- mats$bureaux
  colnames(Y) <- mats$listes_t2

  total_t1 <- colSums(X)
  total_t2 <- colSums(Y)

  F_raw <- diag(total_t1) %*% P
  rownames(F_raw) <- mats$listes_t1
  colnames(F_raw) <- mats$listes_t2

  if (use_ipf) {
    ipf <- ipf_balance_flows(
      F = F_raw,
      target_row_sums = total_t1,
      target_col_sums = total_t2,
      maxit = ipf_maxit
    )
    F <- ipf$F
  } else {
    ipf <- list(F = F_raw, converged = NA, iterations = 0)
    F <- F_raw
  }

  rmse_global <- weighted_rmse(Y, Y_hat)
  rmse_weighted <- weighted_rmse(
    Y, Y_hat,
    weights = matrix(rep(sqrt(pmax(rowSums(X), 1)), ncol(Y)), nrow = nrow(Y), byrow = FALSE)
  )

  list(
    transition_matrix = P,
    aggregate_flows = F,
    aggregate_flows_raw = F_raw,
    fitted_t2 = Y_hat,
    observed_t2 = Y,
    X = X,
    bureaux = mats$bureaux,
    listes_t1 = mats$listes_t1,
    listes_t2 = mats$listes_t2,
    common_lists = priors$common_lists,
    abstention_t1 = priors$abstention_t1,
    abstention_t2 = priors$abstention_t2,
    rmse = rmse_global,
    rmse_weighted = rmse_weighted,
    convergence = opt$convergence,
    value = opt$value,
    ipf_converged = ipf$converged,
    ipf_iterations = ipf$iterations,
    bounds_upper = bounds$upper_mat,
    bounds_lower = bounds$lower_mat,
    optim = opt
  )
}

# ------------------------------------------------------------
# 11) Helpers de sortie
# ------------------------------------------------------------

tidy_transition_matrix <- function(mod_flux) {
  mod_flux$transition_matrix %>%
    as.data.frame() %>%
    rownames_to_column(var = "liste_t1") %>%
    pivot_longer(
      cols = -liste_t1,
      names_to = "liste_t2",
      values_to = "part_report"
    )
}

tidy_aggregate_flows <- function(mod_flux) {
  mod_flux$aggregate_flows %>%
    as.data.frame() %>%
    rownames_to_column(var = "liste_t1") %>%
    pivot_longer(
      cols = -liste_t1,
      names_to = "liste_t2",
      values_to = "voix_flux"
    )
}

get_reports <- function(mod_flux, digits = 3) {
  tidy_transition_matrix(mod_flux) %>%
    mutate(part_report = round(part_report, digits = digits)) %>%
    filter(part_report > 0) %>%
    pivot_wider(names_from = "liste_t2", values_from = "part_report")
}

get_flux_voix <- function(resultats, mod_flux) {
  liste_nuancees <-
    resultats |>
    distinct(tour, liste, nuance) |>
    mutate(
      liste_nuance =
        case_when(
          is.na(nuance) ~ glue::glue("{tour} : {liste}"),
          .default = glue::glue("{tour} : {liste} ({nuance})")
        )
    ) |>
    mutate(liste_nuance) %>%
    split(f = .$tour)

  flux_voix <- tidy_aggregate_flows(mod_flux)

  flux_voix |>
    right_join(
      x = liste_nuancees$t1,
      by = join_by(liste == liste_t1)
    ) |>
    rename(
      liste_t1 = liste_nuance,
      nuance_t1 = nuance
    ) |>
    select(-tour, -liste) |>
    right_join(
      x = liste_nuancees$t2,
      by = join_by(liste == liste_t2)
    ) |>
    rename(liste_t2 = liste_nuance) |>
    select(-tour, -liste, -nuance) |>
    mutate(
      nuance_t1 = recode_bloc_nuance(nuance_t1)
    )
}
