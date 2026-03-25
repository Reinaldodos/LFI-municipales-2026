library(dplyr)
library(tidyr)

prepare_flow_matrices <- function(data,
                                  bureau_col = "bureau",
                                  liste_col = "liste",
                                  tour_col = "tour",
                                  voix_col = "voix",
                                  tour1_value = 1,
                                  tour2_value = 2) {
  df <- data %>%
    dplyr::select(
      bureau = all_of(bureau_col),
      liste  = all_of(liste_col),
      tour   = all_of(tour_col),
      voix   = all_of(voix_col)
    ) %>%
    mutate(
      bureau = as.character(bureau),
      liste  = as.character(liste)
    )

  df1 <- df %>%
    filter(tour == tour1_value) %>%
    group_by(bureau, liste) %>%
    summarise(voix = sum(voix), .groups = "drop")

  df2 <- df %>%
    filter(tour == tour2_value) %>%
    group_by(bureau, liste) %>%
    summarise(voix = sum(voix), .groups = "drop")

  bureaux <- intersect(unique(df1$bureau), unique(df2$bureau))

  X <- df1 %>%
    filter(bureau %in% bureaux) %>%
    pivot_wider(names_from = liste, values_from = voix, values_fill = 0) %>%
    arrange(bureau)

  Y <- df2 %>%
    filter(bureau %in% bureaux) %>%
    pivot_wider(names_from = liste, values_from = voix, values_fill = 0) %>%
    arrange(bureau)

  bureau_ids <- X$bureau

  X <- as.matrix(X %>% select(-bureau))
  Y <- as.matrix(Y %>% select(-bureau))

  storage.mode(X) <- "double"
  storage.mode(Y) <- "double"

  list(
    X = X,
    Y = Y,
    bureaux = bureau_ids,
    listes_t1 = colnames(X),
    listes_t2 = colnames(Y)
  )
}

row_softmax <- function(M) {
  max_row <- apply(M, 1, max)
  Z <- exp(M - max_row)
  Z / rowSums(Z)
}

vec_to_transition <- function(theta, n_from, n_to) {
  M <- matrix(theta, nrow = n_from, ncol = n_to, byrow = TRUE)
  row_softmax(M)
}

entropy_penalty <- function(P, eps = 1e-12) {
  # On pénalise le manque d'entropie:
  # plus une ligne est proche d'un coin, plus la pénalité est forte
  H <- -rowSums(P * log(P + eps))
  # signe moins pour pénaliser les entropies trop faibles
  -sum(H)
}

flow_loss_regularized <- function(theta, X, Y,
                                  lambda_entropy = 0,
                                  lambda_ridge = 0,
                                  weight_mode = c("sqrt", "none", "size")) {
  weight_mode <- match.arg(weight_mode)

  n_from <- ncol(X)
  n_to <- ncol(Y)

  P <- vec_to_transition(theta, n_from, n_to)
  Y_hat <- X %*% P
  R <- Y - Y_hat

  weights <- switch(weight_mode,
    "none" = rep(1, nrow(X)),
    "sqrt" = sqrt(rowSums(X)),
    "size" = rowSums(X)
  )

  fit_term <- sum((R^2) * weights)

  ent_term <- lambda_entropy * entropy_penalty(P)
  ridge_term <- lambda_ridge * sum(theta^2)

  fit_term + ent_term + ridge_term
}

estimate_flows_regularized <- function(data,
                                       bureau_col = "bureau",
                                       liste_col = "liste",
                                       tour_col = "tour",
                                       voix_col = "voix",
                                       tour1_value = 1,
                                       tour2_value = 2,
                                       lambda_entropy = 100,
                                       lambda_ridge = 1,
                                       weight_mode = c("sqrt", "none", "size"),
                                       maxit = 5000) {
  weight_mode <- match.arg(weight_mode)

  mats <- prepare_flow_matrices(
    data = data,
    bureau_col = bureau_col,
    liste_col = liste_col,
    tour_col = tour_col,
    voix_col = voix_col,
    tour1_value = tour1_value,
    tour2_value = tour2_value
  )

  X <- mats$X
  Y <- mats$Y

  n_from <- ncol(X)
  n_to <- ncol(Y)

  theta_init <- rep(0, n_from * n_to)

  opt <- optim(
    par = theta_init,
    fn = flow_loss_regularized,
    X = X,
    Y = Y,
    lambda_entropy = lambda_entropy,
    lambda_ridge = lambda_ridge,
    weight_mode = weight_mode,
    method = "BFGS",
    control = list(maxit = maxit, reltol = 1e-10)
  )

  P <- vec_to_transition(opt$par, n_from, n_to)
  rownames(P) <- mats$listes_t1
  colnames(P) <- mats$listes_t2

  Y_hat <- X %*% P
  colnames(Y_hat) <- mats$listes_t2
  rownames(Y_hat) <- mats$bureaux

  total_t1 <- colSums(X)
  F <- diag(total_t1) %*% P
  rownames(F) <- mats$listes_t1
  colnames(F) <- mats$listes_t2

  list(
    transition_matrix = P,
    aggregate_flows = F,
    fitted_t2 = Y_hat,
    observed_t2 = Y,
    convergence = opt$convergence,
    value = opt$value,
    optim = opt
  )
}
