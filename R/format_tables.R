format_residual_summary <- function(x) {
  x |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), \(x) round(x, 3))
    ) |>
    dplyr::rename(
      Moyenne = mean,
      `Écart-type` = sd,
      Min = min,
      Q01 = q01,
      Q05 = q05,
      Médiane = median,
      Q95 = q95,
      Q99 = q99,
      Max = max
    )
}

format_ranef_tail <- function(x) {
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), \(x) scales::percent(x, accuracy = 0.1))
    ) |>
    dplyr::rename(
      `Top 5%` = part_95,
      `Top 1%` = part_99,
      `> 1σ` = part_gt1,
      `< -1σ` = part_lt_minus1
    )
}
