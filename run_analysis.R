library(targets)

# tar_visnetwork()

tar_manifest()

tar_make()

targets::tar_meta(fields = warnings, complete_only = TRUE)

targets::tar_meta(fields = error, complete_only = TRUE) |>
  filter(name == "commune_random_effects_A") |>
  pull(error)
