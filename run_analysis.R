library(targets)

tar_visnetwork()

tar_make()

targets::tar_meta(fields = warnings, complete_only = TRUE)
