select_ville <- function(code) {
  patt <- str_c("^", code)
  output_LFI |>
    filter(str_detect(string = geo_id, pattern = patt))
}

selection <-
  list(
    "Saint-Denis" = "93_066",
    "Lille" = "59_350",
    "Paris" = "75",
    "Marseille" = "13_055",
    "Lyon" = "69_123",
    "Nantes" = "44_109",
    "Rennes" = "35_238",
    "Toulouse" = "31_555"
  ) |>
  map(.f = ~ data.frame(code = .)) |>
  bind_rows(.id = "ville") |>
  mutate(data = map(.x = code, .f = select_ville)) |>
  unnest(cols = c(data))

output_LFI |>
  ggplot(mapping = aes(
    x = europeennes_2024,
    y = municipales_2026
  )) +
  geom_abline() +
  geom_point() +
  geom_point(
    data = selection,
    mapping = aes(colour = ville)
  ) +
  scale_colour_discrete(palette = "Dark2") +
  theme_minimal() +
  labs(
    title = "Evolution des scores LFI par bdv",
    subtitle = "en parts des inscrits",
    caption = "évolution entre les européennes 2024 et les municipales 2026"
  ) +
  facet_wrap(~ville)

selection |>
  ggplot(mapping = aes(
    x = europeennes_2024,
    y = municipales_2026
  )) +
  geom_abline() +
  geom_point(mapping = aes(colour = ville)) +
  scale_colour_discrete(palette = "Dark2") +
  theme(legend.position = "none") +
  labs(
    title = "Evolution des scores LFI par bdv",
    subtitle = "en parts des inscrits",
    caption = "évolution entre les européennes 2024 et les municipales 2026"
  ) +
  facet_wrap(~ville)
