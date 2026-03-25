library(dplyr)
library(networkD3)
library(htmlwidgets)

plot_sankey_networkD3 <- function(data,
                                  palette_nuances,
                                  seuil_flux = 1,
                                  regrouper_non_exprimes) {
  df <- data %>%
    transmute(
      liste_t1  = as.character(liste_t1),
      nuance_t1 = as.character(nuance_t1),
      liste_t2  = as.character(liste_t2),
      voix_flux = as.numeric(voix_flux)
    ) %>%
    filter(!is.na(voix_flux), voix_flux >= seuil_flux)

  if (regrouper_non_exprimes) {
    df <- df %>%
      mutate(
        liste_t2 = case_when(
          liste_t2 %in% c("t2 : abstentions", "t2 : blancs", "t2 : nuls") ~ "t2 : non exprimés",
          .default = liste_t2
        ),
        liste_t1 = case_when(
          liste_t1 %in% c("t1 : abstentions", "t1 : blancs", "t1 : nuls") ~ "t1 : non exprimés",
          .default = liste_t1
        )
      ) %>%
      group_by(liste_t1, nuance_t1, liste_t2) %>%
      summarise(voix_flux = sum(voix_flux), .groups = "drop")
  }

  # NODES
  nodes <- tibble(
    name = unique(c(df$liste_t1, df$liste_t2))
  ) %>%
    mutate(id = row_number() - 1)

  # LINKS
  links <- df %>%
    left_join(nodes, by = c("liste_t1" = "name")) %>%
    rename(source = id) %>%
    left_join(nodes, by = c("liste_t2" = "name")) %>%
    rename(target = id) %>%
    transmute(
      source,
      target,
      value = voix_flux,
      group = nuance_t1
    )

  # Construction du JS pour la palette
  domain_js <- paste0('"', names(palette_nuances), '"', collapse = ",")
  range_js <- paste0('"', palette_nuances, '"', collapse = ",")

  colour_scale <- JS(
    sprintf(
      "d3.scaleOrdinal().domain([%s]).range([%s])",
      domain_js,
      range_js
    )
  )

  sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    LinkGroup = "group",
    colourScale = colour_scale,
    fontSize = 12,
    nodeWidth = 25
  )
}
