palette_nuances <- c(
  "NA"   = "#BDBDBD",

  # Gauche / extrême gauche
  "LEXG" = "#8B0000",
  "LFI"  = "red",
  "LCOM" = "red",
  "LUG"  = "#E57373",
  "LSOC" = "pink",
  "LDVG" = "pink",

  # Écologistes / régionalistes / divers sensibilité verte
  "LECO" = "#43A047",
  "LVEC" = "#66BB6A",
  "LREG" = "#7CB342",

  # Centre / macronie / divers centre
  "LUC"  = "#FBC02D",
  "LDVC" = "#FFD54F",
  "LREN" = "#FFCA28",
  "LMDM" = "#FFB300",

  # Droite modérée / centre droit / divers droite
  "LLR"  = "#1E88E5",
  "LUDI" = "#42A5F5",
  "LUDI" = "#42A5F5", # volontaire si vous harmonisez mal en amont
  "LUD"  = "#64B5F6",
  "LUDR" = "#5C6BC0",
  "LDVD" = "#90CAF9",
  "LREC" = "#1565C0",
  "LHOR" = "#3949AB",
  "LDSV" = "#7986CB",

  # Extrême droite / droite radicale
  "LRN"  = "black",
  "LEXD" = "black",
  "LUXD" = "black",

  # Divers / inclassables
  "LDIV" = "#9E9E9E"
)

palette_bloc <- c(
  "gauche_radicale" = "#B71C1C",
  "gauche"          = "#E57373",
  "ecologistes"     = "#66BB6A",
  "centre"          = "#FBC02D",
  "droite"          = "#42A5F5",
  "extreme_droite"  = "#283593",
  "divers"          = "#9E9E9E"
)

recode_bloc_nuance <- function(x) {
  dplyr::case_when(
    # Gauche radicale
    x %in% c("LEXG", "LFI", "LCOM") ~ "gauche_radicale",

    # Gauche
    x %in% c("LUG", "LSOC", "LDVG") ~ "gauche",

    # Écologistes / régionalistes
    x %in% c("LECO", "LVEC", "LREG") ~ "ecologistes",

    # Centre (inclut LUDI comme vous l’indiquez)
    x %in% c("LUC", "LDVC", "LREN", "LMDM", "LUDI") ~ "centre",

    # Droite classique
    x %in% c("LLR", "LUD", "LDVD", "LREC", "LHOR", "LDSV") ~ "droite",

    # Extrême droite (inclut LUDR)
    x %in% c("LRN", "LEXD", "LUXD", "LUDR") ~ "extreme_droite",

    # Divers / NA
    x %in% c("LDIV", "NA") ~ "divers",
    TRUE ~ "divers"
  )
}
