library(shiny)
library(dplyr)
library(purrr)

# Charger vos fonctions
"R" |>
  list.files(
    pattern = "R$",
    recursive = TRUE, full.names = TRUE
  ) |>
  map(.f = source)

# Charger les données
reports <- readRDS("data/reports.rds")

ref_communes <- reports %>%
  distinct(code_departement, libelle_commune) %>%
  arrange(code_departement, libelle_commune)

#--------------------------------------------------
# UI
#--------------------------------------------------

ui <- fluidPage(
  titlePanel("Sankey des reports de voix"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "code_departement",
        "Département",
        choices = sort(unique(ref_communes$code_departement)),
        selected = "75"
      ),
      selectInput(
        "libelle_commune",
        "Commune",
        choices = NULL
      ),
      sliderInput(
        "seuil_flux",
        "Seuil",
        min = 0,
        max = 5000,
        step = 100,
        value = 100
      ),
      checkboxInput(
        "regrouper_non_exprimes",
        "Regrouper non exprimés",
        TRUE
      ),
      hr(),
      downloadButton("download_html", "Télécharger HTML"),
      br(), br(),
      strong(textOutput("nb_flux"))
    ),
    mainPanel(
      h4(textOutput("titre")),
      sankeyNetworkOutput("sankey", height = "800px")
    )
  )
)

#--------------------------------------------------
# SERVER
#--------------------------------------------------

server <- function(input, output, session) {
  # Synchronisation commune
  observeEvent(input$code_departement,
    {
      communes <- ref_communes %>%
        filter(code_departement == input$code_departement) %>%
        pull(libelle_commune)

      updateSelectInput(
        session,
        "libelle_commune",
        choices = communes,
        selected = communes[1]
      )
    },
    ignoreInit = FALSE
  )

  # Extraction flux
  flux <- reactive({
    reports %>%
      filter(
        code_departement == input$code_departement,
        libelle_commune == input$libelle_commune
      ) %>%
      pluck("flux_voix", 1)
  })

  # Sankey
  sankey_obj <- reactive({
    plot_sankey_networkD3(
      flux(),
      palette_nuances = palette_bloc,
      seuil_flux = input$seuil_flux,
      regrouper_non_exprimes = input$regrouper_non_exprimes
    )
  })

  output$sankey <- renderSankeyNetwork({
    req(sankey_obj())
    sankey_obj()
  })

  # Compteur
  output$nb_flux <- renderText({
    df <- flux()

    total <- nrow(df)

    conserves <- df %>%
      filter(voix_flux >= input$seuil_flux) %>%
      nrow()

    paste0(
      "Flux conservés : ",
      format(conserves, big.mark = " "),
      " / ",
      format(total, big.mark = " ")
    )
  })

  # Titre
  output$titre <- renderText({
    paste0(input$libelle_commune, " (", input$code_departement, ")")
  })

  # Download HTML
  output$download_html <- downloadHandler(
    filename = function() {
      paste0("sankey_", input$code_departement, "_", input$libelle_commune, ".html")
    },
    content = function(file) {
      htmlwidgets::saveWidget(
        sankey_obj(),
        file,
        selfcontained = TRUE
      )
    }
  )
}

#--------------------------------------------------
# RUN
#--------------------------------------------------

shinyApp(ui, server)
