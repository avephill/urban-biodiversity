library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(mapgl)
library(duckdbfs)
library(shinybusy)
source("preprocess.R")


# Define the UI
ui <- page_sidebar(
  fillable = FALSE, # do not squeeze to vertical screen space
  #tags$head(css),
  titlePanel("GBIF Species Richness by Census Tract"),
  shinybusy::add_busy_spinner(),
  includeMarkdown(
    "Use the search bar in the map to look for a city or other region.
    The app will plot species richness for the taxa selected by census
    tract as a fraction of the total species richness (for the same
    taxonomic group) observed in the county as a whole.  Mouse
    over the map individual richness scores, scroll to zoom, drag to
    pan, ctrl+click to alter angle.
    In the side panel, the app will also plot social vulnerability
    indicators by quantile vs the species
    richness observed."),

  layout_columns(
    card(maplibreOutput("map")),
    card(plotOutput("plot")),
    col_widths = c(8, 4),
    row_heights = c("700px"),
    max_height = "900px"
  ),

  sidebar = sidebar(
    open = TRUE, width = 250,
    selectInput("rank", "Select taxon rank:", rank, selected = "class"),
    textInput("taxon", "taxonomic name:", "Aves"),
    selectInput("svi_theme", "Social vulnerability metric:", svi_themes),
    textInput("state", "Selected state", "California"),
    textInput("county", "Selected county", "Alameda County"),

    #selectInput("state", "Selected state:", get_states(), selected = "California"),
    #uiOutput("county_selector"),
    input_dark_mode(id = "mode"),
  ),
  theme = bs_theme(version = "5")
)

# Define the server
server <- function(input, output, session) {

 
  observe(
    print(paste("theme: ", svi_label(input$svi_theme)))
  )


  init <- reactiveValues(loaded = FALSE)
  # Create a dropdown for counties based on the state selected:
  output$county_selector <- renderUI({
    req(input$state)
    counties <- get_counties(input$state)
    selectInput("county", "Selected County:", choices = counties)
  })

  output$map <- renderMaplibre({

    gdf <- combined_sf(input$state, input$county, input$rank, input$taxon)
    m <- maplibre(bounds = gdf, maxZoom = 12) |> 
      add_source(id = "richness_source", gdf) |>
      add_layer("richness_layer",
                type = "fill",
                source = "richness_source",
                tooltip = "richness",
                paint = list(
                  "fill-color" = viridis_pal("richness"),
                  "fill-opacity" = 0.4
                ) 
      ) |> 
      add_geolocate_control() |> 
      add_geocoder_control()


  m
  })

  observe( 
    if (init$loaded) {
      result <- input$map_geocoder$result
      if (!is.null(result)) {
        if (!is.null(result$features[[1]]$properties$address$county)) {
          county <- result$features[[1]]$properties$address$county
          state <- result$features[[1]]$properties$address$state
          updateTextInput(session, "state", value = state)
          updateTextInput(session, "county", value = county)
          print(glue::glue("{state}, {county}"))
        }
      }
    } else  {
      init$loaded <- TRUE
    }
  )

  output$plot <- renderPlot(
    {
      df <- combined_sf(input$state,
                        input$county,
                        input$rank,
                        input$taxon,
                        input$svi_theme) |> 
        as_tibble() |> 
        mutate(vulnerability = cut(.data[[input$svi_theme]], breaks = c(0, .25, .50, .75, 1), 
               labels = c("Q1-Lowest", "Q2-Low", "Q3-Medium", "Q4-High"))) 
        ggplot(df, aes(x = vulnerability, y = richness, fill = vulnerability)) +
        geom_boxplot(alpha = 0.5) +
        geom_jitter(width = 0.2, alpha = 0.5) + theme_bw(base_size = 18) + 
        theme(legend.position = "none") +
        labs(y = "species richness", x= "vulnerability",
             title =  svi_label(input$svi_theme),
             caption = "Species richness by 2022 Census Tract
                        as a fraction of species richness 
                        observed in the county as a whole")

      
    }
  )

  observeEvent(input$county, {
#      gdf <- combined_sf(input$state, input$county, input$rank, input$taxon)
#      maplibre_proxy("map") |> set_source("richness_layer", gdf) |> fit_bounds(gdf)
  })

}


# Run the app
shinyApp(ui = ui, server = server)
