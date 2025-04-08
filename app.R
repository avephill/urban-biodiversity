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
  titlePanel("Demo App"),
  shinybusy::add_busy_spinner(),
  layout_columns(
    card(maplibreOutput("map")),
    card(includeMarkdown("## Plot"),
         plotOutput("plot")
         ),
    col_widths = c(8, 4),
    row_heights = c("700px"),
    max_height = "900px"
  ),

  sidebar = sidebar(
    open = TRUE, width = 250,
    selectInput("state", "Select state:", get_states(), selected = "California"),
    uiOutput("county_selector"),
    selectInput("rank", "Select taxon rank:", rank, selected = "class"),
    textInput("taxon", "taxonomic name:", "Aves"),
    input_dark_mode(id = "mode"),
    input_switch("svi", "Social Vulnerability", value = TRUE),
  ),
  theme = bs_theme(version = "5")
)

# Define the server
server <- function(input, output, session) {

  # Create a dropdown for counties based on the state selected:
  output$county_selector <- renderUI({
    req(input$state)
    counties <- get_counties(input$state)
    selectInput("county", "Select County:", choices = counties)
  })

  output$map <- renderMaplibre({

    gdf <- combined_sf(input$state, input$county, input$rank, input$taxon)
    maplibre(bounds = gdf) |> 
      add_source(id = "richness_source", gdf) |>
      add_layer("richness_layer",
                type = "fill",
                source = "richness_source",
                tooltip = "richness",
                paint = list(
                  "fill-color" = viridis_pal("richness"),
                  "fill-opacity" = 0.4
                )
      )
  })

  output$plot <- renderPlot(
    {
      combined_sf(input$state, input$county, input$rank, input$taxon) |> 
        as_tibble() |>
        ggplot(aes(x = vulnerability, y = richness, fill = vulnerability)) +
        geom_boxplot(alpha = 0.5) +
        geom_jitter(width = 0.2, alpha = 0.5) + theme_bw(base_size = 18)
    }
  )

  observeEvent(input$county, {
#      gdf <- combined_sf(input$state, input$county, input$rank, input$taxon)
#      maplibre_proxy("map") |> set_source("richness_layer", gdf) |> fit_bounds(gdf)
  })

}


# Run the app
shinyApp(ui = ui, server = server)
