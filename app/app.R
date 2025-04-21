library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(mapgl)
library(duckdbfs)
library(shinybusy)
source("preprocess.R")
source("geolocate.R")


uuid <- "6add767f-6ff7-486e-b4d5-ddce60fb8409"
png <- glue("https://images.phylopic.org/images/{uuid}/raster/508x512.png")

# Define the UI
ui <- page_sidebar(
  fillable = FALSE, # do not squeeze to vertical screen space
  #tags$head(css),
  titlePanel("GBIF Species Richness by Census Tract"),
  shinybusy::add_busy_spinner(),

   layout_columns(
    
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
    value_box("unique species in area", textOutput("total"), 
              showcase = htmltools::tags$img(href = png, width=30, height=30), # fontawesome::fa("kiwi-bird")
    ),
    col_widths = c(8, 4),
  ),



  layout_columns(
    card(maplibreOutput("map")),
    card(plotOutput("plot"), plotOutput("plot2")),
    col_widths = c(8, 4),
    row_heights = c("700px"),
    max_height = "900px"
  ),
  card(
      card_header("Errata"),
      shiny::markdown(readr::read_file("footer.md")),
  ),
  sidebar = sidebar(
    open = TRUE, width = 250,
    selectInput("rank", "Select taxon rank:", rank, selected = "class"),
    textInput("taxon", "taxonomic name:", "Aves"),
    selectInput("map_color", "Map variable:", c("richness", "counts", "vulnerability")),
    selectInput("svi_theme", "Social vulnerability metric:", svi_themes),
    textInput("state", "Selected state", "California"),
    textInput("county", "Selected county", "Alameda County"),

    selectInput("institutioncode", "data provider", providers),
    selectInput("basisofrecord", "observation type", observation_types),

    input_switch("redlines", "Redlined Areas", value = FALSE),

    #selectInput("state", "Selected state:", get_states(), selected = "California"),
    #uiOutput("county_selector"),
    input_dark_mode(id = "mode"),
  ),
  theme = bs_theme(version = "5")
)

# Define the server
server <- function(input, output, session) {

  # Create a dropdown for counties based on the state selected:
  output$county_selector <- renderUI({
    req(input$state)
    counties <- get_counties(input$state)
    selectInput("county", "Selected County:", choices = counties)
  })

  output$total <- renderText({

    gdf <- combined_sf(input$state,
                       input$county,
                       input$rank,
                       input$taxon,
                       input$svi_theme,
                       input$basisofrecord,
                       input$institutioncode)
    gdf$unique_species[1]

  })

  output$map <- renderMaplibre({

    if (input$map_color == "vulnerability"){
      variable <- input$svi_theme
    } else {
      variable <- input$map_color
    }

    gdf <- combined_sf(input$state,
                       input$county,
                       input$rank,
                       input$taxon,
                       input$svi_theme,
                       input$basisofrecord,
                       input$institutioncode)

    if (variable == "counts") {
      map_color_column <- "log_counts"
      gdf <- gdf |> mutate(log_counts = log(counts))
      vmax <- max( max(gdf[[map_color_column]]), 1) # normalize only needed for counts, otherwise 0-1 scale
      vmin <- min(gdf[[map_color_column]]) # normalize only needed for counts, otherwise 0-1 scale
    } else {
      vmin = 0
      vmax = 1
      map_color_column = variable
    }
    m <- maplibre(bounds = gdf, maxZoom = 12) |> 
      add_source(id = "richness_source", gdf) |>
      add_layer("richness_layer",
                type = "fill",
                source = "richness_source",
                tooltip = variable,
                paint = list(
                  "fill-color" = viridis_pal(map_color_column, max_v = vmax),
                  "fill-opacity" = 0.4
                ) 
      ) |> 
      add_geolocate_control() |> 
      add_geocoder_control()

    if (input$redlines) {
      m <- m |>
        add_fill_layer(
          id = "redlines",
          source = list(type = "vector",
                        url = paste0("pmtiles://", "https://data.source.coop/cboettig/us-boundaries/mappinginequality.pmtiles")),
          source_layer = "mappinginequality",
          fill_opacity = 0.6,
          fill_color = list("get", "fill")
        ) |> add_layers_control()
    }

  m
  })

  # Attempt to pull state/county from geocoder search
  init <- reactiveValues(loaded = FALSE)
  observe(
    if (init$loaded) {
      result <- input$map_geocoder$result

      if (!is.null(result)) {
        res <- extract_county_state(input$map_geocoder$result)
        if(length(res$county) > 0) {
          updateTextInput(session, "state", value = res$state)
          updateTextInput(session, "county", value = res$county)
          print(glue::glue("selected {res$state}, {res$county}"))
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
                        input$svi_theme,
                        input$basisofrecord,
                        input$institutioncode) |>
        as_tibble() |>
        na.omit() |>
        mutate(vulnerability =
                 cut(.data[[input$svi_theme]], 
                     breaks = c(0, .25, .50, .75, 1), 
                     labels = c("Q1-Lowest", "Q2-Low", "Q3-Medium", "Q4-High")
                 )
              )

      df |>
        ggplot(aes(x = vulnerability, y = richness, fill = vulnerability)) +
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
  output$plot2 <- renderPlot(
    {
      df <- combined_sf(input$state,
                        input$county,
                        input$rank,
                        input$taxon,
                        input$svi_theme,
                        input$basisofrecord,
                        input$institutioncode) |> 
        as_tibble() |>
        na.omit() |>
        mutate(vulnerability = 
                 cut(.data[[input$svi_theme]], 
                     breaks = c(0, .25, .50, .75, 1), 
                     labels = c("Q1-Lowest", "Q2-Low", "Q3-Medium", "Q4-High")
                 )
              )

      df |>
        ggplot(aes(x = vulnerability, y = counts, fill = vulnerability)) +
        geom_boxplot(alpha = 0.5) +
        geom_jitter(width = 0.2, alpha = 0.5) + 
        scale_y_log10() +
        theme_bw(base_size = 18) +
        theme(legend.position = "none") +
        labs(y = "sampling effort (counts)", x= "vulnerability",
             title =  "",
             caption = "")

      
  })

#  observeEvent(input$county, {
#      gdf <- combined_sf(input$state, input$county, input$rank, input$taxon)
#      maplibre_proxy("map") |> set_source("richness_layer", gdf) |> fit_bounds(gdf)
#  })

}


# Run the app
shinyApp(ui = ui, server = server)
