library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(mapgl)
library(duckdbfs)


css <-
  HTML(paste0("<link rel='stylesheet' type='text/css' ",
              "href='https://demos.creative-tim.com/",
              "material-dashboard/assets/css/",
              "material-dashboard.min.css?v=3.2.0'>"))


# Define the UI
ui <- page_sidebar(
  fillable = FALSE, # do not squeeze to vertical screen space
  tags$head(css),
  titlePanel("Demo App"),

  # chat_box("which tracts have the highest vulnerability"),
  # textOutput("agent"),

  layout_columns(
    card(maplibreOutput("map")),
    card(includeMarkdown("## Plot"),
         plotOutput("plot")
         ),
    col_widths = c(8, 4),
    row_heights = c("500px"),
    max_height = "600px"
  ),

  sidebar = sidebar(
    open = FALSE, width = 150,
    input_switch("svi", "Social Vulnerability", value = TRUE),
    theme = bs_theme(version = "5")
  )
)

source("preprocess.R")


# Define the server
server <- function(input, output, session) {

  gdf = combined |> to_sf(crs = "EPSG:4326")


  output$map <- renderMaplibre({

    m <- maplibre(center = c(-122.5, 37.8), zoom = 9) |>
      add_source(id = "birds", gdf) |>
      add_layer("birds-layer",
                type = "fill",
                source = "birds",
                tooltip = "richness",
                paint = list(
                  "fill-color" = interpolate(column = "richness",
                                            values = c(0, 1),
                                            stops = c("lightgreen",
                                                      "darkgreen"),
                                            na_color = "lightgrey"),
                  "fill-opacity" = 0.4
                )
      )
  m
  })

  print(head(gdf))

  output$plot <- renderPlot(
    {
      gdf |> as_tibble() |>
      ggplot(aes(x = svi_bin, y = richness, fill = svi_bin)) +
      geom_boxplot(alpha = 0.5) +
      geom_jitter(width = 0.2, alpha = 0.5) + theme_bw(base_size = 18)
    }
  )
}


# Run the app
shinyApp(ui = ui, server = server)
