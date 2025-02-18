

chat_box <- function(ex_query = "write a question here.") {
    card(
        layout_columns(
        textInput("chat",
            label = NULL,
            ex_query,
            width = "100%"),
        div(
        actionButton("user_msg", "", icon = icon("paper-plane"),
                    class = "btn-primary btn-sm align-bottom"),
        class = "align-text-bottom"),
        col_widths = c(11, 1)),
        fill = FALSE
    )
}

chat_explain <- function() {
  card(fill = TRUE,
    card_header(fa("robot"),  textOutput("model", inline = TRUE)),
    accordion(
      open = FALSE,
      accordion_panel(
        title = "show sql",
        icon = fa("terminal"),
        verbatimTextOutput("sql_code"),
      ),
      accordion_panel(
        title = "explain",
        icon = fa("user", prefer_type="solid"),
        textOutput("explanation"),
      )
    ),
  )
}


safe_parse <- function(txt) {
  gsub("[\r\n]", " ", txt) |> gsub("\\s+", " ", x = _)
}


# helper utilities
# faster/more scalable to pass maplibre the ids to refilter pmtiles,
# than to pass it the full geospatial/sf object
filter_column <- function(full_data, filtered_data, id_col = "FIPS") {
  if (nrow(filtered_data) < 1) return(NULL)
  values <- full_data |>
    inner_join(filtered_data, copy = TRUE) |>
    pull(id_col)
  # maplibre syntax for the filter of PMTiles
  list("in", list("get", id_col), list("literal", values))
}
