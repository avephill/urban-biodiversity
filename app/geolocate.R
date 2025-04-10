library(duckdbfs)
library(glue)
library(dplyr)

point_to_county <- function(longitude, latitude) {
  duckdbfs::open_dataset(glue::glue("https://{aws_public_endpoint}/public-social-vulnerability/2022-tracts-h3-z8.parquet")) |>
    dplyr::filter(h8 == h3_latlng_to_cell_string(latitude, longitude, 8L)) |> 
    dplyr::collect()
}

extract_county_state <- function(result) {

   

        feature <- result$features[[1]]

        # San Francisco geo coordinates fall outside of the county!
        if(grepl("San Francisco", feature$text)){
          county <- "San Francisco County"
          state <- "California"

        # Some return types have state/county info
        } else if (!is.null(feature$properties$address$county)) {
          county <- feature$properties$address$county
          state <- feature$properties$address$state


        # Other types of features don't, we extract a point
        # and 'manually' resolve (by h3geo)
        } else if("geometry" %in% names(feature)) {
          point <- feature$geometry$coordinates
          loc <- point_to_county(point[[1]], point[[2]])

          state <- loc$STATE
          county <- loc$COUNTY

        } else if("center" %in% names(feature)) {

          point <- feature$center
          loc <- point_to_county(point[[1]], point[[2]])
          state <- loc$STATE
          county <- loc$COUNTY

        } else {
          warning("County not found, please search again")
          state <- "California"
          county <- "Alameda"
        }
      
    return(list(state = state, county = county))
}

