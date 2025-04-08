library(ggplot2)
library(dplyr)
library(mapgl)
library(duckdbfs)

source("preprocess.R")

#get_states()
# get_counties("Texas")

aoi <- area_hexes("California", "San FranciscoCounty")
h0 <- get_h0(aoi)
gbif <- get_gbif(aoi)

get_taxa("class", "California", "San Francisco County")

sp <- get_taxa("species", "California", "San Francisco County")
length(sp) # 104,595

classes <- 
# initial default


bench::bench_time({
  gdf <- combined_sf()
})












fill_color = interpolate(column = "richness",
                         values = c(0, 1),
                         stops = c("lightgreen",
                                   "darkgreen"),
                         na_color = "lightgrey")

maplibre(bounds = gdf) |> 
  add_fill_layer(id = "nc_data",
                 source = gdf,
                 fill_color = fill_color, #"blue",
                 fill_opacity = 0.5)
