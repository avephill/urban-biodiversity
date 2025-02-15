## Illustrate/test core app functionality without shiny

library(dplyr)
library(duckdbfs)

library(mapgl)
library(glue)

duckdbfs::load_h3()
duckdbfs::load_spatial()

sf = open_dataset("https://minio.carlboettiger.info/public-census/year=2022/tracts-hex-z10.parquet") |>
  filter(STATE == "California", COUNTY == "San Francisco County") |>
  mutate(h10 = toupper(unnest(h10)),
         h0 = h3_cell_to_parent(h10, 0L)) |>
  mutate(h0 = upper(as.character(h0)))


h0 = sf |> distinct(h0) |> pull(h0)
tiles = paste0("https://minio.carlboettiger.info/public-gbif/hex/h0=", h0, "/part0.parquet")
gbif = open_dataset(tiles) |> select(class, species, h10)


sf_birds = sf |>
  inner_join(gbif, 'h10') |>
  filter(class == "Aves") |>
  select(c('species', 'FIPS', 'geom')) |>
  distinct()

total = sf_birds |> distinct(species) |> count() |> pull(n)

gdf = sf_birds |>
  count(FIPS, geom) |>
  mutate(score = n / {total}) |>
  to_sf(crs = "EPSG:4326")


m <- maplibre(center = c(-122.5, 37.8), zoom = 9) |>
  add_source(id = "birds", gdf) |>
  add_layer("birds-layer",
            type = "fill",
            source = "birds",
            tooltip = "n",
            paint = list(
              "fill-color" = interpolate(column = "score",
                                         values = c(0, 1),
                                         stops = c("lightgreen", "darkgreen"),
                                         na_color = "lightgrey"),
              "fill-opacity" = 0.4
            )

    )

library(htmlwidgets)
htmlwidgets::saveWidget(m, "example.html")
