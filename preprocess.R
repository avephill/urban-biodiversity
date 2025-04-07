library(dplyr)
library(duckdbfs)
library(ggplot2)
library(mapgl)
library(glue)
library(memoise)


css <- shiny::HTML(paste0("<link rel='stylesheet' type='text/css' href='https://demos.creative-tim.com/material-dashboard/assets/css/material-dashboard.min.css?v=3.2.0'>"))
rank <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")


duckdbfs::load_h3()
duckdbfs::load_spatial()


aws_public_endpoint <- "minio.carlboettiger.info"
aws_s3_endpoint <- "minio.carlboettiger.info"


get_counties <- memoise(function(state = "California") {
  us_counties |> filter(STATE == {state}) |> arrange(COUNTY) |> head(1000) |> pull(COUNTY)
}#, cache = cache_filesystem(tempfile())
)


# use to populate selctors for state and county
census_areas <- open_dataset(glue("https://{aws_public_endpoint}/public-social-vulnerability/2022/SVI2022_US_county.parquet")) 
us_counties <- census_areas |> distinct(STATE, COUNTY)
states <- census_areas |> distinct(STATE) |> pull(STATE)


fill_color = interpolate(column = "richness",
                        values = c(0, 1),
                        stops = c("lightgreen",
                                  "darkgreen"),
                        na_color = "lightgrey")

# This has FIPS, h10, STATE, COUNTY, geom, year, currently public-social-vulnerability hex tracts dont
# tracts <- open_dataset(glue("https://{aws_public_endpoint}/public-census/year=2022/tracts-hex-z10.parquet"))


area_hexes <- function(state, county) {

  tracts <- open_dataset(glue("https://{aws_public_endpoint}/public-census/year=2022/tracts-hex-z10.parquet"))
  county_hexes <- tracts |>
    filter(STATE == {state}, COUNTY == {county}) |>
    mutate(h10 = toupper(unnest(h10)))
}


gbif_area <- function(aoi) {

  # determine h0 parent cells of this area:
  h0 <- aoi |> mutate(h0 = h3_cell_to_parent(h10, 0L)) |> distinct(h0) |> pull(h0) |> toupper()

  tiles <- paste0(glue("https://{aws_public_endpoint}/public-gbif/hex/h0="), h0, "/part0.parquet")

  gbif <- open_dataset(tiles) |> select(class, species, h10)

  gbif
}


gbif_richness_fraction <- function(gbif, aoi, rank = "class", taxon = "Aves") {

  species_area = aoi |>
    inner_join(gbif, 'h10') |>
    filter(.data[[rank]] == {taxon}) |>
    select(c('species', 'FIPS', 'geom')) |>
    distinct()

  total = species_area |> distinct(species) |> count() |> pull(n)

  richness = species_area |>
    count(FIPS, geom) |>
    mutate(richness = n / {total})

  richness

}

library(memoise)

compute_data_ <- function(state = "California",
                         county = "San Francisco County",
                         rank = "class",
                         taxon = "Aves") {

  aoi <- area_hexes(state, county)
  gbif <- gbif_area(aoi)
  richness <- gbif |> gbif_richness_fraction(aoi, rank, taxon)

  # Access SVI
  svi <- open_dataset(glue::glue("https://{aws_public_endpoint}/public-social-vulnerability/2022/SVI2022_US_tract.parquet")) |>
         select("RPL_THEMES", "FIPS", "COUNTY") |> 
         mutate(RPL_THEMES = ifelse(RPL_THEMES < 0, NA, RPL_THEMES))

  # Access CalEnviroScreen
  ces <- open_dataset(glue("https://{aws_public_endpoint}/public-calenviroscreen/ces_2021.parquet"), format="parquet")
  ces_subset <- ces |> select("Poverty", "FIPS")

  # Filter GBIF to our area-of-interest (h-index) and species of interest

  combined <- richness  |>
    inner_join(svi, "FIPS")  |>
    inner_join(ces_subset, "FIPS") |>
    mutate(vulnerability = cut(RPL_THEMES, breaks = c(0, .25, .50, .75, 1), 
                            labels = c("Q1-Lowest", "Q2-Low", "Q3-Medium", "Q4-High"))) |>
    mutate(poverty_bin = cut(Poverty, breaks = c(0, 25, 50, 75, 100), 
                            labels = c("0-25", "25-50", "50-75", "75-100")))

  combined

}

compute_data <- memoise(compute_data_)


combined_sf <- memoise(
  function(state = "California",
           county = "San Francisco County",
           rank = "class",
           taxon = "Aves")
  {
    combined <- compute_data(state, county, rank, taxon)
    combined |> to_sf(crs = "EPSG:4326")                                   
  },
  cache = cache_filesystem(tempfile())
)
