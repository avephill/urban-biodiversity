library(dplyr)
library(duckdbfs)
library(ggplot2)
library(mapgl)
library(glue)

duckdbfs::load_h3()
duckdbfs::load_spatial()

# Access US Census Tracts with FIPS codes, geoms, and H3 index (zoom 10)
sf = open_dataset("https://minio.carlboettiger.info/public-census/year=2022/tracts-hex-z10.parquet") |>
  filter(STATE == "California", COUNTY == "San Francisco County") |>
  mutate(h10 = toupper(unnest(h10)),
         h0 = h3_cell_to_parent(h10, 0L)) |>
  mutate(h0 = upper(as.character(h0)))

# Access GBIF data from the corresponding h0 tiles
h0 = sf |> distinct(h0) |> pull(h0)
tiles = paste0("https://minio.carlboettiger.info/public-gbif/hex/h0=", h0, "/part0.parquet")
gbif = open_dataset(tiles) |> select(class, species, h10)

# Access SVI
svi = open_dataset("https://minio.carlboettiger.info/public-social-vulnerability/2022/SVI2022_US_tract.parquet")

# Access CalEnviroScreen
ces = open_dataset("https://minio.carlboettiger.info/public-calenviroscreen/ces_2021.parquet", format="parquet")

# Filter GBIF to our area-of-interest (h-index) and species of interest
sf_birds = sf |>
  inner_join(gbif, 'h10') |>
  filter(class == "Aves") |>
  select(c('species', 'FIPS', 'geom')) |>
  distinct()

total = sf_birds |> distinct(species) |> count() |> pull(n)

bird_counts = sf_birds |>
  count(FIPS, geom) |>
  mutate(richness = n / {total})

ces_poverty = ces |> select("Poverty", "FIPS")
combined <- svi |>
  select("RPL_THEMES", "FIPS") |> filter(RPL_THEMES > 0) |>
  inner_join(bird_counts, "FIPS")  |>
  inner_join(ces_poverty, "FIPS") |>
  mutate(svi_bin = cut(RPL_THEMES, breaks = c(0, .25, .50, .75, 1), 
                           labels = c("Q1", "Q2", "Q3", "Q4"))) |>
  mutate(poverty_bin = cut(Poverty, breaks = c(0, 25, 50, 75, 100), 
                           labels = c("0-25", "25-50", "50-75", "75-100")))
 
  
