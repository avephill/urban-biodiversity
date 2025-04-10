source("app/preprocess.R")


aoi <- area_hexes(state = "California",  county = "Alameda County")
gbif <- get_gbif(aoi)
richness <- gbif |> gbif_richness_fraction(aoi=aoi)

aws_s3_endpoint <- "minio.carlboettiger.info"

latitude = 37.78506
longitude = -122.7277


open_dataset(glue::glue("https://{aws_public_endpoint}/public-social-vulnerability/2022-tracts-h3-z5.parquet")) |>
      filter(h5 == h3_latlng_to_cell_string(latitude, longitude, 5L))  |> 
      collect()
})

