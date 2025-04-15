source("app/preprocess.R")
aoi <- area_hexes(state = "California",  county = "Alameda County")
gbif <- get_gbif(aoi)


# all gbif columns:
h0 <- get_h0(aoi)
gbif <- paste0(glue("https://{aws_public_endpoint}/public-gbif/hex/h0="), h0, "/part0.parquet") |> open_dataset() 

richness <- gbif |> gbif_richness_fraction(aoi=aoi)

aws_s3_endpoint <- "minio.carlboettiger.info"

latitude = 37.78506
longitude = -122.7277


open_dataset(glue::glue("https://{aws_public_endpoint}/public-social-vulnerability/2022-tracts-h3-z5.parquet")) |>
      filter(h5 == h3_latlng_to_cell_string(latitude, longitude, 5L))  |> 
      collect()
})

gbif  |> group_by(basisofrecord) |> summarise(d = mean(coordinateuncertaintyinmeters), sd = sd(coordinateuncertaintyinmeters)) |> arrange(desc(d))
gbif  |> filter(institutioncode == "iNaturalist") |> group_by(basisofrecord) |> summarise(n = sum(is.na(coordinateuncertaintyinmeters))) |> arrange(desc(n))


gbif  |> filter(institutioncode == "iNaturalist") |> count(basisofrecord)
# Add basisofrecord filter
# HUMAN_OBSERVATION   221744111
# PRESERVED_SPECIMEN    8462730
# MATERIAL_SAMPLE       2039603
# OBSERVATION           2023602
# OCCURRENCE             767165
# MACHINE_OBSERVATION    521594
# FOSSIL_SPECIMEN        419687
# MATERIAL_CITATION       69018
# LIVING_SPECIMEN         15366

institutioncode <- c("all", "CLO", "iNaturalist", "other")

# drop uncertainty over 1 km, but keep those as NA
gbif  |> 
  filter(coordinateuncertaintyinmeters < 1000 || is.na(coordinateuncertaintyinmeters)) |>
  count(coordinateuncertaintyinmeters, sort= TRUE)  |> 
  collect()

paste0(glue("https://{aws_public_endpoint}/public-gbif/taxa.parquet")) |> open_dataset() 


taxa <- open_dataset("~/data/Taxon.tsv")
