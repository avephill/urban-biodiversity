source("app/preprocess.R")


aoi <- area_hexes(state = "California",  county = "Alameda County")
gbif <- get_gbif(aoi)
richness <- gbif |> gbif_richness_fraction(aoi=aoi)


rank = "class"
taxon = "Aves"
species_area = aoi |>
    inner_join(gbif, 'h10') |>
    filter(.data[[rank]] == {taxon}) |>
    count(species, FIPS, geom, name = "counts")

  total = species_area |> distinct(species) |> count() |> pull(n)

  richness = species_area |>
    group_by(FIPS, geom) |>
    summarise(richness = n() / total, 
              counts = sum(counts),
              .groups = "drop")
