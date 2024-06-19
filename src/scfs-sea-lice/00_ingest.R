#' AUTHOR: Cole B. Brookson
#' DATE OF CREATION: 2024-03-29

library(magrittr)

# pull the fish data & site data
fish_data <- pull_data(
    data_loc = paste0(
        "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/",
        "Data/BroughtonSeaLice_fishData.csv"
    ),
    out_loc = here::here("./data/scfs-sea-lice/fish-data.csv"),
    env_load = TRUE
)

# pull the site data
site_data <- pull_data(
    data_loc = paste0(
        "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/",
        "Data/BroughtonSeaLice_siteData.csv"
    ),
    out_loc = here::here("./data/scfs-sea-lice/site-data.csv"),
    env_load = TRUE
)

# join the datasets into one
scfs_data <- dplyr::left_join(
    x = fish_data,
    y = site_data,
    by = "site_id",
    suffix = c("", ".annoying_duplicate_column")
) %>%
    dplyr::select(-dplyr::ends_with(".annoying_duplicate_column"))
readr::write_csv(
    data.frame(
        scfs_names = names(scfs_data)
    ),
    here::here("./data/scfs-sea-lice/scfs-scfs-names-match-sheet.csv")
)
# create relationship table for all Pharos fields
pharos_df <- readr::read_csv(here::here("./data/dataset-template.csv"))
names(pharos_df)
match_df <- data.frame(
    pharos_names = c(
        "Sample ID", "Animal ID", "Host species", "Host species NCBI tax ID",
        "Latitude", "Longitude", "Spatial uncertainty", "Collection day",
        "Collection month", "Collection year",
        "Collection method or tissue", "Detection method", "Primer sequence",
        "Primer citation", "Detection target", "Detection target NCBI tax ID",
        "Detection outcome", "Detection measurement", "Detection measurement units",
        "Pathogen", "Pathogen NCBI tax ID", "GenBank accession", "Detection comments",
        "Organism sex", "Dead or alive", "Health notes", "Life stage", "Age",
        "Mass", "Length"
    )
)
readr::write_csv(
    match_df,
    here::here("./data/scfs-sea-lice/scfs-pharos-names-match-sheet.csv")
)
