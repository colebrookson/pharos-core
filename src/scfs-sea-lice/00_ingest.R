#' AUTHOR: Cole B. Brookson
#' DATE OF CREATION: 2024-03-29

# pull the fish data
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
