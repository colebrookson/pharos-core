#' AUTHOR: Cole B. Brookson
#' DATE OF CREATION: 2024-03-29

# pull the fish data
pull_data(
    data_loc = paste0(
        "https://github.com/sjpeacock/Sea-lice-database/blob/master/",
        "Data/BroughtonSeaLice_fishData.csv"
    ),
    out_loc = here::here("./data/scfs-sea-lice/fish-data.csv")
)

# pull the site data
pull_data(
    data_loc = paste0(
        "https://github.com/sjpeacock/Sea-lice-database/blob/master/",
        "Data/BroughtonSeaLice_siteData.csv"
    ),
    out_loc = here::here("./data/scfs-sea-lice/site-data.csv")
)
