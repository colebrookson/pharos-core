#' AUTHOR: Cole B. Brookson
#' DATE OF CREATION: 2024-03-29

library(magrittr)

cluster_data <- readxl::read_xlsx(
    here::here("./data/dfo-pathogens/bass-etal-2023.xlsx"),
    sheet = "all cluster data"
)

# also read in by species
coho_data <- readxl::read_xlsx(
    here::here("./data/dfo-pathogens/bass-etal-2023.xlsx"),
    sheet = "coho"
)
sockeye_data <- readxl::read_xlsx(
    here::here("./data/dfo-pathogens/bass-etal-2023.xlsx"),
    sheet = "sockeye"
)
chinook_data <- readxl::read_xlsx(
    here::here("./data/dfo-pathogens/bass-etal-2023.xlsx"),
    sheet = "chinook"
)
names(coho_data)
