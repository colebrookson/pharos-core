#' AUTHOR: Cole B. Brookson
#' DATE OF CREATION: 2024-04-29

#' Pull data from web
#'
#' @description Get the csv data file from a location on the internet via URL,
#' download to hidden folder
#'
#' @param data_loc character -- where to look for the data
#' @param out_loc character -- where to save the cleaned file
#' @param env_load boolean -- should the file then be loaded into the working
#' environment?
#'
#' @return dataframe -- only if (env_load == TRUE)
#'
#' @examples
#' pull_data("https://github.com/sjpeacock/Sea-lice-database.csv")
#'
#' @export
pull_data <- function(data_loc, out_loc, env_load = FALSE) {
    download.file(
        url = data_loc,
        destfile = out_loc,
        method = "auto",
        mode = "wb"
    )
    if (env_load) {
        return(
            readr::read_csv(out_loc, guess_max = 100000)
        )
    }
}
