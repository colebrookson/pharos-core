#' AUTHOR: Cole B. Brookson
#' DATE OF CREATION: 2024-04-29

#' Pull data from web
#'
#' @description Get the data file from a location on the internet via URL,
#' download to hidden folder
#' @param data_loc character -- where to look for the data
#' @param out_loc character -- where to save the cleaned file
#'
#' @examples
#' pull_data("https://github.com/sjpeacock/Sea-lice-database.csv")
#'
#' @export
pull_data <- function(data_loc, out_loc) {
    download.file(
        data_loc,
        destfile = out_loc,
        extra = "-r -p --random-wait"
    )
}
