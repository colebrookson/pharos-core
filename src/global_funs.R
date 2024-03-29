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

#' Check if the host species is allowed
#'
#' @param host_species character - The host species to check
#'
#' @return character - The host species if allowed
#' @export
check_host_species <- function(host_species) {
    disallowed_species <- c("homo sapiens", "homo sapien", "human")
    if (tolower(host_species) %in% disallowed_species) {
        stop("Please do not upload data on human infections to Pharos.")
    }
    return(host_species)
}

#' Check if the NCBI identifier is valid
#'
#' @param ncbi_id numeric - The NCBI identifier to check
#'
#' @return numeric - The NCBI identifier if valid
#' @export
check_ncbi <- function(ncbi_id) {
    if (!is.numeric(ncbi_id) || nchar(as.character(ncbi_id)) < 1 ||
        nchar(as.character(ncbi_id)) > 7) {
        stop("A NCBI taxonomic identifier consists of one to seven digits.")
    }
    return(ncbi_id)
}

#' Check if the detection outcome is valid
#'
#' @param detection_outcome character - The detection outcome to check
#'
#' @return character - The detection outcome if valid
#' @export
check_detection_outcome <- function(detection_outcome) {
    allowed_outcomes <- c("positive", "negative", "inconclusive")
    if (!(tolower(detection_outcome) %in% allowed_outcomes)) {
        stop("Detection outcome must be an unambiguous value such as
        'positive', 'negative', or 'inconclusive'.")
    }
    return(detection_outcome)
}

#' Check if the organism sex is valid
#'
#' @param organism_sex character - The organism sex to check
#'
#' @return character - The organism sex if valid
#' @export
check_organism_sex <- function(organism_sex) {
    allowed_sexes <- c("male", "female", "unknown")
    if (!(tolower(organism_sex) %in% allowed_sexes)) {
        stop("Organism sex must be an unambiguous value such as male,
         female, or unknown.")
    }
    return(organism_sex)
}

#' Check if the dead or alive status is valid
#'
#' @param dead_or_alive character - The dead or alive status to check
#'
#' @return character - The dead or alive status if valid
#' @export
check_dead_or_alive <- function(dead_or_alive) {
    allowed_statuses <- c("dead", "alive", "unknown")
    if (!(tolower(dead_or_alive) %in% allowed_statuses)) {
        stop("Dead or alive must be an unambiguous value such as dead,
        alive, or unknown.")
    }
    return(dead_or_alive)
}

#' Check if the latitude is within valid range
#'
#' @param latitude numeric - The latitude to check
#'
#' @return numeric - The latitude if valid
#' @export
check_lat <- function(latitude) {
    if (!is.numeric(latitude) || latitude < -90 || latitude > 90) {
        stop("Latitude must be between -90 and 90.")
    }
    return(latitude)
}

#' Check if the longitude is within valid range
#'
#' @param longitude numeric - The longitude to check
#'
#' @return numeric - The longitude if valid
#' @export
check_lon <- function(longitude) {
    if (!is.numeric(longitude) || longitude < -180 || longitude > 180) {
        stop("Longitude must be between -180 and 180.")
    }
    return(longitude)
}

#' Check if the date is valid
#'
#' @param year numeric - The year of the date
#' @param values list - A list containing the collection day and month
#'
#' @return numeric - The year if valid
#' @export
check_date <- function(year, values) {
    day <- values$collection_day
    month <- values$collection_month

    # Don't do any validation until all three are filled out
    if (is.null(day) || is.null(month)) {
        return(year)
    }

    if (nchar(as.character(year)) < 4) {
        stop("Year must be a four-digit year")
    }

    tryCatch(
        {
            date <- as.Date(paste(year, month, day, sep = "-"))
            message(paste(
                "Date", format(date, "%Y-%m-%d"),
                "is ready to release"
            ))
        },
        error = function(e) {
            stop(paste(
                "Date", paste(year, month, day, sep = "-"),
                "is invalid,", e$message
            ))
        }
    )

    return(year)
}

#' Check if the value is a valid float
#'
#' @param value numeric - The value to check
#'
#' @return numeric - The value if valid
#' @export
check_float <- function(value) {
    if (!is.numeric(value)) {
        stop("Must be a number, units can be configured in
        dataset settings (coming soon).")
    }
    return(value)
}

context("Record Validators")

testthat::test_that("check_host_species returns correct result", {
    testthat::expect_equal(check_host_species("Canis lupus"), "Canis lupus")
    testthat::expect_error(
        check_host_species("Homo sapiens"),
        "Please do not upload data on human infections to Pharos."
    )
})

testthat::test_that("check_ncbi returns correct result", {
    testthat::expect_equal(check_ncbi(12345), 12345)
    testthat::expect_error(
        check_ncbi("ABC"),
        "A NCBI taxonomic identifier consists of one to seven digits."
    )
})

testthat::test_that("check_detection_outcome returns correct result", {
    testthat::expect_equal(check_detection_outcome("positive"), "positive")
    testthat::expect_error(
        check_detection_outcome("uncertain"),
        "Detection outcome must be an unambiguous value such as 'positive',
    'negative', or 'inconclusive'."
    )
})

testthat::test_that("check_organism_sex returns correct result", {
    testthat::expect_equal(check_organism_sex("male"), "male")
    testthat::expect_error(
        check_organism_sex("unknown"),
        "Organism sex must be an unambiguous value such as male, female,
    or unknown."
    )
})

testthat::test_that("check_dead_or_alive returns correct result", {
    testthat::expect_equal(check_dead_or_alive("alive"), "alive")
    testthat::expect_error(
        check_dead_or_alive("unknown"),
        "Dead or alive must be an unambiguous value such as dead, alive,
     or unknown."
    )
})

testthat::test_that("check_lat returns correct result", {
    testthat::expect_equal(check_lat(45.678), 45.678)
    testthat::expect_error(
        check_lat(100),
        "Latitude must be between -90 and 90."
    )
})

testthat::test_that("check_lon returns correct result", {
    testthat::expect_equal(check_lon(-123.456), -123.456)
    testthat::expect_error(
        check_lon(200),
        "Longitude must be between -180 and 180."
    )
})

testthat::test_that("check_date returns correct result", {
    values <- list(collection_day = 15, collection_month = 6)
    testthat::expect_equal(check_date(2022, values), 2022)

    values <- list(collection_day = 31, collection_month = 2)
    testthat::expect_error(
        check_date(2022, values),
        "Date 2022-2-31 is invalid, invalid 'time' argument"
    )
})

testthat::test_that("check_float returns correct result", {
    testthat::expect_equal(check_float(3.14), 3.14)
    testthat::expect_error(
        check_float("ABC"),
        "Must be a number, units can be configured in
    dataset settings (coming soon)."
    )
})
