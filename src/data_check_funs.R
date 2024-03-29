#' AUTHOR: Cole B. Brookson
#' DATE OF CREATION: 2024-04-29

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

#' Check Data Function
#'
#' This function checks the validity of data in a dataframe that supposedly
#' match the pharos standard.
#'
#' @param df The dataframe to be checked.
#'
#' @return A dataframe containing the problem rows, columns, and errors.
#'
#' @examples
#' df <- data.frame(
#'     host_species = c("Cat", "Dog", "Rabbit"),
#'     ncbi_id = c(12345, 67890, 54321),
#'     detection_outcome = c("Positive", "Negative", "Positive"),
#'     organism_sex = c("Male", "Female", "Unknown"),
#'     dead_or_alive = c("Alive", "Dead", "Alive"),
#'     latitude = c(40.7128, 37.7749, 34.0522),
#'     longitude = c(-74.0060, -122.4194, -118.2437),
#'     year = c(2021, 2022, 2023),
#'     collection_day = c(1, 2, 3),
#'     collection_month = c("January", "February", "March"),
#'     value = c(1.23, 4.56, 7.89)
#' )
#' check_data(df)
#'
#' @export
check_data <- function(df) {
    # Initialize an empty dataframe to store problem rows, columns, and errors
    problem_data <- data.frame(
        row = integer(),
        column = character(),
        error = character(),
        stringsAsFactors = FALSE
    )

    # Iterate over each row in the dataframe
    for (i in 1:nrow(df)) {
        row <- df[i, ]

        # Apply the check functions to each column in the row
        tryCatch(
            {
                check_host_species(row$host_species)
                check_ncbi(row$ncbi_id)
                check_detection_outcome(row$detection_outcome)
                check_organism_sex(row$organism_sex)
                check_dead_or_alive(row$dead_or_alive)
                check_lat(row$latitude)
                check_lon(row$longitude)
                check_date(row$year, list(
                    collection_day = row$collection_day,
                    collection_month = row$collection_month
                ))
                check_float(row$value)
            },
            error = function(e) {
                # If an error occurs, add the row, column, and error
                # to the problem_data dataframe
                problem_data <- rbind(
                    problem_data,
                    data.frame(
                        row = i,
                        column = names(row)[which(!sapply(row, is.null))],
                        error = e$message, stringsAsFactors = FALSE
                    )
                )
            }
        )
    }

    # Return the problem_data dataframe
    return(problem_data)
}
