testthat::context("Record Validators")

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

# Test case 1: Valid data
test_that("Valid data returns an empty dataframe", {
    df <- data.frame(
        host_species = c("Cat", "Dog", "Rabbit"),
        ncbi_id = c(12345, 67890, 54321),
        detection_outcome = c("Positive", "Negative", "Positive"),
        organism_sex = c("Male", "Female", "Unknown"),
        dead_or_alive = c("Alive", "Dead", "Alive"),
        latitude = c(40.7128, 37.7749, 34.0522),
        longitude = c(-74.0060, -122.4194, -118.2437),
        year = c(2021, 2022, 2023),
        collection_day = c(1, 2, 3),
        collection_month = c("January", "February", "March"),
        value = c(1.23, 4.56, 7.89)
    )

    result <- check_data(df)

    expect_equal(nrow(result), 0)
})

# Test case 2: Invalid host_species
test_that("Invalid host_species returns the correct error", {
    df <- data.frame(
        host_species = c("Cat", "Dog", "Invalid"),
        ncbi_id = c(12345, 67890, 54321),
        detection_outcome = c("Positive", "Negative", "Positive"),
        organism_sex = c("Male", "Female", "Unknown"),
        dead_or_alive = c("Alive", "Dead", "Alive"),
        latitude = c(40.7128, 37.7749, 34.0522),
        longitude = c(-74.0060, -122.4194, -118.2437),
        year = c(2021, 2022, 2023),
        collection_day = c(1, 2, 3),
        collection_month = c("January", "February", "March"),
        value = c(1.23, 4.56, 7.89)
    )

    result <- check_data(df)

    expect_equal(nrow(result), 1)
    expect_equal(result$error[1], 
    "Please do not upload data on human infections to Pharos.")
})

# Test case 3: Invalid ncbi_id
test_that("Invalid ncbi_id returns the correct error", {
    df <- data.frame(
        host_species = c("Cat", "Dog", "Rabbit"),
        ncbi_id = c(12345, "Invalid", 54321),
        detection_outcome = c("Positive", "Negative", "Positive"),
        organism_sex = c("Male", "Female", "Unknown"),
        dead_or_alive = c("Alive", "Dead", "Alive"),
        latitude = c(40.7128, 37.7749, 34.0522),
        longitude = c(-74.0060, -122.4194, -118.2437),
        year = c(2021, 2022, 2023),
        collection_day = c(1, 2, 3),
        collection_month = c("January", "February", "March"),
        value = c(1.23, 4.56, 7.89)
    )

    result <- check_data(df)

    expect_equal(nrow(result), 1)
    expect_equal(result$error[1], 
    "A NCBI taxonomic identifier consists of one to seven digits.")
})

# Test case 4: Invalid detection_outcome
test_that("Invalid detection_outcome returns the correct error", {
    df <- data.frame(
        host_species = c("Cat", "Dog", "Rabbit"),
        ncbi_id = c(12345, 67890, 54321),
        detection_outcome = c("Positive", "Invalid", "Positive"),
        organism_sex = c("Male", "Female", "Unknown"),
        dead_or_alive = c("Alive", "Dead", "Alive"),
        latitude = c(40.7128, 37.7749, 34.0522),
        longitude = c(-74.0060, -122.4194, -118.2437),
        year = c(2021, 2022, 2023),
        collection_day = c(1, 2, 3),
        collection_month = c("January", "February", "March"),
        value = c(1.23, 4.56, 7.89)
    )

    result <- check_data(df)

    expect_equal(nrow(result), 1)
    expect_equal(result$error[1], 
    "Detection outcome must be an unambiguous value such as 'positive', 
    'negative', or 'inconclusive'.")
})

# Test case 5: Invalid organism_sex
test_that("Invalid organism_sex returns the correct error", {
    df <- data.frame(
        host_species = c("Cat", "Dog", "Rabbit"),
        ncbi_id = c(12345, 67890, 54321),
        detection_outcome = c("Positive", "Negative", "Positive"),
        organism_sex = c("Male", "Invalid", "Unknown"),
        dead_or_alive = c("Alive", "Dead", "Alive"),
        latitude = c(40.7128, 37.7749, 34.0522),
        longitude = c(-74.0060, -122.4194, -118.2437),
        year = c(2021, 2022, 2023),
        collection_day = c(1, 2, 3),
        collection_month = c("January", "February", "March"),
        value = c(1.23, 4.56, 7.89)
    )

    result <- check_data(df)

    expect_equal(nrow(result), 1)
    expect_equal(result$error[1], 
    "Organism sex must be an unambiguous value such as male, 
    female, or unknown.")
})

# Test case 6: Invalid dead_or_alive
test_that("Invalid dead_or_alive returns the correct error", {
    df <- data.frame(
        host_species = c("Cat", "Dog", "Rabbit"),
        ncbi_id = c(12345, 67890, 54321),
        detection_outcome = c("Positive", "Negative", "Positive"),
        organism_sex = c("Male", "Female", "Unknown"),
        dead_or_alive = c("Alive", "Invalid", "Unknown"),
        latitude = c(40.7128, 37.7749, 34.0522),
        longitude = c(-74.0060, -122.4194, -118.2437),
        year = c(2021, 2022, 2023),
        collection_day = c(1, 2, 3),
        collection_month = c("January", "February", "March"),
        value = c(1.23, 4.56, 7.89)
    )

    result <- check_data(df)

    expect_equal(nrow(result), 1)
    expect_equal(result$error[1], 
    "Dead or alive must be an unambiguous value such as dead, 
    alive, or unknown.")
})

# Test case 7: Invalid latitude
test_that("Invalid latitude returns the correct error", {
    df <- data.frame(
        host_species = c("Cat", "Dog", "Rabbit"),
        ncbi_id = c(12345, 67890, 54321),
        detection_outcome = c("Positive", "Negative", "Positive"),
        organism_sex = c("Male", "Female", "Unknown"),
        dead_or_alive = c("Alive", "Dead", "Alive"),
        latitude = c(40.7128, 100, 34.0522),
        longitude = c(-74.0060, -122.4194, -118.2437),
        year = c(2021, 2022, 2023),
        collection_day = c(1, 2, 3),
        collection_month = c("January", "February", "March"),
        value = c(1.23, 4.56, 7.89)
    )

    result <- check_data(df)

    expect_equal(nrow(result), 1)
    expect_equal(result$error[1], "Latitude must be between -90 and 90.")
})

# Test case 8: Invalid longitude
test_that("Invalid longitude returns the correct error", {
    df <- data.frame(
        host_species = c("Cat", "Dog", "Rabbit"),
        ncbi_id = c(12345, 67890, 54321),
        detection_outcome = c("Positive", "Negative", "Positive"),
        organism_sex = c("Male", "Female", "Unknown"),
        dead_or_alive = c("Alive", "Dead", "Alive"),
        latitude = c(40.7128, 37.7749, 34.0522),
        longitude = c(-74.0060, -200, -118.2437),
        year = c(2021, 2022, 2023),
        collection_day = c(1, 2, 3),
        collection_month = c("January", "February", "March"),
        value = c(1.23, 4.56, 7.89)
    )

    result <- check_data(df)

    expect_equal(nrow(result), 1)
    expect_equal(result$error[1], "Longitude must be between -180 and 180.")
})

# Test case 9: Invalid year
test_that("Invalid year returns the correct error", {
    df <- data.frame(
        host_species = c("Cat", "Dog", "Rabbit"),
        ncbi_id = c(12345, 67890, 54321),
        detection_outcome = c("Positive", "Negative", "Positive"),
        organism_sex = c("Male", "Female", "Unknown"),
        dead_or_alive = c("Alive", "Dead", "Alive"),
        latitude = c(40.7128, 37.7749, 34.0522),
        longitude = c(-74.0060, -122.4194, -118.2437),
        year = c(21, 2022, 2023),
        collection_day = c(1, 2, 3),
        collection_month = c("January", "February", "March"),
        value = c(1.23, 4.56, 7.89)
    )

    result <- check_data(df)

    expect_equal(nrow(result), 1)
    expect_equal(result$error[1], "Year must be a four-digit year")
})

# Test case 10: Invalid value
test_that("Invalid value returns the correct error", {
    df <- data.frame(
        host_species = c("Cat", "Dog", "Rabbit"),
        ncbi_id = c(12345, 67890, 54321),
        detection_outcome = c("Positive", "Negative", "Positive"),
        organism_sex = c("Male", "Female", "Unknown"),
        dead_or_alive = c("Alive", "Dead", "Alive"),
        latitude = c(40.7128, 37.7749, 34.0522),
        longitude = c(-74.0060, -122.4194, -118.2437),
        year = c(2021, 2022, 2023),
        collection_day = c(1, 2, 3),
        collection_month = c("January", "February", "March"),
        value = c("Invalid", 4.56, 7.89)
    )

    result <- check_data(df)

    expect_equal(nrow(result), 1)
    expect_equal(result$error[1], 
    "Must be a number, units can be configured in 
    dataset settings (coming soon).")
})
