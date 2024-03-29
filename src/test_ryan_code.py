library(testthat)

# User class tests
test_that("User class tests", {
  user <- list(
    researcherID = "123",
    organization = "Org",
    email = "user@example.com",
    name = "John Doe",
    projectIDs = NULL,
    firstName = "John",
    lastName = "Doe",
    downloadIDs = NULL
  )
  
  expect_equal(user$researcherID, "123")
  expect_equal(user$organization, "Org")
  expect_equal(user$email, "user@example.com")
  expect_equal(user$name, "John Doe")
  expect_equal(user$projectIDs, NULL)
  expect_equal(user$firstName, "John")
  expect_equal(user$lastName, "Doe")
  expect_equal(user$downloadIDs, NULL)
})

# Project class tests
test_that("Project class tests", {
  project <- list(
    projectID = "456",
    name = "Project",
    datasetIDs = list("dataset1", "dataset2"),
    deletedDatasetIDs = NULL,
    lastUpdated = NULL,
    description = NULL,
    projectType = NULL,
    surveillanceStatus = NULL,
    citation = NULL,
    relatedMaterials = NULL,
    projectPublications = NULL,
    othersCiting = NULL,
    authors = NULL,
    publishStatus = "Unpublished"
  )
  
  expect_equal(project$projectID, "456")
  expect_equal(project$name, "Project")
  expect_equal(project$datasetIDs, list("dataset1", "dataset2"))
  expect_equal(project$deletedDatasetIDs, NULL)
  expect_equal(project$lastUpdated, NULL)
  expect_equal(project$description, NULL)
  expect_equal(project$projectType, NULL)
  expect_equal(project$surveillanceStatus, NULL)
  expect_equal(project$citation, NULL)
  expect_equal(project$relatedMaterials, NULL)
  expect_equal(project$projectPublications, NULL)
  expect_equal(project$othersCiting, NULL)
  expect_equal(project$authors, NULL)
  expect_equal(project$publishStatus, "Unpublished")
})

# Dataset class tests
test_that("Dataset class tests", {
  dataset <- list(
    projectID = "789",
    datasetID = "dataset1",
    name = "Dataset",
    lastUpdated = NULL,
    earliestDate = NULL,
    latestDate = NULL,
    releaseStatus = NULL,
    releaseReport = NULL,
    registerPages = NULL,
    age = NULL,
    mass = NULL,
    length = NULL
  )
  
  expect_equal(dataset$projectID, "789")
  expect_equal(dataset$datasetID, "dataset1")
  expect_equal(dataset$name, "Dataset")
  expect_equal(dataset$lastUpdated, NULL)
  expect_equal(dataset$earliestDate, NULL)
  expect_equal(dataset$latestDate, NULL)
  expect_equal(dataset$releaseStatus, NULL)
  expect_equal(dataset$releaseReport, NULL)
  expect_equal(dataset$registerPages, NULL)
  expect_equal(dataset$age, NULL)
  expect_equal(dataset$mass, NULL)
  expect_equal(dataset$length, NULL)
})