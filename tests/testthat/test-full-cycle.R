# Test import and export functions with test data
library(testthat)

# Load test data creation function
source(system.file("R", "test_data.R", package = "SpiGesR"))

test_that("full import-export cycle works for XML format", {
  # Skip test if running on CRAN
  skip_on_cran()
  
  # Create temporary files for testing
  temp_dir <- tempdir()
  data_file <- file.path(temp_dir, "test_data.xml")
  id_file <- file.path(temp_dir, "test_id.xml")
  
  # Create test data
  test_data <- create_test_data()
  
  # Export test data to XML
  expect_true(
    export_spiges_xml(
      test_data, 
      data_file, 
      id_file, 
      validate = FALSE
    )
  )
  
  # Check that files exist
  expect_true(file.exists(data_file))
  expect_true(file.exists(id_file))
  
  # Import the exported XML files
  imported_data <- import_spiges_xml(data_file, id_file)
  
  # Process the imported data
  processed_data <- process_spiges_data(imported_data)
  
  # Check that the imported data has the expected structure
  expect_true(is.list(processed_data))
  expect_true("enterprise" %in% names(processed_data))
  expect_true("cases" %in% names(processed_data))
  expect_true("identifiers" %in% names(processed_data))
  
  # Check that the enterprise ID matches
  expect_equal(
    processed_data$enterprise$ent_id, 
    test_data$enterprise$ent_id
  )
  
  # Clean up
  unlink(data_file)
  unlink(id_file)
})

test_that("full import-export cycle works for SwissDRG format", {
  # Skip test if running on CRAN
  skip_on_cran()
  
  # Create temporary file for testing
  temp_dir <- tempdir()
  swissdrg_file <- file.path(temp_dir, "test_swissdrg.dat")
  
  # Create test data
  test_data <- create_test_data()
  
  # Export test data to SwissDRG format
  expect_true(
    export_swissdrg(
      test_data, 
      swissdrg_file
    )
  )
  
  # Check that file exists
  expect_true(file.exists(swissdrg_file))
  
  # Read the exported file
  swissdrg_content <- readLines(swissdrg_file)
  
  # Check that we have the expected number of lines (one per case)
  expect_equal(length(swissdrg_content), nrow(test_data$cases$administrative))
  
  # Clean up
  unlink(swissdrg_file)
})

test_that("full import-export cycle works for SPLG_TEXT format", {
  # Skip test if running on CRAN
  skip_on_cran()
  
  # Create temporary file for testing
  temp_dir <- tempdir()
  splg_text_file <- file.path(temp_dir, "test_splg.txt")
  
  # Create test data
  test_data <- create_test_data()
  
  # Export test data to SPLG_TEXT format
  expect_true(
    export_splg_text(
      test_data, 
      splg_text_file
    )
  )
  
  # Check that file exists
  expect_true(file.exists(splg_text_file))
  
  # Read the exported file
  splg_text_content <- readLines(splg_text_file)
  
  # Check that we have the expected format
  expect_equal(splg_text_content[1], "SPLG-INPUT")
  
  # Clean up
  unlink(splg_text_file)
})

test_that("full import-export cycle works for SPLG_JSON format", {
  # Skip test if running on CRAN
  skip_on_cran()
  
  # Create temporary file for testing
  temp_dir <- tempdir()
  splg_json_file <- file.path(temp_dir, "test_splg.json")
  
  # Create test data
  test_data <- create_test_data()
  
  # Export test data to SPLG_JSON format
  expect_true(
    export_splg_json(
      test_data, 
      splg_json_file
    )
  )
  
  # Check that file exists
  expect_true(file.exists(splg_json_file))
  
  # Read the exported file
  splg_json_content <- jsonlite::read_json(splg_json_file)
  
  # Check that we have the expected structure
  expect_true("splg-json" %in% names(splg_json_content))
  expect_equal(length(splg_json_content[["splg-json"]]), nrow(test_data$cases$administrative))
  
  # Clean up
  unlink(splg_json_file)
})

test_that("spiges_labelled class works correctly", {
  # Create a test vector
  gender <- spiges_labelled(
    c(1, 2, 1, 9),
    labels_de = c("männlich" = 1, "weiblich" = 2, "unbekannt" = 9),
    labels_fr = c("masculin" = 1, "féminin" = 2, "inconnu" = 9),
    labels_it = c("maschile" = 1, "femminile" = 2, "sconosciuto" = 9),
    name = "gender"
  )
  
  # Test class
  expect_true(inherits(gender, "spiges_labelled"))
  
  # Test attributes
  expect_equal(attr(gender, "labels_de"), c("männlich" = 1, "weiblich" = 2, "unbekannt" = 9))
  expect_equal(attr(gender, "labels_fr"), c("masculin" = 1, "féminin" = 2, "inconnu" = 9))
  expect_equal(attr(gender, "labels_it"), c("maschile" = 1, "femminile" = 2, "sconosciuto" = 9))
  expect_equal(attr(gender, "name"), "gender")
  
  # Test get_labels function
  expect_equal(get_labels(gender, "de"), c("männlich" = 1, "weiblich" = 2, "unbekannt" = 9))
  expect_equal(get_labels(gender, "fr"), c("masculin" = 1, "féminin" = 2, "inconnu" = 9))
  expect_equal(get_labels(gender, "it"), c("maschile" = 1, "femminile" = 2, "sconosciuto" = 9))
})

test_that("process_spiges_data correctly processes data", {
  # Create test data
  test_data <- create_test_data()
  
  # Process the test data
  processed_data <- process_spiges_data(test_data)
  
  # Check that the processed data has the expected structure
  expect_true(is.list(processed_data))
  expect_true("enterprise" %in% names(processed_data))
  expect_true("cases" %in% names(processed_data))
  expect_true("identifiers" %in% names(processed_data))
  
  # Check that the enterprise ID matches
  expect_equal(
    processed_data$enterprise$ent_id, 
    test_data$enterprise$ent_id
  )
})
