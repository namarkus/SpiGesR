# Test import functions
library(testthat)

test_that("spiges_labelled creates multilingual labelled vectors", {
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

test_that("import_spiges_xml validates input files", {
  # Test with non-existent files
  expect_error(import_spiges_xml("non_existent_file.xml", "non_existent_file.xml"))
})

test_that("import_spiges_csv validates input directory", {
  # Test with non-existent directory
  expect_error(import_spiges_csv("non_existent_directory"))
})

# Additional tests would be added for the actual import functionality
# using the example files in inst/extdata
