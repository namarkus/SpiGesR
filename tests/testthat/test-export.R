# Test export functions
library(testthat)

test_that("export_spiges_xml validates input", {
  # Test with invalid input
  expect_error(export_spiges_xml("not_a_list", "output_data.xml", "output_id.xml"))
})

test_that("export_swissdrg validates input", {
  # Test with invalid input
  expect_error(export_swissdrg("not_a_list", "output.dat"))
  
  # Test with missing required data
  expect_error(export_swissdrg(list(), "output.dat"))
})

test_that("export_splg_text validates input", {
  # Test with invalid input
  expect_error(export_splg_text("not_a_list", "output.txt"))
  
  # Test with missing required data
  expect_error(export_splg_text(list(), "output.txt"))
})

test_that("export_splg_json validates input", {
  # Test with invalid input
  expect_error(export_splg_json("not_a_list", "output.json"))
  
  # Test with missing required data
  expect_error(export_splg_json(list(), "output.json"))
})

# Additional tests would be added for the actual export functionality
# using test data created in the test_data.R file
