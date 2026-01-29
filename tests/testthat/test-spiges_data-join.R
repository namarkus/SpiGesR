test_that("left_join.spiges_data joins admin and preserves attributes", {
  spiges_data <- list(
    admin = data.frame(fall_id = c("A", "B"), a = 1:2),
    diag = data.frame(fall_id = c("A", "A"), dx = c("x", "y"))
  )
  class(spiges_data) <- c("spiges_data", class(spiges_data))
  attr(spiges_data, "meta") <- list(Datenjahr = 2025L)
  attr(spiges_data, "problems") <- tibble::tibble()

  out <- left_join(spiges_data, diag, by = "fall_id")

  expect_s3_class(out, "spiges_data")
  expect_true("dx" %in% names(out$admin))
  expect_identical(attr(out, "meta"), list(Datenjahr = 2025L))
  expect_true(inherits(attr(out, "problems"), "tbl_df"))
})

test_that("left_join_tab supports .into and keeps original table", {
  spiges_data <- list(
    admin = data.frame(fall_id = c("A", "B"), a = 1:2),
    diag = data.frame(fall_id = c("A", "B"), dx = c("x", "y"))
  )
  class(spiges_data) <- c("spiges_data", class(spiges_data))
  attr(spiges_data, "meta") <- list(Datenjahr = 2025L)

  out <- left_join_tab(spiges_data, admin, diag, .into = joined, by = "fall_id")

  expect_s3_class(out, "spiges_data")
  expect_true(all(c("admin", "diag", "joined") %in% names(out)))
  expect_true("dx" %in% names(out$joined))
})

test_that("n:m join emits a warning", {
  spiges_data <- list(
    admin = data.frame(fall_id = c("A", "A"), a = 1:2),
    diag = data.frame(fall_id = c("A", "A"), dx = c("x", "y"))
  )
  class(spiges_data) <- c("spiges_data", class(spiges_data))

  expect_warning(
    left_join_tab(spiges_data, admin, diag, by = "fall_id"),
    "Potential n:m join detected"
  )
})
