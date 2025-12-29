test_that("fmt_swissdrg_babydata checks vars", {
  admin <- tibble(fall_id = 1L, aufnahmegewicht = 1L)
  neugeb <- tibble(fall_id = 1L, gestationsalter1 = 1L, gestationsalter2 = 1L)
  expect_warning(fmt_swissdrg_babydata(admin, neugeb))

  admin <- tibble(fall_id = 1L)
  neugeb <- tibble(
    fall_id = 1L,
    geburtsgewicht = 1L,
    gestationsalter1 = 1L,
    gestationsalter2 = 1L
  )
  expect_warning(fmt_swissdrg_babydata(admin, neugeb))

  admin <- tibble(fall_id = 1L, aufnahmegewicht = 1L)
  neugeb <- tibble(fall_id = 1L, geburtsgewicht = 1L, gestationsalter2 = 1L)
  expect_warning(fmt_swissdrg_babydata(admin, neugeb))

  admin <- tibble(fall_id = 1L, aufnahmegewicht = 1L)
  neugeb <- tibble(fall_id = 1L, geburtsgewicht = 1L, gestationsalter2 = 1L)
  expect_warning(fmt_swissdrg_babydata(admin, neugeb))

  admin <- tibble(fall_id = 1L)
  neugeb <- tibble(fall_id = 1L, gestationsalter1 = 1L, gestationsalter2 = 1L)
  expect_error(fmt_swissdrg_babydata(admin, neugeb))

  admin <- tibble(fall_id = 1L, aufnahmegewicht = 1L)
  neugeb <- tibble(fall_id = 1L, geburtsgewicht = 1L)
  expect_erro(fmt_swissdrg_babydata(admin, neugeb))
})

test_that("fmt_swissdrg_babydata works", {
  admin <- tibble(
    fall_id = 1:4,
    aufnahem = c(1L, NA_integer_, 1L, NA_integer_),
    ssw = c(NA_integer_, 2L, 2L, NA_integer_)
  )

  expect_identical(
    fmt_swissdrg_babydata(admin, neugeb)$babydata,
    c('1|', '|2', '|2', '')
  )
})
