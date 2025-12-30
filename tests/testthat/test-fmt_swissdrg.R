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

test_that("fmt_swissdrg_diag works", {
  diag <- tibble(
    fall_id = c(111L, 112L, 113L, 114L, 114L),
    diagnose_id = c(1L, 1L, 1L, 1L, 2L),
    diagnose_kode = c('I269', 'I269', 'I269', 'I269', 'E1190'),
    diagnose_zusatz = c('', NA_character_, 'E1190', '', '')
  )

  expect_identical(
    fmt_swissdrg_diag(diag)$diagnoses,
    c(
      'I269',
      'I269',
      'I269|E1190',
      'I269|E1190'
    )
  )
})

test_that("fmt_swissdrg_proc works", {
  proc <- tibble(
    fall_id = c(111L, 112L, 113L, 114L, 115L, 115L, 115L),
    behandlung_id = c(1L, 1L, 1L, 1L, 1L, 2L, 3L),
    behandlung_chop = c(
      '5432',
      '5432',
      '5432',
      NA_character_,
      '992502',
      '874199',
      '921122'
    ),
    behandlung_seitigkeit = c(
      NA_integer_,
      2L,
      0L,
      NA_integer_,
      NA_integer_,
      1L,
      NA_integer_
    ),
    behandlung_beginn = c(
      NA_character_,
      NA_character_,
      '20090325',
      NA_character_,
      '',
      '',
      '20221103'
    )
  )

  expect_identical(
    fmt_swissdrg_proc(proc)$procedures,
    c(
      '5432::',
      '5432:L:',
      '5432:B:20090325',
      '992502::|874199:R:|921122::20221103'
    )
  )
})

test_that("fmt_swissdrg_medi works", {
  medi <- tibble(
    fall_id = c(111L, 111L, 112L, 112L),
    medi_id = c(1L, 2L, 1L, 2L),
    medi_atc = c('L01XC07', 'B02BD02', 'L04AA04', 'J02AC04'),
    medi_zusatz = c(NA_character_, 'Plas', 'CFR', 'Susp'),
    medi_verabreichungsart = c('IV', 'IV', 'IV', 'O'),
    medi_dosis = c('450.0', '1500', '200', '500'),
    medi_einheit = c('mg', 'U', 'mg', 'mg')
  )

  expect_identical(
    fmt_swissdrg_medi(medi)$medications,
    c(
      'L01XC07::IV:450.0:mg|B02BD02:Plas:IV:1500:U',
      'L04AA04:CFR:IV:200:mg|J02AC04:Susp:O:500:mg'
    )
  )
})
