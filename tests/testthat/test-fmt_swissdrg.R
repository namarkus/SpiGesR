test_that("fmt_swissdrg_admin admin formats correctly", {
  admin <- tibble::tibble(
    fall_id = 111L,
    alter = 45L,
    alter_U1 = 0L,
    geschlecht = 1L,
    eintrittsdatum = "2025010113",
    eintritt_aufenthalt = 1L,
    eintrittsart = 1L,
    austrittsdatum = "2025010509",
    austrittsentscheid = 1L,
    austritt_aufenthalt = 1L,
    beatmung = 0L
  )

  res <- fmt_swissdrg_admin(admin)
  expect_equal(unique(sapply(res[-1], class)), 'character')

  expect_equal(
    paste(as.character(res[1, -1]), collapse = ';'),
    "111;45;;M;20250101;01;20250105;00;0"
  )
})

test_that("fmt_swissdrg_admin age formats correctly", {
  admin <- tibble::tibble(
    fall_id = c(111L, 112L, 113L, 114L, 115L, 116L, 117L, 118L, 119L),
    alter = c(45L, 45L, 45L, NA_integer_, 0L, 45L, 45L, NA_integer_, 125L),
    alter_U1 = c(NA_integer_, 0, 1L, 1L, 1L, 1L, 1L, 0L, NA_integer_),
    geschlecht = 1L,
    eintrittsdatum = "2025010113",
    eintritt_aufenthalt = 1L,
    eintrittsart = 1L,
    austrittsdatum = "2025010509",
    austrittsentscheid = 1L,
    austritt_aufenthalt = 1L,
    beatmung = NA_real_
  )

  res <- fmt_swissdrg_admin(admin)

  expect_equal(res$age[1:3], c('45', '45', '45'))
  expect_equal(res$age_days[4:6], c('1', '1', ''))
  expect_equal(res$age[7], "45")
  expect_equal(res$age_days[7], "")
  expect_equal(res$age_days[8], "1")
  expect_equal(res$age[9], "124")
})

test_that("fmt_swissdrg_admin admission type formats correctly", {
  admin <- tibble::tibble(
    fall_id = 111:114,
    alter = 30L,
    alter_U1 = NA_integer_,
    geschlecht = 1L,
    eintrittsdatum = "2025031513",
    eintrittsart = c(5L),
    eintritt_aufenthalt = c(6L),
    austrittsdatum = "2025032009",
    austrittsentscheid = c(2L),
    austritt_aufenthalt = c(6L),
    beatmung = 0L
  )

  res <- fmt_swissdrg_admin(admin)

  expect_equal(res$adm_mode[1], "06") # eintritt_aufenthalt == 6L & eintrittsart == 5L
  expect_equal(res$exit_mode[1], "06") # austrittsentscheid != 5L & austritt_aufenthalt == 6L
})

test_that("fmt_swissdrg_admin dates format correctly", {
  admin <- tibble::tibble(
    fall_id = 111L,
    alter = 30L,
    alter_U1 = NA_integer_,
    geschlecht = 1L,
    eintrittsdatum = NA_character_,
    eintrittsart = c(5L),
    eintritt_aufenthalt = c(6L),
    austrittsdatum = NA_character_,
    austrittsentscheid = c(2L),
    austritt_aufenthalt = c(6L),
    beatmung = 0L
  )

  res <- fmt_swissdrg_admin(admin)
  expect_equal(res$adm_date[1], '')
  expect_equal(res$exit_date[1], '')

  admin$eintrittsdatum = '2025031513'
  admin$austrittsdatum = '2025032009'
  res <- fmt_swissdrg_admin(admin)
  expect_equal(res$adm_date[1], '20250315')
  expect_equal(res$exit_date[1], '20250320')

  admin$eintrittsdatum = as.Date('2025-03-15')
  admin$austrittsdatum = as.Date('2025-03-20')
  res <- fmt_swissdrg_admin(admin)
  expect_equal(res$adm_date[1], '20250315')
  expect_equal(res$exit_date[1], '20250320')

  admin$eintrittsdatum = as.POSIXct('2025-03-15 13:00:00')
  admin$austrittsdatum = as.POSIXct('2025-03-20 09:00:00')
  res <- fmt_swissdrg_admin(admin)
  expect_equal(res$adm_date[1], '20250315')
  expect_equal(res$exit_date[1], '20250320')
})


test_that("fmt_swissdrg_babydata checks vars", {
  admin <- tibble::tibble(fall_id = 1L, aufnahmegewicht = 1L)
  neugeb <- tibble::tibble(fall_id = 1L, geburtsgewicht = 1L)
  expect_error(fmt_swissdrg_babydata(admin, neugeb))
})

test_that("fmt_swissdrg_babydata works", {
  admin <- dplyr::tibble(
    fall_id = 1:4,
    aufnahmegewicht = c(1L, NA_integer_, 1L, NA_integer_),
  )
  neugeb <- dplyr::tibble(
    fall_id = 1:4,
    gestationsalter2 = c(NA_integer_, 2L, 2L, NA_integer_)
  )

  expect_identical(
    fmt_swissdrg_babydata(admin, neugeb)$baby_data,
    c('1|', '|2', '1|2', '')
  )
})

test_that("fmt_swissdrg_diag works", {
  diag <- dplyr::tibble(
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
  proc <- dplyr::tibble(
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
  medi <- tibble::tribble(
    ~fall_id , ~medi_id , ~medi_atc , ~medi_zusatz  , ~medi_verabreichungsart , ~medi_dosis , ~medi_einheit ,
    111L     , 1L       , 'L01XC07' , NA_character_ , 'IV'                    , '450.0'     , 'mg'          ,
    111L     , 2L       , 'B02BD02' , 'Plas'        , 'IV'                    , '1500'      , 'U'           ,
    112L     , 1L       , 'L04AA04' , 'CFR'         , 'IV'                    , '200'       , 'mg'          ,
    112L     , 2L       , 'J02AC04' , 'Susp'        , 'O'                     , '500'       , 'mg'          ,
    113L     , 1L       , 'J0AC3'   , 'Susp'        , 'O'                     , '500'       , 'mg' # not in result nchar(medi_atc < 7)
  )

  expect_identical(
    fmt_swissdrg_medi(medi)$medications,
    c(
      'L01XC07::IV:450.0:mg|B02BD02:Plas:IV:1500:U',
      'L04AA04:CFR:IV:200:mg|J02AC04:Susp:O:500:mg'
    )
  )
})
