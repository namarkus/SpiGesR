test_that("admin only can be read", {
  dirname <- testthat::test_path("testdata", "spiges_admin_only")

  selected_files <- c(admin = "1_Administratives.csv")

  spiges_coltypes = list(
    admin = tibble::tribble(
      ~fileheader  , ~canonical       , ~type       , ~col_spec              , ~anon_fg , ~user_col_fg ,
      'JAHR'       , 'jahr'           , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'ENT_ID'     , 'ent_id'         , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'BURNR'      , 'standort_id'    , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'FALL_ID'    , 'fall_id_spital' , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'GESCHLECHT' , 'geschlecht'     , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'ALTER'      , 'alter'          , 'integer'   , readr::col_double()    , FALSE    , FALSE
    )
  )

  out <- read_spiges_csv_files(dirname, selected_files, spiges_coltypes)

  expect_length(out, 1)
  expect_named(out, c("admin"))
  expect_shape(out$admin, nrow = 3)
  expect_true("fall_id" %in% names(out$admin))
  expect_identical(names(out$admin)[2], "fall_id")
})

test_that("fall_id is mapped from admin, is first column, and key cols are removed", {
  dir <- testthat::test_path("testdata", "spiges_minimal")

  selected_files <- c(
    admin = "1_Administratives.csv",
    diag = '5_Diagnosen.csv',
    proc = '6_Behandlungen.csv'
  )

  spiges_coltypes = list(
    admin = tibble::tribble(
      ~fileheader   , ~canonical       , ~type       , ~col_spec              , ~anon_fg , ~user_col_fg ,
      'JAHR'        , 'jahr'           , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'ENT_ID'      , 'ent_id'         , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'STANDORT_ID' , 'standort_id'    , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'FALL_ID'     , 'fall_id_spital' , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'GESCHLECHT'  , 'geschlecht'     , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'ALTER'       , 'alter'          , 'integer'   , readr::col_double()    , FALSE    , FALSE
    ),
    diag = tibble::tribble(
      ~fileheader           , ~canonical            , ~type       , ~col_spec              , ~anon_fg , ~user_col_fg ,
      'JAHR'                , 'jahr'                , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'ENT_ID'              , 'ent_id'              , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'STANDORT_ID'         , 'standort_id'         , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'FALL_ID'             , 'fall_id_spital'      , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'DIAGNOSE_ID'         , 'diagnose_id'         , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'DIAGNOSE_KODE'       , 'diagnose_kode'       , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'DIAGNOSE_SEITIGKEIT' , 'diagnose_seitigkeit' , 'integer'   , readr::col_double()    , FALSE    , FALSE
    ),
    proc = tibble::tribble(
      ~fileheader             , ~canonical              , ~type       , ~col_spec              , ~anon_fg , ~user_col_fg ,
      'JAHR'                  , 'jahr'                  , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'ENT_ID'                , 'ent_id'                , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'STANDORT_ID'           , 'standort_id'           , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'FALL_ID'               , 'fall_id_spital'        , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'BEHANDLUNG_ID'         , 'behandlung_id'         , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'BEHANDLUNG_CHOP'       , 'behandlung_chop'       , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'BEHANDLUNG_SEITIGKEIT' , 'behandlung_seitigkeit' , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'BEHANDLUNG_BEGINN'     , 'behandlung_beginn'     , 'character' , readr::col_character() , FALSE    , FALSE
    )
  )

  out <- read_spiges_csv_files(dir, selected_files, spiges_coltypes)

  expect_named(out, c("admin", "diag", "proc"))
  expect_true("fall_id" %in% names(out$admin))
  expect_identical(names(out$admin)[2], "fall_id")

  # key_cols <- c("jahr", "ent_id", "standort_id", "fall_id_spital")
  # expect_false(any(key_cols %in% names(out$admin)))
  # expect_false(any(key_cols %in% names(out$diag)))

  expect_false(anyNA(out$diag$fall_id))
})
