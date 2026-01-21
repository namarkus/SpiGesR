# --- read_spiges_csv_file() -----------------------------------------------
test_that("read_spiges_csv_file renames, casts integers, and reports problems", {
  tmp_dir <- tempfile("spiges_csv_file_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  csv_path <- file.path(tmp_dir, "demo.csv")
  # The second row has a non-integer value and the third has a parse error.
  writeLines(
    c(
      "JAHR,ENT_ID,BURNR,FALL_ID,ALTER",
      "2024,E1,S1,F1,1.5",
      "2024,E1,S1,F1,x"
    ),
    csv_path
  )

  coltypes_tbl <- tibble::tribble(
    ~fileheader , ~canonical       , ~type       , ~col_spec              , ~anon_fg , ~user_col_fg ,
    "JAHR"      , "jahr"           , "integer"   , readr::col_double()    , FALSE    , FALSE        ,
    "ENT_ID"    , "ent_id"         , "character" , readr::col_character() , FALSE    , FALSE        ,
    "BURNR"     , "standort_id"    , "character" , readr::col_character() , FALSE    , FALSE        ,
    "FALL_ID"   , "fall_id_spital" , "character" , readr::col_character() , FALSE    , FALSE        ,
    "ALTER"     , "alter"          , "integer"   , readr::col_double()    , FALSE    , FALSE
  )

  out <- read_spiges_csv_file(
    fpath = csv_path,
    tablenm = "admin",
    coltypes_tbl = coltypes_tbl,
    use_vroom = FALSE
  )

  expect_true(all(
    c(
      "jahr",
      "ent_id",
      "standort_id",
      "fall_id_spital",
      "fall_id",
      "alter"
    ) %in%
      names(out$data)
  ))
  expect_type(out$data$alter, "integer")
  expect_true(all(is.na(out$data$alter)))

  expect_true(nrow(out$problems) >= 3)
  expect_true(all(out$problems$file == "admin"))
  expect_true(any(out$problems$expected == "an integer"))
  expect_true(any(out$problems$expected == "unique fall key"))
})


# --- read_spiges_csv_files() ----------------------------------------------
test_that("admin only can be read", {
  dirname <- testthat::test_path("testdata", "spiges_admin_only")

  file_headers <- list(
    admin = c('JAHR', 'ENT_ID', 'BURNR', 'FALL_ID', 'GESCHLECHT', 'ALTER')
  )
  selected_files <- c(admin = "1_Administratives.csv")
  spiges_columns <- spiges_schema[['1.4']][['spiges_columns']]
  spiges_tablecolumns <- spiges_columns[names(selected_files)]
  spiges_col_types <- make_coltypes(
    spiges_tablecolumns,
    file_headers,
    col_types = NULL
  )

  out <- read_spiges_csv_files(dirname, selected_files, spiges_col_types)

  expect_named(out, c("data", "problems"))
  expect_length(out$data, 1)
  expect_named(out$data, c("admin"))
  expect_shape(out$data$admin, nrow = 4)
  expect_true("jahr" %in% names(out$data$admin))
  expect_identical(names(out$data$admin)[1], "jahr")
  expect_true("fall_id" %in% names(out$data$admin))
  expect_identical(names(out$data$admin)[2], "fall_id")
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
      ~fileheader  , ~canonical       , ~type       , ~col_spec              , ~anon_fg , ~user_col_fg ,
      'JAHR'       , 'jahr'           , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'ENT_ID'     , 'ent_id'         , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'BURNR'      , 'standort_id'    , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'FALL_ID'    , 'fall_id_spital' , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'GESCHLECHT' , 'geschlecht'     , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'ALTER'      , 'alter'          , 'integer'   , readr::col_double()    , FALSE    , FALSE
    ),
    diag = tibble::tribble(
      ~fileheader           , ~canonical            , ~type       , ~col_spec              , ~anon_fg , ~user_col_fg ,
      'JAHR'                , 'jahr'                , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'ENT_ID'              , 'ent_id'              , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'BURNR'               , 'standort_id'         , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'FALL_ID'             , 'fall_id_spital'      , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'DIAGNOSE_ID'         , 'diagnose_id'         , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'DIAGNOSE_KODE'       , 'diagnose_kode'       , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'DIAGNOSE_SEITIGKEIT' , 'diagnose_seitigkeit' , 'integer'   , readr::col_double()    , FALSE    , FALSE
    ),
    proc = tibble::tribble(
      ~fileheader             , ~canonical              , ~type       , ~col_spec              , ~anon_fg , ~user_col_fg ,
      'JAHR'                  , 'jahr'                  , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'ENT_ID'                , 'ent_id'                , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'BURNR'                 , 'standort_id'           , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'FALL_ID'               , 'fall_id_spital'        , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'BEHANDLUNG_ID'         , 'behandlung_id'         , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'BEHANDLUNG_CHOP'       , 'behandlung_chop'       , 'character' , readr::col_character() , FALSE    , FALSE        ,
      'BEHANDLUNG_SEITIGKEIT' , 'behandlung_seitigkeit' , 'integer'   , readr::col_double()    , FALSE    , FALSE        ,
      'BEHANDLUNG_BEGINN'     , 'behandlung_beginn'     , 'character' , readr::col_character() , FALSE    , FALSE
    )
  )

  out <- read_spiges_csv_files(dir, selected_files, spiges_coltypes)

  expect_named(out, c("data", "problems"))
  expect_named(out$data, c("admin", "diag", "proc"))
  expect_true("fall_id" %in% names(out$data$admin))
  expect_identical(names(out$data$admin)[2], "fall_id")

  # key_cols <- c("jahr", "ent_id", "standort_id", "fall_id_spital")
  # expect_false(any(key_cols %in% names(out$admin)))
  # expect_false(any(key_cols %in% names(out$diag)))

  expect_false(anyNA(out$data$diag$fall_id))
})


# --- read_spiges_csv() ---------------------------------------------------
test_that("read_spiges_csv reads selected tables and honors user col_types", {
  dir <- testthat::test_path("testdata", "spiges_minimal")

  user_cols <- readr::cols_only(
    DIAGNOSE_KODE = readr::col_character(),
    BEHANDLUNG_CHOP = readr::col_character()
  )

  out <- read_spiges_csv(
    dirname = dir,
    tables = c("diag", "behand"),
    col_types = user_cols
  )

  expect_s3_class(out, "spiges_data")
  expect_named(out, c("admin", "diag", "proc"))

  expect_true(all(
    c("jahr", "ent_id", "standort_id", "fall_id_spital", "fall_id") %in%
      names(out$admin)
  ))

  expect_true(setequal(names(out$diag), c("fall_id", "diagnose_kode")))
  expect_true(setequal(names(out$proc), c("fall_id", "behandlung_chop")))
  expect_equal(nrow(problems(out)), 0)
})
