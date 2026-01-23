#' Read BFS hospital statistics (SpiGes) from a directory
#'
#' @param dirname Directory containing the CSV files.
#' @param tables Character vector of table names to read (fuzzy). If \code{NULL} (default), all available tables in the directory
#'   are read.
#' @param col_types Optional \code{readr::cols_only()} object that is used as
#'   \code{col_types} for all tables (overrides table-specific col_types).
#' @param version = '1.4' SpiGes Version.
#'
#' @return A list with the requested tables (always containing \code{admin}),
#'   with attributes \code{Datenjahr}, \code{Datenversion}, \code{Source}.
#'
#' @export
read_spiges_csv <- function(
  dirname,
  tables = NULL,
  col_types = NULL,
  version = '1.4'
) {
  # --- Basic checks -----------------------------------------------------
  ## check dirname
  if (!dir.exists(dirname)) {
    stop("Directory ", dirname, " does not exist.", call. = FALSE)
  }

  ## check tables
  if (!missing(tables) & !inherits(tables, 'character')) {
    stop("If not missing `tables` must be a character vector.", call. = FALSE)
  }

  ## check col_types
  if (!is.null(col_types)) {
    if (!inherits(col_types, "col_spec")) {
      stop(
        "If provided, `col_types` must be a readr::cols() or readr::cols_only() object.",
        call. = FALSE
      )
    }
  }

  ## check col_types
  if (!version %in% names(spiges_schema)) {
    stop(
      "SpiGes version, `",
      version,
      "` not found. Possible versions are: ",
      paste(names(spiges_schema), collapse = ', '),
      ". Otherwise, contact package author."
    )
  }
  spiges_columns <- spiges_schema[[version]][['spiges_columns']]
  spiges_filenames <- spiges_schema[[version]][['spiges_tables']]

  # get files in directory (once)
  dir_files <- dir(dirname, pattern = '^\\d{1,2}\\_\\w+\\.csv$')
  if (length(dir_files) == 0L) {
    stop(
      "Directory ",
      dirname,
      " does not contain any expected files.",
      call. = FALSE
    )
  }

  # Metadata for version info
  Datenversion <- file.info(dirname)$mtime

  available_files <- spiges_filenames[spiges_filenames %in% dir_files]
  if (length(available_files) == 0L) {
    stop(
      "No known SpiGes files found in directory ",
      dirname,
      ". ",
      call. = FALSE
    )
  }

  # Admin must be available, otherwise the structure cannot be built
  if (!"1_Administratives.csv" %in% available_files) {
    stop(
      "No `Administratives` file found in directory ",
      dirname,
      ". This table is required as the central base table.",
      call. = FALSE
    )
  }

  # --- Handle the `tables` argument (fuzzy, but constrained to available) ----
  if (is.null(tables)) {
    # If tables is NULL, read all available tables
    selected_files <- available_files
  } else {
    tables <- tolower(tables)

    matched <- available_files[Reduce(
      `|`,
      lapply(tables, function(x) grepl(x, available_files, ignore.case = TRUE))
    )]

    if (length(matched) == 0L) {
      stop(
        "None of the entries in `tables` match the available tables in the directory. ",
        "Available tables are: ",
        paste(available_tables, collapse = ", "),
        call. = FALSE
      )
    }

    selected_files <- matched
  }

  # Ensure `admin` is always included when any other table is selected
  if (!"1_Administratives.csv" %in% selected_files) {
    selected_files <- c(admin = "1_Administratives.csv", selected_files)
    warning(
      "Table `1_Administratives.csv` was automatically added to `tables` because other tables ",
      "were requested and `1_Administratives.csv` is the central base table.",
      call. = FALSE
    )
  }

  # --- get headers per table --------------------------------------
  file_headers <- sapply(selected_files, function(x) {
    readr::read_lines(file.path(dirname, x), n_max = 1)
  })
  file_headers <- strsplit(gsub('"', '', file_headers), ',')

  # --- prepare col_types per table --------------------------------------
  spiges_tablecolumns <- spiges_columns[names(selected_files)]

  spiges_col_types <- make_coltypes(
    spiges_tablecolumns,
    file_headers,
    col_types
  )

  # --- check if key cols are present in all selected tables -------------
  missing_key_by_table <- lapply(names(spiges_col_types), function(tablenm) {
    cols_tbl <- spiges_col_types[[tablenm]]
    missing_key_rows <- dplyr::filter(cols_tbl, key == 1L, is.na(fileheader))
    if (nrow(missing_key_rows) == 0L) {
      return(character(0))
    }
    missing_key_rows$canonical
  })
  names(missing_key_by_table) <- names(spiges_col_types)

  admin_missing_keys <- missing_key_by_table[["admin"]]
  if (!is.null(admin_missing_keys) && length(admin_missing_keys) > 0L) {
    warning(
      "Required key columns missing in admin table: ",
      paste(admin_missing_keys, collapse = ", "),
      ",  only table `admin` will be read.",
      call. = FALSE
    )
    selected_files <- selected_files["admin"]
    spiges_col_types <- spiges_col_types["admin"]
  }

  tables_missing_keys <- names(missing_key_by_table)[
    vapply(missing_key_by_table, length, integer(1)) > 0L
  ]
  tables_missing_keys <- setdiff(tables_missing_keys, "admin")
  if (length(tables_missing_keys) > 0L) {
    missing_detail <- vapply(
      tables_missing_keys,
      function(tablenm) {
        missing_cols <- missing_key_by_table[[tablenm]]
        paste0(tablenm, " [", paste(missing_cols, collapse = ", "), "]")
      },
      character(1)
    )
    warning(
      "Skipping tables with missing key columns: ",
      paste(missing_detail, collapse = "; "),
      call. = FALSE
    )
    selected_files <- selected_files[
      !names(selected_files) %in% tables_missing_keys
    ]
    spiges_col_types <- spiges_col_types[
      !names(spiges_col_types) %in% tables_missing_keys
    ]
  }

  # --- check if not only key cols are marked to read -------------
  only_keys_by_table <- sapply(names(spiges_col_types), function(tablenm) {
    cols_tbl <- spiges_col_types[[tablenm]]
    return(all(cols_tbl$key == 1L))
  })
  names(only_keys_by_table) <- names(spiges_col_types)

  only_keys_tables <- names(only_keys_by_table)[
    only_keys_by_table & !names(only_keys_by_table) == 'admin'
  ]
  if (length(only_keys_tables) > 0L) {
    warning(
      "Skipping tables with only key columns: ",
      paste(only_keys_tables, collapse = "; "),
      call. = FALSE
    )
    selected_files <- selected_files[
      !names(selected_files) %in% only_keys_tables
    ]
    spiges_col_types <- spiges_col_types[
      !names(spiges_col_types) %in% only_keys_tables
    ]
  }

  # --- read csv files ----------------------------------------------------
  read_result <- read_spiges_csv_files(
    dirname,
    selected_files,
    spiges_col_types
  )

  # --- check for problems and disply if any ----------------------------------------------------
  if (
    nrow(read_result$data$admin) > 0L &&
      "jahr" %in% names(read_result$data$admin)
  ) {
    Datenjahr <- read_result$data$admin$jahr[1]
  } else {
    Datenjahr <- NA
  }

  spiges_meta <- list(
    Datenjahr = Datenjahr,
    Datenversion = Datenversion,
    Source = "SpiGes",
    Sourceformat = "CSV",
    Version = version
  )

  spiges_data <- new_spiges_data(
    read_result$data,
    meta = spiges_meta,
    problems = read_result$problems
  )

  all_problems <- readr::problems(spiges_data)

  if (nrow(all_problems) > 0L) {
    cli::cli_inform(c(
      "i" = "{nrow(all_problems)} problems detected while reading SpiGes data.",
      "i" = "Use {.fn problems}() to inspect them, e.g.",
      ">" = "problems(x) or problems(x$admin)"
    ))
  }

  spiges_data
}


# Internal helper: build a cols_only() object from a schema, header, and
# an optional override from a readr::cols()/cols_only() object.
#
# @param spiges_tablecolumns  list of data.frame with columns: canonical, type, clear_name, anon_name
# @param col_types user provided cols() object
#
# @keywords internal
# @noRd
make_coltypes <- function(spiges_tablecolumns, fileheaders, col_types) {
  if (!is.null(col_types)) {
    user_col_types <-
      tibble::enframe(
        col_types$cols,
        name = 'varname',
        value = 'col_spec'
      ) |>
      dplyr::mutate(user_col_fg = 1L, matched = 0L)
  }

  spiges_coltypes <- vector(mode = 'list', length = length(spiges_tablecolumns))
  names(spiges_coltypes) <- names(spiges_tablecolumns)

  for (tablenm in names(spiges_tablecolumns)) {
    tablecolumns <- spiges_tablecolumns[[tablenm]]
    fileheader <- fileheaders[[tablenm]]

    spiges_cols <- tablecolumns |>
      dplyr::mutate(
        fileheader = dplyr::case_when(
          clear_name %in% fileheader ~ clear_name,
          anon_name %in% fileheader ~ anon_name,
          .default = NA_character_
        ),
        anon_fg = anon_name %in% fileheader,
        clear_anon_name = dplyr::if_else(
          clear_name == anon_name,
          clear_name,
          paste0(clear_name, '/', anon_name)
        ),
        missing_key = dplyr::if_else(
          is.na(fileheader) & key == 1,
          clear_anon_name,
          NA_character_
        )
      ) |>
      dplyr::filter(!is.na(fileheader) | key == 1) |>
      dplyr::select(fileheader, canonical, type, key, missing_key, anon_fg)

    ## check spiges_specs -----------------------------------
    not_in_spec <- setdiff(fileheader, spiges_cols$fileheader)

    if (length(not_in_spec) > 0) {
      warning(
        "Unknown Column(s) (",
        paste0(shQuote(not_in_spec), collapse = ', '),
        ") found in table ",
        tablenm,
        " please contact package author."
      )
      new_spiges_cols <-
        dplyr::tibble(
          fileheader = not_in_spec,
          canonical = tolower(not_in_spec),
          type = NA_character_,
          key = 0L,
          missing_key = NA_character_,
          anon_fg = as.logical(NA)
        )

      spiges_cols <- dplyr::bind_rows(spiges_cols, new_spiges_cols)
    }

    ## add readr col-specs -----------------------------------
    col_specs <- lapply(spiges_cols$type, function(t) {
      switch(
        t,
        integer = readr::col_double(), # to be checked an converted after reading
        double = readr::col_double(),
        numeric = readr::col_double(),
        character = readr::col_character(),
        date = readr::col_date(),
        datetime = readr::col_datetime(),
        logical = readr::col_logical(),
        spigesint = readr::col_integer(),
        spigeschar = readr::col_character(),
        # fallback for unknown/NA types
        readr::col_guess()
      )
    })
    spiges_cols$col_spec <- col_specs
    spiges_cols <- spiges_cols |>
      dplyr::mutate(user_col_fg = FALSE) |>
      dplyr::select(
        fileheader,
        canonical,
        type,
        col_spec,
        anon_fg,
        user_col_fg,
        key,
        missing_key
      )

    if (is.null(col_types)) {
      spiges_coltypes[[tablenm]] <- spiges_cols
      next
    }

    # from here we can assume user-defined `user_col_types` exists

    spiges_cols <-
      spiges_cols |>
      dplyr::left_join(
        user_col_types,
        by = c("fileheader" = "varname"),
        suffix = c('', '.header')
      ) |>
      dplyr::left_join(
        user_col_types,
        by = c("canonical" = "varname"),
        suffix = c('', '.canonical')
      ) |>
      dplyr::mutate(
        col_spec = dplyr::coalesce(
          col_spec.header,
          col_spec.canonical,
          col_spec
        ),
        user_col_fg = dplyr::coalesce(
          user_col_fg.header,
          user_col_fg.canonical,
          user_col_fg
        )
      ) |>
      dplyr::filter(user_col_fg == 1L | key == 1L) |>
      dplyr::select(
        fileheader,
        canonical,
        type,
        col_spec,
        anon_fg,
        user_col_fg,
        key,
        missing_key
      )

    # user col_types that are used in an actual table are marked as "matched"
    user_col_types$matched[
      user_col_types$varname %in%
        c(spiges_cols$fileheader, spiges_cols$canonical)
    ] <- 1L

    # check if keys are missing
    missing_key <- spiges_cols |>
      dplyr::filter(user_col_fg == 0L & key == 1L) |>
      dplyr::pull(missing_key)

    if (length(missing_key) > 0) {
      warning(
        "Key columns are mandatory, key-colum(s) ",
        paste0(shQuote(missing_key), collapse = ", "),
        " will be read nonetheless in table ",
        tablenm,
        call. = FALSE
      )
    }

    spiges_coltypes[[tablenm]] <- spiges_cols
  }

  if (!is.null(col_types)) {
    no_match <- user_col_types$varname[user_col_types$matched == 0L]
    if (length(no_match) > 0) {
      warning(
        "Element(s) of `col_types` ",
        paste0(shQuote(no_match), collapse = ", "),
        " were not found in any table.",
        call. = FALSE
      )
    }
  }
  return(spiges_coltypes)
}


# Internal helper: standard problems table with stable column types.
#
# @keywords internal
# @noRd
empty_problems_tbl <- function() {
  tibble::tibble(
    row = integer(),
    col = integer(),
    expected = character(),
    actual = character(),
    file = character()
  )
}


# Internal helper: read a SpiGes csv file based on the schema object.
# Returns data with canonical column names.
#
# @keywords internal
# @noRd
read_spiges_csv_files <- function(
  dirname,
  selected_files,
  spiges_coltypes
) {
  spiges_data <- vector(mode = 'list', length = length(selected_files))
  names(spiges_data) <- names(selected_files)
  spiges_problems <- empty_problems_tbl()

  # Prefer vroom for speed; fall back to readr if vroom isn't installed.
  use_vroom <- requireNamespace("vroom", quietly = TRUE)

  # Prepare key creation for fall_id mapping.
  key_cols <- c("jahr", "ent_id", "standort_id", "fall_id_spital")

  # --- read admin first --------------------------------------------------
  admin_path <- file.path(dirname, selected_files[["admin"]])
  admin_result <- read_spiges_csv_file(
    fpath = admin_path,
    tablenm = "admin",
    coltypes_tbl = spiges_coltypes[["admin"]],
    key_cols = key_cols,
    use_vroom = use_vroom
  )

  # Build the fall_id map once from the admin table.
  admin_key <- admin_result$data$fall_id
  unique_key <- admin_key[!duplicated(admin_key)]
  fall_id <- vapply(unique_key, digest::digest, character(1), algo = "xxhash64")
  fall_id_map <- stats::setNames(fall_id, unique_key)
  admin_result$data$fall_id <- fall_id_map[admin_key]

  if ("jahr" %in% names(admin_result$data)) {
    admin_result$data <- admin_result$data |>
      dplyr::relocate(fall_id, .after = jahr)
  }

  spiges_data[["admin"]] <- admin_result$data
  if (nrow(admin_result$problems) > 0L) {
    spiges_problems <- dplyr::bind_rows(spiges_problems, admin_result$problems)
  }

  # --- read non-admin tables --------------------------------------------
  non_admin_names <- setdiff(names(selected_files), "admin")
  for (tablenm in non_admin_names) {
    fpath <- file.path(dirname, selected_files[[tablenm]])

    read_result <- read_spiges_csv_file(
      fpath = fpath,
      tablenm = tablenm,
      coltypes_tbl = spiges_coltypes[[tablenm]],
      key_cols = key_cols,
      use_vroom = use_vroom
    )

    raw_key <- read_result$data$fall_id
    read_result$data$fall_id <- fall_id_map[raw_key]

    missing_idx <- which(is.na(read_result$data$fall_id))
    missing_problems <- empty_problems_tbl()
    if (length(missing_idx) > 0L) {
      missing_problems <- tibble::tibble(
        row = missing_idx,
        col = NA_integer_,
        expected = "fall key present in Administratives",
        actual = raw_key[missing_idx],
        file = tablenm
      )
    }

    file_problems <- dplyr::bind_rows(
      read_result$problems,
      missing_problems
    )

    if ("jahr" %in% names(read_result$data)) {
      read_result$data <- read_result$data |>
        dplyr::relocate(fall_id, .after = jahr)
    }

    # Drop key columns from non-admin tables after fall_id is set.
    read_result$data <- read_result$data |>
      dplyr::select(-dplyr::any_of(key_cols))

    spiges_data[[tablenm]] <- read_result$data
    if (nrow(file_problems) > 0L) {
      spiges_problems <- dplyr::bind_rows(spiges_problems, file_problems)
    }
  }

  list(
    data = spiges_data,
    problems = spiges_problems
  )
}


# Internal helper: read a single SpiGes csv file based on the schema object.
# Returns canonical column names and per-file problems.
#
# @keywords internal
# @noRd
read_spiges_csv_file <- function(
  fpath,
  tablenm,
  coltypes_tbl,
  key_cols = c("jahr", "ent_id", "standort_id", "fall_id_spital"),
  use_vroom = requireNamespace("vroom", quietly = TRUE)
) {
  # Build the rename map from file headers to canonical names.
  rename_map <- setNames(coltypes_tbl$fileheader, coltypes_tbl$canonical)

  # Build the readr::cols() spec (also accepted by vroom).
  col_name <- coltypes_tbl$fileheader
  col_type <- coltypes_tbl$col_spec
  cols_obj <- readr::cols(.default = readr::col_skip())
  cols_obj$cols <- stats::setNames(col_type, col_name)

  # --- read csv file ---------------------------------------------------
  spiges_csv <- if (use_vroom) {
    suppressWarnings(
      vroom::vroom(
        file = fpath,
        delim = ",",
        col_names = TRUE,
        col_types = cols_obj,
        quote = "\"",
        na = c("", " "),
        locale = readr::locale(encoding = "UTF-8")
        # progress: vroom default is fine
      )
    )
  } else {
    suppressWarnings(
      readr::read_delim(
        file = fpath,
        delim = ",",
        col_names = TRUE,
        col_types = cols_obj,
        quote = "\"",
        na = c("", " "),
        locale = readr::locale(encoding = "UTF-8"),
        show_col_types = FALSE
      )
    )
  }

  # capture parsing problems before any dplyr transforms drop them
  csv_problems <- readr::problems(spiges_csv)
  if (nrow(csv_problems) > 0L) {
    csv_problems <- dplyr::mutate(csv_problems, file = tablenm)
  } else {
    csv_problems <- empty_problems_tbl()
  }

  spiges_csv <- spiges_csv |>
    dplyr::rename(!!!rename_map)

  # Build the fall_id key
  spiges_csv <- tidyr::unite(
    spiges_csv,
    "fall_id",
    dplyr::all_of(key_cols),
    sep = "|",
    remove = FALSE
  )

  # build key for duplicate checks in this table (for selected tables only)
  if (tablenm %in% c('admin', 'newborn', 'psychiatry', 'diag', 'proc')) {
    if (tablenm == 'diag' & 'diagnose_id' %in% coltypes_tbl$canonical) {
      key_cols <- c(key_cols, 'diagnose_id')
    } else if (
      tablenm == 'proc' & 'behandlung_id' %in% coltypes_tbl$canonical
    ) {
      key_cols <- c(key_cols, 'behandlung_id')
    }

    tbl_key <- tidyr::unite(
      spiges_csv,
      "tbl_key",
      dplyr::all_of(key_cols),
      sep = "|"
    ) |>
      dplyr::pull(tbl_key)

    dup_idx <- which(duplicated(tbl_key))
    dup_problems <- empty_problems_tbl()
    if (length(dup_idx) > 0L) {
      dup_problems <- tibble::tibble(
        row = dup_idx,
        col = NA_integer_,
        expected = "unique fall key",
        actual = tbl_key[dup_idx],
        file = tablenm
      )
    }
  } else {
    dup_problems <- empty_problems_tbl()
  }

  missing_key_idx <- which(!stats::complete.cases(spiges_csv[key_cols]))
  missing_key_problems <- empty_problems_tbl()
  if (length(missing_key_idx) > 0L) {
    missing_key_problems <- tibble::tibble(
      row = missing_key_idx,
      col = NA_integer_,
      expected = "non-missing fall key components",
      actual = spiges_csv$fall_id[missing_key_idx],
      file = tablenm
    )
  }

  # prepare int conversion problems
  int_problems <- empty_problems_tbl()

  # --- convert numeric columns that are integer-like back to integer ----
  int_col_nr <- which(coltypes_tbl$type == 'integer')
  int_col_names <- coltypes_tbl$canonical[int_col_nr]

  for (col_name in int_col_names) {
    # skip if the canonical column is not present in the read data
    if (!col_name %in% names(spiges_csv)) {
      warning(
        "Expected integer column '",
        col_name,
        "' not found in table ",
        tablenm,
        call. = FALSE
      )
      next
    }

    col_value_raw <- spiges_csv[[col_name]]

    # attempt numeric coercion (suppress warnings for coercion)
    col_value_num <- suppressWarnings(as.numeric(col_value_raw))

    value_is_int <- is.na(col_value_num) |
      (is.finite(col_value_num) &
        abs(col_value_num - round(col_value_num)) <= 1e-8 &
        col_value_num >= -.Machine$integer.max - 1 &
        col_value_num <= .Machine$integer.max)

    bad_idx <- which(!value_is_int)

    if (length(bad_idx) > 0) {
      int_problems <- dplyr::bind_rows(
        int_problems,
        tibble::tibble(
          row = bad_idx,
          col = which(names(spiges_csv) == col_name),
          expected = "an integer",
          actual = as.character(col_value_raw[bad_idx]),
          file = tablenm
        )
      )
    }

    # --- convert -------------------------------------------------------
    int_value <- integer(length(col_value_num))
    int_value[value_is_int] <- as.integer(round(col_value_num[value_is_int]))
    int_value[!value_is_int] <- NA_integer_
    spiges_csv[[col_name]] <- int_value
  }

  file_problems <- dplyr::bind_rows(
    csv_problems,
    int_problems,
    dup_problems,
    missing_key_problems
  )

  list(
    data = spiges_csv,
    problems = file_problems
  )
}
