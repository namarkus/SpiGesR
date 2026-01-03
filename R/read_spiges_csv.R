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

  # --- read csv files ----------------------------------------------------
  spiges_data <- read_spiges_csv_files(
    dirname,
    selected_files,
    spiges_col_types
  )

  # --- check for problems and disply if any ----------------------------------------------------
  all_problems <- readr::problems(spiges_data)

  if (nrow(all_problems) > 0L) {
    cli::cli_inform(c(
      "i" = "{nrow(all_problems)} problems detected while reading SpiGes data.",
      "i" = "Use {.fn problems}() to inspect them, e.g.",
      ">" = "problems(x) or problems(x$admin)"
    ))
  }

  # --- Metadata attributes -----------------------------------------------
  if (nrow(spiges_data$admin) > 0L && "jahr" %in% names(spiges_data$admin)) {
    Datenjahr <- spiges_data$admin$jahr[1]
  } else {
    Datenjahr <- NA
  }

  spiges_data <- spiges_set_meta(
    spiges_data,
    Datenjahr = Datenjahr,
    Datenversion = Datenversion,
    Source = "SpiGes",
    Sourceformat = 'CSV',
    Version = version
  )
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
    col_types <-
      tibble::enframe(
        col_types$cols,
        name = 'user_varname',
        value = 'user_col_spec'
      )
  }

  spiges_coltypes <- vector(mode = 'list', length = length(spiges_tablecolumns))
  names(spiges_coltypes) <- names(spiges_tablecolumns)

  for (tablenm in names(spiges_tablecolumns)) {
    tablecolumns <- spiges_tablecolumns[[tablenm]]
    fileheader <- fileheaders[[tablenm]]
    spiges_cols <- tibble::tibble(fileheader = fileheaders[[tablenm]])

    spiges_cols <- spiges_cols |>
      dplyr::left_join(tablecolumns, by = c('fileheader' = 'clear_name')) |>
      dplyr::select(
        fileheader,
        canonical_clear = canonical,
        type_clear = type
      ) |>
      dplyr::left_join(tablecolumns, by = c('fileheader' = 'anon_name')) |>
      dplyr::select(
        fileheader,
        canonical_clear,
        canonical_anon = canonical,
        type_clear,
        type_anon = type
      ) |>
      dplyr::mutate(
        anon_fg = is.na(canonical_clear) & !is.na(canonical_anon)
      ) |>
      dplyr::mutate(
        canonical = dplyr::if_else(anon_fg, canonical_anon, canonical_clear),
        type = dplyr::if_else(anon_fg, type_anon, type_clear)
      ) |>
      dplyr::select(fileheader, canonical, type, anon_fg)

    ## check spiges_specs -----------------------------------
    no_canonical = which(is.na(spiges_cols$canonical))

    if (length(no_canonical) > 0) {
      warning(
        "Unknown Column(s) (",
        paste0(shQuote(spiges_cols$fileheader[no_canonical]), collapse = ', '),
        ") found in table ",
        tablenm,
        " please contact package author."
      )
      spiges_cols <- spiges_cols |>
        dplyr::mutate(
          canonical = dplyr::coalesce(canonical, tolower(fileheader)),
          type = dplyr::coalesce(type, 'unknown')
        )
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

    if (is.null(col_types)) {
      spiges_coltypes[[tablenm]] <-
        spiges_cols |>
        dplyr::mutate(user_col_fg = FALSE) |>
        dplyr::select(
          fileheader,
          canonical,
          type,
          col_spec,
          anon_fg,
          user_col_fg
        )

      next
    }

    spiges_coltypes[[tablenm]] <-
      spiges_cols |>
      dplyr::left_join(col_types, by = c('fileheader' = 'user_varname')) |>
      dplyr::left_join(col_types, by = c('canonical' = 'user_varname')) |>
      dplyr::mutate(
        col_spec = dplyr::coalesce(user_col_spec.x, user_col_spec.y, col_spec),
        user_col_fg = !is.na(user_col_spec.x) | !is.na(user_col_spec.y)
      ) |>
      dplyr::select(fileheader, canonical, type, col_spec, anon_fg, user_col_fg)
  }

  return(spiges_coltypes)
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
  spiges_data <- new_spiges_data(spiges_data)

  # Prefer vroom for speed; fall back to readr if vroom isn't installed.
  use_vroom <- requireNamespace("vroom", quietly = TRUE)

  # Ensure the Administratives table is processed first so we can build a
  # fall_id lookup map once and reuse it for all other tables.
  selected_files <- selected_files[c(
    "admin",
    setdiff(names(selected_files), "admin")
  )]
  spiges_coltypes <- spiges_coltypes[names(selected_files)]

  # prepare key creation
  key_cols <- c("jahr", "ent_id", "standort_id", "fall_id_spital")
  fall_id_map <- NULL

  for (tablenm in names(selected_files)) {
    # mapping for rename variables
    rename_map <- setNames(
      spiges_coltypes[[tablenm]]$fileheader,
      spiges_coltypes[[tablenm]]$canonical
    )

    # build readr::cols object (also accepted by vroom)
    col_name <- spiges_coltypes[[tablenm]]$fileheader
    col_type <- spiges_coltypes[[tablenm]]$col_spec

    cols_obj <- readr::cols(.default = readr::col_skip())
    cols_obj$cols <- stats::setNames(col_type, col_name)

    fpath <- file.path(dirname, selected_files[[tablenm]])

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
          quote = '"',
          na = c("", " "),
          locale = readr::locale(encoding = "UTF-8"),
          show_col_types = FALSE
        )
      )
    }

    spiges_csv <- spiges_csv |>
      dplyr::rename(!!!rename_map)

    # --- create unique key ----------------------------
    key <- do.call(
      paste,
      c(spiges_csv[key_cols], sep = "|")
    )

    dup_idx <- integer(0) # for duplicate checks
    if (tablenm == 'admin') {
      # Record duplicate keys (ignore them for mapping purposes, i.e. keep first)
      dup_idx <- which(duplicated(key))

      ukey <- key[!duplicated(key)]
      fall_id <- vapply(ukey, digest::digest, character(1), algo = "xxhash64")
      fall_id_map <- stats::setNames(fall_id, ukey)

      spiges_csv$fall_id <- fall_id_map[key]
    } else {
      if (is.null(fall_id_map)) {
        stop(
          "Internal error: fall_id_map is not initialised. Ensure Administratives is read first.",
          call. = FALSE
        )
      }

      spiges_csv$fall_id <- fall_id_map[key]
    }

    # get problems for read file
    csv_problems <- readr::problems(spiges_csv)

    # prepare int conversion problems
    int_problems <- tibble::tibble(
      row = integer(),
      col = integer(),
      expected = character(),
      actual = character()
    )

    # --- convert numeric columns that are integer-like back to integer ----
    int_col_nr <- which(spiges_coltypes[[tablenm]]$type == 'integer')
    int_col_names <- spiges_coltypes[[tablenm]]$canonical[int_col_nr]

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

    # --- add join-key problems ------------------------------------------
    dup_problems <- tibble::tibble(
      row = integer(),
      col = integer(),
      expected = character(),
      actual = character(),
      file = character()
    )
    if (length(dup_idx) > 0L) {
      dup_problems <- tibble::tibble(
        row = dup_idx,
        col = NA_integer_,
        expected = "unique fall key",
        actual = key[dup_idx],
        file = tablenm
      )
    }

    missing_idx <- which(is.na(spiges_csv$fall_id))
    missing_problems <- tibble::tibble(
      row = integer(),
      col = integer(),
      expected = character(),
      actual = character(),
      file = character()
    )
    if (tablenm != 'admin' && length(missing_idx) > 0L) {
      missing_problems <- tibble::tibble(
        row = missing_idx,
        col = NA_integer_,
        expected = "fall key present in Administratives",
        actual = key[missing_idx],
        file = tablenm
      )
    }

    file_problems <- dplyr::bind_rows(
      csv_problems,
      int_problems,
      dup_problems,
      missing_problems
    )

    spiges_csv_out <- spiges_csv |> dplyr::relocate(fall_id, .after = jahr)

    spiges_data[[tablenm]] <- spiges_csv_out
    spiges_data <- spiges_add_problems(spiges_data, file_problems)
  }

  spiges_data
}
