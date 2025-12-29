#' Read BFS hospital statistics (SpiGes) from a directory
#'
#' @param dirname Directory containing the CSV files.
#' @param tables Character vector of table names to read (fuzzy). If \code{NULL} (default), all available tables in the directory
#'   are read.
#' @param col_types Optional \code{readr::cols_only()} object that is used as
#'   \code{col_types} for all tables (overrides table-specific col_types).
#' @param version = '1.4' SpiGes Version}.
#'
#' @return A list with the requested tables (always containing \code{admin}),
#'   with attributes \code{Datenjahr}, \code{Datenversion}, \code{Source}.
#'
#' @importFrom dplyr if_else select mutate coalesce
#' @importFrom tibble tibble enframe
#'
#' @export
read_bfs_spiges <- function(
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
  spiges_data <- read_spiges_csv(dirname, selected_files, spiges_col_types)

  # --- Metadata attributes -----------------------------------------------
  if (nrow(spiges_data$admin) > 0L && "jahr" %in% names(spiges_data$admin)) {
    Datenjahr <- spiges_data$admin$jahr[1]
  } else {
    Datenjahr <- NA
  }

  attr(spiges_data, "Datenjahr") <- Datenjahr
  attr(spiges_data, "Datenversion") <- Datenversion
  attr(spiges_data, "Source") <- "SpiGes"
  attr(spiges_data, "Sourcefromat") <- "CSV"
  attr(spiges_data, "Version") <- "SpiGes"

  class(spiges_data) <- c("spiges_data", class(spiges_data))

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
          canonical = coalesce(canonical, tolower(fileheader)),
          type = coalesce(type, 'unknown')
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
        col_spec = coalesce(user_col_spec.x, user_col_spec.y, col_spec),
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
read_spiges_csv <- function(
  dirname,
  selected_files,
  spiges_coltypes
) {
  spiges_data <- vector(mode = 'list', length = length(selected_files))
  names(spiges_data) <- names(selected_files)

  for (tablenm in names(selected_files)) {
    # mapping for rename variables
    rename_map <- setNames(
      spiges_coltypes[[tablenm]]$fileheader,
      spiges_coltypes[[tablenm]]$canonical
    )

    # build readr::cols object
    col_name <- spiges_coltypes[[tablenm]]$fileheader
    col_type <- spiges_coltypes[[tablenm]]$col_spec

    cols_obj <- readr::cols(.default = readr::col_skip())
    cols_obj$cols <- stats::setNames(col_type, col_name)

    # read csv file
    spiges_csv <-
      readr::read_delim(
        file = file.path(dirname, selected_files[[tablenm]]),
        delim = ",",
        col_names = TRUE,
        col_types = cols_obj,
        quote = '"',
        na = c("", " "),
        locale = readr::locale(encoding = "UTF-8"),
        show_col_types = FALSE
      ) |>
      suppressWarnings() |>
      dplyr::rename(!!!rename_map)

    # --- add unique ID (compact xxhash64 key)
    spiges_csv <- spiges_csv |>
      dplyr::mutate(
        fall_id = paste(
          jahr,
          ent_id,
          standort_id,
          fall_id_spital,
          sep = "|"
        )
      )

    spiges_csv$fall_id = sapply(spiges_csv$fall_id, function(x) {
      digest::digest(x, algo = 'xxhash64')
    })

    # get problems for read file
    csv_problems <- readr::problems(spiges_csv)

    # prepe int conversion problems
    int_problems <- tibble(
      row = integer(),
      col = integer(),
      expected = character(),
      actual = character()
    )

    # --- convert numeric columns that are integer-like back to integer ----
    int_col_nr <- which(spiges_coltypes[[tablenm]]$type == 'integer')

    for (col_n in int_col_nr) {
      col_value <- spiges_csv[[col_n]]
      value_is_int <- is.na(col_value) |
        (is.finite(col_value) & abs(col_value - round(col_value)) <= 1e-8)

      # --- Problems sammeln -------------------------------------------
      bad_idx <- which(!value_is_int)

      if (length(bad_idx) > 0) {
        int_problems <- dplyr::bind_rows(
          int_problems,
          tibble(
            row = bad_idx,
            col = int_col_nr,
            expected = "an integer",
            actual = as.character(col_value[bad_idx]),
            file = file
          )
        )
      }

      # --- Konvertieren -----------------------------------------------
      int_value <- as.integer(round(col_value))
      int_value[!value_is_int] <- NA_integer_
      spiges_csv[[col_n]] <- int_value
    }

    # attach combined problems to the resulting data.frame (keeps same structure as readr::problems())
    problems <- dplyr::bind_rows(csv_problems, int_problems)
    attr(spiges_csv, "problems") <- problems

    spiges_data[[tablenm]] <- spiges_csv |>
      dplyr::relocate(fall_id, .after = jahr)
  }

  return(spiges_data)
}
