#' spiges helpers

#' Check presence of required SpiGes tables
#'
#' @param spiges_data list or object of class 'spiges_data'
#' @param tablenames character vector of table names to check
#' @return invisible(NULL) if all tables present, otherwise throws an error
check_spiges_tables <- function(spiges_data, tablenames) {
  if (!is.list(spiges_data)) {
    stop("spiges_data must be a list")
  }

  if (!inherits(spiges_data, "spiges_data")) {
    stop("spiges_data must be an object of class `spiges_data`")
  }

  missing_tables <- setdiff(tablenames, names(spiges_data))
  if (length(missing_tables) > 0L) {
    stop(
      "Missing table(s): ",
      paste(missing_tables, collapse = ', '),
      " is/are needed and not present."
    )
  }

  invisible(NULL)
}

#' Check presence of required SpiGes variables
#'
#' @param spiges_table dataframe of a SpiGes-data Object, eg. `admin`
#' @param var_names character vector of variable names to check
#' @return invisible(NULL) if all tables present, otherwise throws an error
check_spiges_var <- function(spiges_table, var_names) {
  if (!inherits(spiges_table, "data.frame")) {
    stop("spiges_data must be a dataframe")
  }

  missing_cols <- setdiff(var_names, names(spiges_table))

  if (length(missing_cols) > 0) {
    stop(
      "Missing column(s): ",
      paste(missing_cols, collapse = ', '),
      " is/are needed and not present."
    )
  }

  invisible(NULL)
}

#' coerce variable of different classes to Date (withour Hour)
#'
#' @param x vector to be coerced to SpiGes-Date
#' @return vector fo class Date
spiges2date <- function(x) {
  spiges_date <- as.Date(rep(NA, length(x)))

  if (inherits(x, 'POSIXt')) {
    tz <- attr(x, "tzone")
    if (is.null(tz) || tz == "") {
      tz <- Sys.timezone()
    }
    spiges_date <- as.Date(x, tz = tz)
  } else if (inherits(x, 'Date')) {
    spiges_date <- x
  } else if (inherits(x, c('character', 'integer', 'numeric'))) {
    spiges_date <- as.Date(
      substr(as.character(x), 1, 8),
      format = '%Y%m%d'
    )
  }
  spiges_date
}

#' coerce variable of different classes to character vector date string (withour Hour)
#'
#' @param x vector to be coerced to character (format SpiGes-Date)
#' @return vector fo class character
spiges2datestr <- function(x) {
  spiges_chr <- character(length = length(x))

  if (inherits(x, c('Date', 'POSIXt'))) {
    spiges_chr <- format(x, format = '%Y%m%d')
  } else if (inherits(x, c('character', 'integer', 'numeric'))) {
    date_str = substr(as.character(x), 1, 8)
    date_fmt = grepl('^\\d{8}$', date_str)

    spiges_chr[date_fmt] <- date_str[date_fmt]
  }
  spiges_chr
}
