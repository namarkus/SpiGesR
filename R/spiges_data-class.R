#' spiges_data class
#'

# internal constructor
new_spiges_data <- function(x, meta = NULL, problems = NULL) {
  stopifnot(is.list(x))

  # Klasse setzen (und list drinlassen)
  class(x) <- unique(c("spiges_data", class(x)))

  # meta immer als Attribut (NULL ist ok, oder leeres list/tibble – wie du willst)
  if (is.null(meta)) {
    meta <- list()
  }
  stopifnot(is.list(meta), !is.null(names(meta)) || length(meta) == 0L)
  attr(x, "meta") <- meta

  # problems immer als Attribut (leer statt NULL ist meist praktischer)
  if (is.null(problems)) {
    problems <- tibble::tibble()
  }
  attr(x, "problems") <- problems

  x
}


# internal: get metadata
spiges_meta <- function(x) {
  attr(x, "meta")
}

# internal validation
validate_spiges_data <- function(x) {
  stopifnot(
    inherits(x, "spiges_data"),
    is.list(x),
    "admin" %in% names(x)
  )
  x
}

#' @importFrom readr problems
NULL

#' Extract parsing and key problems from spiges_data
#'
#' @param x A spiges_data object
#' @param ... unused
#'
#' @export
problems.spiges_data <- function(x, ...) {
  stopifnot(inherits(x, "spiges_data"))

  p <- attr(x, "problems", exact = TRUE)

  if (is.null(p) || !inherits(p, "data.frame") || nrow(p) == 0L) {
    return(tibble::tibble())
  }

  tibble::as_tibble(p)
}

#' @export
print.spiges_data <- function(x, ..., n = 50) {
  stopifnot(inherits(x, "spiges_data"), is.list(x))

  meta <- attr(x, "meta", exact = TRUE)
  if (is.null(meta)) {
    meta <- list()
  }

  strip_ms <- function(v) {
    if (is.null(v) || length(v) == 0 || is.na(v)) {
      return(NA_character_)
    }

    # POSIXct/POSIXlt -> seconds precision
    if (inherits(v, c("POSIXct", "POSIXlt"))) {
      return(format(v, "%Y-%m-%d %H:%M:%S"))
    }

    s <- as.character(v[[1]])
    # remove fractional seconds ".123", ".123456" etc.
    s <- sub("(\\d{2}:\\d{2}:\\d{2})\\.[0-9]+", "\\1", s)
    # remove trailing ".123" if no time present
    s <- sub("\\.[0-9]+$", "", s)
    s
  }

  datenjahr <- meta$Datenjahr
  datenversion <- format(as.POSIXct(meta$Datenversion), "%Y-%m-%d %H:%M:%S")

  # --- line 1 (only year + version) ---
  # (keeps output stable even if one is missing)
  if (!is.na(datenjahr) && !is.na(datenversion) && length(datenversion) > 0) {
    cli::cli_text(
      "{.strong SpiGes} Datenjahr = {datenjahr} | Datenversion = {.dim {datenversion}}"
    )
  } else if (!is.na(datenjahr)) {
    cli::cli_text("{.strong SpiGes} Datenjahr = {datenjahr}")
  } else if (!is.na(datenversion)) {
    cli::cli_text("{.strong SpiGes} Datenversion = {.dim {datenversion}}")
  } else {
    cli::cli_text("{.strong SpiGes}")
  }

  # --- tables ---------
  nm <- names(x)
  if (is.null(nm)) {
    nm <- rep.int("", length(x))
  }
  n_show <- min(length(x), n)

  if (length(x) == 0L) {
    cli::cli_text("{.dim (no tables)}")
  } else {
    for (i in seq_len(n_show)) {
      tab_nm <- nm[[i]]
      if (is.na(tab_nm) || tab_nm == "") {
        tab_nm <- paste0("[[", i, "]]")
      }
      xi <- x[[i]]

      if (inherits(xi, "data.frame")) {
        cli::cli_text("• {tab_nm} {.dim ({nrow(xi)} x {ncol(xi)})}")
      } else {
        cli::cli_text("• {tab_nm} {.dim (non-data.frame)}")
      }
    }
    if (n_show < length(x)) {
      cli::cli_text("{.dim … and {length(x) - n_show} more.}")
    }
  }

  # --- problems hint (colored) ---
  p <- attr(x, "problems", exact = TRUE)
  nprob <- if (is.null(p) || !inherits(p, "data.frame")) 0L else nrow(p)

  if (nprob > 0L) {
    cli::cli_alert_warning(
      "{nprob} parsing/key problem{?s} detected. Use {.code problems(x)}."
    )
  }

  invisible(x)
}


#' Validate a spiges_data object
#'
#' Internal helper that checks whether an object inherits from
#' the `spiges_data` class. Throws an error otherwise.
#'
#' @param x Object to check.
#'
#' @return Invisibly returns `TRUE` if `x` is a `spiges_data` object.
#'   Otherwise, an error is thrown.
#'
#' @keywords internal
check_spiges_data <- function(x) {
  if (!inherits(x, "spiges_data")) {
    spiges_abort("`.data` must be a `spiges_data` object.")
  }
  invisible(TRUE)
}

#' Validate a table name in a spiges_data object
#'
#' Internal helper that checks whether a given table exists in a
#' `spiges_data` object.
#'
#' @param .data A `spiges_data` object.
#' @param tab Table name as a single string.
#'
#' @return Invisibly returns `TRUE` if the table exists.
#'   Otherwise, an error is thrown.
#'
#' @keywords internal
check_spiges_table <- function(.data, tab) {
  if (!tab %in% names(.data)) {
    spiges_abort(
      c(
        "Invalid table" = paste0(
          "Table '",
          tab,
          "' does not exist in `.data`."
        ),
        "Available tables" = paste(names(.data), collapse = ", ")
      )
    )
  }
  invisible(TRUE)
}


#' Validate column names in a spiges table
#'
#' Internal helper that checks whether specified columns exist
#' in a table of a `spiges_data` object.
#'
#' @param data_tab A data.frame or tibble (single table from `spiges_data`).
#' @param cols Character vector of column names.
#' @param tab Optional table name (used for error messages).
#'
#' @return Invisibly returns `TRUE` if all columns exist.
#'   Otherwise, an error is thrown.
#'
#' @keywords internal
check_spiges_columns <- function(data_tab, cols, tab = NULL) {
  missing <- setdiff(cols, names(data_tab))

  if (length(missing) > 0) {
    msg <- paste0("Unknown column(s): ", paste(missing, collapse = ", "))
    if (!is.null(tab)) {
      msg <- paste0(msg, " in table '", tab, "'.")
    }
    spiges_abort(msg)
  }

  invisible(TRUE)
}

#' Throw a SpiGesR error
#'
#' Internal helper to signal errors consistently across SpiGesR.
#' Wraps [rlang::abort()] and standardizes error header and class.
#'
#' @param message A character vector or named character vector of messages.
#'   Use a named vector to create a structured error (header + bullets).
#' @param ... Unused (reserved for future extensions).
#' @param call The call to display. Defaults to [rlang::caller_env()].
#' @param class Additional condition classes (character vector).
#'
#' @return Nothing. This function always throws an error.
#'
#' @keywords internal
spiges_abort <- function(
  message,
  ...,
  call = rlang::caller_env(),
  class = NULL
) {
  # Ensure a consistent top-level class for tests/handlers
  cond_class <- unique(c(class, "spiges_error"))

  # If user passed a single unnamed string, prepend a standard header
  if (
    is.character(message) && length(message) == 1L && is.null(names(message))
  ) {
    message <- c("SpiGesR error" = message)
  }

  rlang::abort(message = message, call = call, class = cond_class)
}
