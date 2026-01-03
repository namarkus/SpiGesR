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

# internal: set metadata
spiges_set_meta <- function(
  x,
  Datenjahr,
  Datenversion,
  Source = c('SpiGes', 'MS'),
  Sourceformat = c('CSV', 'XML', 'DAT'),
  Version = "1.4"
) {
  stopifnot(inherits(x, "spiges_data"))

  attr(x, "meta") <- list(
    Datenjahr = Datenjahr,
    Datenversion = Datenversion,
    Source = Source,
    Sourceformat = Sourceformat,
    Version = Version
  )
  x
}

# internal: add problems
spiges_add_problems <- function(x, problems) {
  if (is.null(problems) || nrow(problems) == 0) {
    return(x)
  }

  p_old <- attr(x, "problems", exact = TRUE)
  if (is.null(p_old)) {
    p_old <- tibble::tibble()
  }

  p_new <- problems

  attr(x, "problems") <- dplyr::bind_rows(p_old, p_new)
  x
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
