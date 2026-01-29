#' Filter spiges_data by admin table
#'
#' @inheritParams dplyr::filter
#' @export
filter.spiges_data <- function(.data, ...) {
  check_spiges_data(.data)

  ids <- .data$admin |>
    dplyr::filter(...) |>
    dplyr::pull(fall_id) |>
    unique()

  out <- lapply(.data, function(df) {
    if (is.data.frame(df) && "fall_id" %in% names(df)) {
      dplyr::filter(df, fall_id %in% ids)
    } else {
      df
    }
  })

  restore_spiges_data(out, .data)
}

#' Filter rows in a specific table of a spiges_data object
#'
#' Applies [dplyr::filter()] to a single table inside a `spiges_data` object.
#' All other tables remain unchanged.
#'
#' @inheritParams dplyr::filter
#' @param .data A `spiges_data` object (named list of tables).
#' @param .tab Table to filter (unquoted name or string), e.g. `admin` or `"admin"`.
#'
#' @return A `spiges_data` object with filtered rows in `.tab`.
#' @seealso [dplyr::filter()]
#' @export
filter_in <- function(.data, .tab, ...) {
  check_spiges_data(.data)

  tab_name <- rlang::as_string(rlang::ensym(.tab))
  check_spiges_table(.data, tab_name)

  .data[[tab_name]] <- dplyr::filter(.data[[tab_name]], ...)
  .data
}

#' Mutate variables in a table of a spiges_data object
#'
#' Applies [dplyr::mutate()] to a single table inside a `spiges_data` object.
#' All other tables remain unchanged.
#'
#' @inheritParams dplyr::mutate
#' @param .data A `spiges_data` object (named list of tables).
#' @param .tab Table to mutate (unquoted name or string), e.g. `admin` or `"admin"`.
#'
#' @return A `spiges_data` object with mutated columns in `.tab`.
#' @seealso [dplyr::mutate()]
#' @export
mutate_in <- function(.data, .tab, ...) {
  check_spiges_data(.data)

  tab_name <- rlang::as_string(rlang::ensym(.tab))
  check_spiges_table(.data, tab_name)

  .data[[tab_name]] <- dplyr::mutate(.data[[tab_name]], ...)
  .data
}

#' Select variables from a table in a spiges_data object
#'
#' Selects columns from one table contained in a `spiges_data` (a named list of
#' tibbles/data.frames). Selection syntax in `...` uses tidyselect, like
#' [dplyr::select()].
#'
#' @param .data A `spiges_data` object (named list of tables).
#' @param .tab Table to modify (unquoted name or string), e.g. `admin` or `"admin"`.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to select from
#'   the chosen table. Supports the same helpers as [dplyr::select()], e.g.
#'   `starts_with()`, `ends_with()`, `where()`, `-col`, etc.
#'
#' @return A `spiges_data` object with the selected columns applied to `.tab`.
#' @seealso [dplyr::select()], [dplyr::dplyr_tidy_select]
#' @export
select_in <- function(.data, .tab, ...) {
  check_spiges_data(.data)

  tab_name <- rlang::as_string(rlang::ensym(.tab))
  check_spiges_table(.data, tab_name)

  if (!tab_name %in% names(.data)) {
    rlang::abort(paste0("Tabelle '", tab_name, "' nicht vorhanden."))
  }

  .data[[tab_name]] <- dplyr::select(.data[[tab_name]], ...)
  .data
}

#' Select tables from a spiges_data object
#'
#' Selects tables from a `spiges_data` object (a named list of tables).
#' Selection syntax in `...` uses tidyselect, like [dplyr::select()].
#'
#' @param .data A `spiges_data` object (named list of tables).
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> Tables to select.
#'
#' @return A `spiges_data` object with only the selected tables.
#' @seealso [dplyr::select()], [dplyr::dplyr_tidy_select]
#' @export
select.spiges_data <- function(.data, ...) {
  check_spiges_data(.data)

  out <- dplyr::select(.data, ...)
  restore_spiges_data(out, .data)
}

# internal: restore attributes from spiges_data object while keeping names
restore_spiges_data <- function(out, template) {
  attrs <- attributes(template)
  attrs$names <- names(out)
  attrs$class <- class(template)
  attributes(out) <- attrs
  out
}
