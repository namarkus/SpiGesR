#' Filter spiges_data by admin table
#'
#' @inheritParams dplyr::filter
#' @export
filter.spiges_data <- function(.data, ...) {
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

  class(out) <- class(.data)
  out
}

#' Select variables from a table in a spiges_data object
#'
#' Selects columns from one table contained in a `spiges_data` (a named list of
#' tibbles/data.frames). Selection syntax in `...` uses tidyselect, like
#' [dplyr::select()].
#'
#' @param .data A `spiges_data` object (named list of tables).
#' @param tab Table to modify (unquoted name or string), e.g. `admin` or `"admin"`.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to select from
#'   the chosen table. Supports the same helpers as [dplyr::select()], e.g.
#'   `starts_with()`, `ends_with()`, `where()`, `-col`, etc.
#'
#' @return A `spiges_data` object with the selected columns applied to `tab`.
#' @seealso [dplyr::select()], [dplyr::dplyr_tidy_select]
#' @export
select_test <- function(.data, tab, ...) {
  check_spiges_data(.data)

  tab_name <- rlang::as_string(rlang::ensym(tab))
  check_spiges_table(.data, tab_name)

  if (!tab_name %in% names(.data)) {
    rlang::abort(paste0("Tabelle '", tab_name, "' nicht vorhanden."))
  }

  .data[[tab_name]] <- dplyr::select(.data[[tab_name]], ...)
  .data
}


spiges24_fr |>
  select_test(
    tab = admin,
    jahr,
    fall_id,
    ent_id,
    uid,
    spital_id,
    standort_id,
    fall_id_ch,
    fall_id_spital
  ) |>
  purrr::pluck('admin')

spiges24_fr |>
  select_test(
    jahr,
    fall_id,
    ent_id,
    uid,
    spital_id,
    standort_id,
    fall_id_ch,
    fall_id_spital
  ) |>
  purrr::pluck('admin')
