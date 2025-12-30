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
