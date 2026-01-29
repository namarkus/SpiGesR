# Interne Arbeitsfunktion (nicht exportiert)
.join_spiges <- function(
  .data,
  .tabx,
  .taby,
  type = "left",
  by = "fall_id",
  .into = NULL,
  ...
) {
  check_spiges_data(.data)
  template <- .data

  tabx_name <- rlang::as_string(rlang::ensym(.tabx))
  check_spiges_table(.data, tabx_name)

  taby_name <- rlang::as_string(rlang::ensym(.taby))
  check_spiges_table(.data, taby_name)

  into_name <- NULL
  if (!is.null(.into)) {
    into_name <- rlang::as_string(rlang::ensym(.into))
  }

  # Join-Funktion auswählen
  join_fn <- switch(
    type,
    "left" = dplyr::left_join,
    "right" = dplyr::right_join,
    "inner" = dplyr::inner_join,
    "full" = dplyr::full_join,
    stop("Unknown join type: ", type)
  )

  # n:m Warnung (nur für einfache character-by)
  if (is.character(by)) {
    by_x <- names(by)
    if (is.null(by_x)) {
      by_x <- by
    }
    by_y <- unname(by)

    x_keys <- .data[[tabx_name]][, by_x, drop = FALSE]
    y_keys <- .data[[taby_name]][, by_y, drop = FALSE]

    dup_x <- any(duplicated(x_keys))
    dup_y <- any(duplicated(y_keys))
    if (dup_x && dup_y) {
      rlang::warn(
        "Potential n:m join detected: duplicate keys in both tables."
      )
    }
  }

  # Join durchführen
  result <- join_fn(.data[[tabx_name]], .data[[taby_name]], by = by, ...)

  # Ergebnis platzieren
  if (is.null(into_name)) {
    .data[[tabx_name]] <- result
  } else {
    if (into_name %in% names(.data)) {
      stop(sprintf("Table '%s' already exists", into_name))
    }
    .data[[into_name]] <- result
  }

  restore_spiges_data(.data, template)
}

#' @importFrom dplyr left_join
#' @export
#' @method left_join spiges_data
left_join.spiges_data <- function(
  .data,
  .taby,
  by = "fall_id",
  .into = NULL,
  ...
) {
  .join_spiges(
    .data,
    .tabx = "admin",
    .taby = .taby,
    type = "left",
    by = by,
    .into = .into,
    ...
  )
}

#' @importFrom dplyr inner_join
#' @export
#' @method inner_join spiges_data
inner_join.spiges_data <- function(
  .data,
  .taby,
  by = "fall_id",
  .into = NULL,
  ...
) {
  .join_spiges(
    .data,
    .tabx = "admin",
    .taby = .taby,
    type = "inner",
    by = by,
    .into = .into,
    ...
  )
}

#' @importFrom dplyr right_join
#' @export
#' @method right_join spiges_data
right_join.spiges_data <- function(
  .data,
  .taby,
  by = "fall_id",
  .into = NULL,
  ...
) {
  .join_spiges(
    .data,
    .tabx = "admin",
    .taby = .taby,
    type = "right",
    by = by,
    .into = .into,
    ...
  )
}

#' @importFrom dplyr full_join
#' @export
#' @method full_join spiges_data
full_join.spiges_data <- function(
  .data,
  .taby,
  by = "fall_id",
  .into = NULL,
  ...
) {
  .join_spiges(
    .data,
    .tabx = "admin",
    .taby = .taby,
    type = "full",
    by = by,
    .into = .into,
    ...
  )
}

# *_tab Funktionen (auch sehr kurz!)
#' @export
left_join_tab <- function(
  .data,
  .tabx,
  .taby,
  by = "fall_id",
  .into = NULL,
  ...
) {
  .join_spiges(
    .data,
    .tabx = .tabx,
    .taby = .taby,
    type = "left",
    by = by,
    .into = .into,
    ...
  )
}

#' @export
inner_join_tab <- function(
  .data,
  .tabx,
  .taby,
  by = "fall_id",
  .into = NULL,
  ...
) {
  .join_spiges(
    .data,
    .tabx = .tabx,
    .taby = .taby,
    type = "inner",
    by = by,
    .into = .into,
    ...
  )
}

#' @export
right_join_tab <- function(
  .data,
  .tabx,
  .taby,
  by = "fall_id",
  .into = NULL,
  ...
) {
  .join_spiges(
    .data,
    .tabx = .tabx,
    .taby = .taby,
    type = "right",
    by = by,
    .into = .into,
    ...
  )
}

#' @export
full_join_tab <- function(
  .data,
  .tabx,
  .taby,
  by = "fall_id",
  .into = NULL,
  ...
) {
  .join_spiges(
    .data,
    .tabx = .tabx,
    .taby = .taby,
    type = "full",
    by = by,
    .into = .into,
    ...
  )
}
