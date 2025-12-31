#' spiges_data class
#'

# internal constructor
new_spiges_data <- function(x, meta = NULL) {
  stopifnot(is.list(x))
  class(x) <- c("spiges_data", class(x))
  if (!is.null(meta)) {
    attr(x, "meta") <- meta
  }
  x
}

# internal: get metadata
spiges_meta <- function(x) {
  attr(x, "meta")
}

# internal: set metadata
spiges_set_meta <- function(x, meta) {
  attr(x, "meta") <- meta
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

#' Extract parsing and key problems from spiges_data
#'
#' @param x A spiges_data object
#' @param ... unused
#'
#' @export
problems.spiges_data <- function(x, ...) {
  stopifnot(is.list(x))

  res <- lapply(names(x), function(nm) {
    p <- attr(x[[nm]], "problems", exact = TRUE)
    if (is.null(p) || nrow(p) == 0L) {
      return(NULL)
    }
    p$table <- nm
    p
  })

  res <- res[!vapply(res, is.null, logical(1))]

  if (length(res) == 0L) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(res)
}
