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
