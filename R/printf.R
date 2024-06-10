#' Shorthand matching C-style printf() functionality.
#'
#' @param ... A character vector, an object, or any combination of these. If an initial character vector is supplied, it is parsed using [sprintf()].
#'
#' @return N/A
#' @export
#'
#' @seealso [sprintf()]
#' @examples
#' printf("%d %s", 1, "abc")
printf = function (...) {
  cat(paste(sprintf(...),"\n"))
}
