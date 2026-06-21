#' @title Helper to compare values treating NA as equal
#'
#' @param x A vector of values to compare.
#' @param y A vector of values to compare against.
#'
#' @return A logical vector indicating where `x` and `y` are equal, treating
#' NA values as equal to each other.
#'
#' @examples
#' x <- c(1, 2, NA, 4)
#' y <- c(1, 2, NA, 5)
#' NA_equal(x, y) # Returns: TRUE TRUE TRUE FALSE
#'
#' @noRd

NA_equal <- function(x, y) {
  (x == y) | (is.na(x) & is.na(y))
}
