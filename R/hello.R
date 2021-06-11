
#'
#' @param x
#'
#' @return The output from \code{\link{print}}
#' @export
#'
#' @examples
#' hello("Google")
#' \dontrun{
#' hello("GSoC")
#' }
#'
hello <- function(x) {
  print(paste0("Hello,", x,"!"))
}
