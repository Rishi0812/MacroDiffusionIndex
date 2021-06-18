#' Get Dataset
#'
#' @return data
#' @export
#'
#' @examples
#'mdi_get_data()
#'

mdi_get_data <- function(){
  path <- system.file("extdata", "GSCO_SPX_Technical_CSV.csv", package = "MacroDiffusionIndex")
  data <- read.csv(path)

}
