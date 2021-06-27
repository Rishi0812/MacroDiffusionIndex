#' Get Dataset
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @import quantmod
#'
#' @examples
#'mdi_get_data()
#'

mdi_get_data <- function(data){
  fundamental = getSymbols.FRED('NFCI',env=globalenv())

  behavioral = getSymbols.FRED('UMCSENT',env=globalenv())

}



