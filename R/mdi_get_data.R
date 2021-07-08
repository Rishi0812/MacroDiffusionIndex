#'@title  Function to parse economic data
#'
#'@description The mdi_get_data() function currently retrieves data from FRED.
#'
#'  The 'default' series includes a range of indicators from Fundamental to
#'  Behavioral and Catalyst categories. The sub-categories and their respective
#'  tickers and descriptions are described in the Details section. Users can
#'  specify their own character of symbols TODO - add
#'  descriptions next to ticker
#'
#'@param symbols Character vector of symbol names to download. Default: NULL, which will return all the symbols described in @details.
#'
#'@details
#'
#'  The sub-categories and their respective tickers and descriptions are
#'  described below.
#'
#'  | FUNDAMENTAL | |
#'  | --- | --- |
#'  | Economic Trend: | |
#'  | OECDLOLITOAASTSAM | TODO: add description |
#'  | ICSA | |
#'  | INDPRO | |
#'  | T10Y2Y | |
#'  | BAA10Y | |
#'  | --- | --- |
#'  | Liquidity: | |
#'  | NFCI | |
#'  | --- | --- |
#'  | Velocity: | |
#'  | M2V | |
#'  | TOTCI | |
#'  | M0263AUSM500NNBR | |
#'  | M0264AUSM500NNBR | |
#'  | BOGZ1FA895050005Q | |
#'  | --- | --- |
#'  | --- | --- |
#'  | BEHAVIORAL | |
#'  | Confirmation Bias: Surveys | |
#'  | UMCSENT | |
#'  | CSUSHPISA | |
#'  | SPCS20RSA | |
#'  | --- | --- |
#'  | Representative Bias: Options Market | |
#'  | VIXCLS | |
#'  | VXVCLS | |
#'  | EVZCLS | |
#'  | THREEFYTP10 - | |
#'  | --- | --- |
#'  | Cognitive Dissonance: | |
#'  | EMVOVERALLEMV | |
#'  | --- | --- |
#'  | Loss Aversion: | |
#'  | --- | --- |
#'  | Anchoring Bias: | |
#'  | --- | --- |
#'  | Framing Bias: | |
#'  | --- | --- |
#'  | Overconfidence: | |
#'  | --- | --- |
#'  | --- | --- |
#'  | CATALYST | |
#'  | Earnings Surprise: | |
#'  | --- | --- |
#'  | Economic Surprise: | |
#'  | CFNAIMA3 | |
#'  | STLENI | |
#'  | STLFSI2 | |
#'  | --- | --- |
#'  | Geopolitics: | |
#'  | WLEMUINDXD | |
#'
#'@return Applicable time series data from FRED, stored as xts thanks to the
#'  internal getSymbols.FRED function. Data to be stored in a new environment called econ_data.
#'@export
#'
#'@import quantmod
#'
#' @examples
#' \dontrun{
#' mdi_get_data()
#' head(econ_data$ICSA)
#' }
#'

mdi_get_data <- function(symbols = NULL){

  econ_data <- new.env()

  if(is.null(symbols)) {
    symbols <- c('OECDLOLITOAASTSAM', 'ICSA', 'INDPRO', 'T10Y2Y', 'BAA10Y', # Economic Trend
                 'NFCI', # Liquidity
                 'M2V', 'TOTCI', 'M0263AUSM500NNBR', 'M0264AUSM500NNBR', 'BOGZ1FA895050005Q', # Velocity
                 'UMCSENT', 'CSUSHPISA', 'SPCS20RSA', # Confirmation Bias: Surveys
                 'VIXCLS', 'VXVCLS', 'EVZCLS', 'THREEFYTP10', # Representative Bias
                 'EMVOVERALLEMV', # Cognitive Dissonance
                 'CFNAIMA3', 'STLENI', 'STLFSI2', # Economic Surprise
                 'WLEMUINDXD' # Geopolitics
                 )
  }

  getSymbols(Symbols = symbols, src='FRED', env = econ_data)

}



