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

##Fundamental

  #RELATIVE VALUATION

  #ECONOMIC TREND
  getSymbols.FRED('OECDLOLITOAASTSAM',env=globalenv())
  getSymbols.FRED('ICSA',env=globalenv())
  getSymbols.FRED('INDPRO',env=globalenv())
  getSymbols.FRED('T10Y2Y',env=globalenv())
  getSymbols.FRED('BAA10Y',env=globalenv())

  #LIQUIDITY
  getSymbols.FRED('NFCI',env=globalenv())

  #VELOCITY
  getSymbols.FRED('M2V',env=globalenv())
  getSymbols.FRED('TOTCI',env=globalenv())
  getSymbols.FRED('M0263AUSM500NNBR',env=globalenv())
  getSymbols.FRED('M0264AUSM500NNBR',env=globalenv())
  getSymbols.FRED('BOGZ1FA895050005Q',env=globalenv())

##BEHAVIORAL

  #CONFIRMATION BIAS: SURVEYS
  getSymbols.FRED('UMCSENT',env=globalenv())
  getSymbols.FRED('CSUSHPISA',env=globalenv())
  getSymbols.FRED('SPCS20RSA',env=globalenv())

  #REPRESNTATIVENESS BIAS: OPTIONS MARKET
  getSymbols.FRED('VIXCLS',env=globalenv())
  getSymbols.FRED('VXVCLS',env=globalenv())
  getSymbols.FRED('EVZCLS',env=globalenv())
  getSymbols.FRED('THREEFYTP10',env=globalenv())

  #COGNITIVE DISSONANCE
  getSymbols.FRED('EMVOVERALLEMV',env=globalenv())

  #LOSS AVERSION

  #ANCHORING BIAS

  #FRAMING BIAS

  #OVERCONFIDENCE


##CATALYST

  #EARNINGS SURPRISE

  #ECONOMIC SURPRISE
  getSymbols.FRED('CFNAIMA3',env=globalenv())
  getSymbols.FRED('STLENI',env=globalenv())
  getSymbols.FRED('STLFSI2',env=globalenv())

  #GEOPOLITICS
  getSymbols.FRED('WLEMUINDXD',env=globalenv())

}



