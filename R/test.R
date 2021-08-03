### Add libraries
library(quantmod)

#Create new environment
test_data <- new.env()

### Get data
getSymbols(Symbols = "ICSA",
           src='FRED',
           env = test_data)

eapply(test_data, start)
eapply(test_data, end)
eapply(test_data, frequency)

test_data$ICSA = test_data$ICSA["1974-12-01/2020-12-01"]
test_data$ICSA <- to.monthly(test_data$ICSA)
dat <- test_data$ICSA[, -c(1:3)]
ICSA<-ts(dat, end=c(2020,4),freq=4)

test <- merge(data$BAA10Y, data$BOGZ1FA895050005Q, data$CFNAIMA3,
              data$CSUSHPISA, data$EMVOVERALLEMV, data$EVZCLS)

mean(ICSA)
locf
