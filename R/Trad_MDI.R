### Add libraries
library(quantmod)

#Create new environment
econ_data <- new.env()
csv_data <- new.env()

### Add symbols
symbols <- c('OECDLOLITOAASTSAM', 'ICSA', 'INDPRO', 'T10Y2Y', 'BAA10Y', # Economic Trend
             'NFCI', # Liquidity
             'M2V', 'TOTCI', 'BOGZ1FA895050005Q', # Velocity
             'UMCSENT', 'CSUSHPISA', 'SPCS20RSA', # Confirmation Bias: Surveys
             'VIXCLS', 'VXVCLS', 'EVZCLS', 'THREEFYTP10', # Representative Bias
             'EMVOVERALLEMV', # Cognitive Dissonance
             'CFNAIMA3', 'STLENI', 'STLFSI2', # Economic Surprise
             'WLEMUINDXD' # Geopolitics
)


### Get data
getSymbols(Symbols = symbols,
           src='FRED',
           env = econ_data)

## Get CSV Data

csvfiles <- list.files(path = "~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data")
for(i in 1:length(csvfiles)) {
  assign(paste0('data', i),
         read.csv2(paste0("~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/",
                          csvfiles[i])))
}

# dat <- read.csv("~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/GSOC_20dayPutCall.csv")
#
# dat2 <- dat[-c(1:5), ]
# colnames(dat2)[1] <- "Date"
# colnames(dat2)[2] <- "PX_LAST"
#
# dat2$Date<-strftime(strptime(dat2$Date,"%m/%d/%Y"),"%Y-%m-%d")
# PutCall <- xts(dat2[,2], as.Date(dat2[,1], format = "%Y-%m-%d", env = econ_data))
#
# rm(dat,dat2)



### Merge data - this will store it in a list
data <- eapply(env = econ_data, FUN = merge.xts)


# eapply(econ_data, start)
# eapply(econ_data, end)
# eapply(econ_data, frequency)

data_fin <- lapply(data, to.quarterly, OHLC=FALSE)

xts_data_quart <- do.call(merge.xts, data_fin)

xts_data_quart = xts_data_quart["1974-12-01/2020-12-01"]


### At this point, you may want to merge the data by timestamp, not sure.
### Alternative is to keep them separate, build the lags, and merge later after
### reviewing/solving for the periodicities

# insert code here, including filling NAs

### Add lags
diff.lookbacks <- 1:10 # Lets use 10 lags, but this will vary based on periodicity of the dataset, unless we normalize the data to equal periodicities first

# Below is an example of adding lags...you will inevitably want to wrap this in a for loop, perhaps using eapply
example_lags <- do.call(cbind, lapply(diff.lookbacks, function(n) lag(econ_data$BAA10Y, k = n)))

head(example_lags, 20)
