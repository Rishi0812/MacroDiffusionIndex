### Add libraries
library(quantmod)

#Create new environment
fundamental_data <- new.env()
behavioral_data <- new.env()
catalyst_data <- new.env()

### Add symbols
symbols1 <- c('AMTMNO', 'AMTMTI', 'OECDLOLITOAASTSAM', 'ICSA', 'INDPRO', 'T10Y2Y', 'BAA10Y', 'MKTGDPHKA646NWDB', 'MKTGDPSGA646NWDB', 'RGDPNABRA666NRUG', # Economic Trend
             'NFCI', # Liquidity
             'M2V', 'TOTCI', 'BOGZ1FA895050005Q' # Velocity
             )

symbols2 <- c('UMCSENT', 'CSUSHPISA', 'SPCS20RSA', # Confirmation Bias: Surveys
              'VIXCLS', 'VXVCLS', 'EVZCLS', 'THREEFYTP10', # Representative Bias
              'EMVOVERALLEMV', # Cognitive Dissonance
              'BOGZ1FA483064105Q' # Overconfidence
              )

symbols3 <- c('CFNAIMA3', 'STLFSI2', # Economic Surprise
              'WLEMUINDXD' # Geopolitics
              )

### Get data
getSymbols(Symbols = symbols1,
           src='FRED',
           env = fundamental_data)

getSymbols(Symbols = symbols2,
           src='FRED',
           env = behavioral_data)

getSymbols(Symbols = symbols3,
           src='FRED',
           env = catalyst_data)

rm(symbols1)
rm(symbols2)
rm(symbols3)

## Get CSV Data

fundamental_csv <- new.env()
behavioral_csv <- new.env()
catalyst_csv <- new.env()

#fundamental
csvfiles <- list.files(path = "~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data")
for(i in 1:length(csvfiles)) {
  temp = read.csv(paste0("~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/",
                      csvfiles[i]))
  assign(x = paste0('data', i), value = temp, envir = fundamental_csv)
}

#behavioral
csvfiles <- list.files(path = "~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data")
for(i in 1:length(csvfiles)) {
  temp = read.csv(paste0("~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/",
                         csvfiles[i]))
  assign(x = paste0('data', i), value = temp, envir = behavioral_csv)
}

#catalyst
csvfiles <- list.files(path = "~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data")
for(i in 1:length(csvfiles)) {
  temp = read.csv(paste0("~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/",
                         csvfiles[i]))
  assign(x = paste0('data', i), value = temp, envir = catalyst_csv)
}

#Data pre-processing
data2 <- eapply(env = csv_data, FUN = merge.zoo)
data_2 <- do.call(merge.zoo, data2)

data_fin0 <- lapply(data2, strftime(strptime(data2$Date,"%m/%d/%Y"),"%Y-%m-%d"))
# dat <- read.csv("~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/GSOC_20dayPutCall.csv")
#
# dat2 <- dat[-c(1:5), ]
# colnames(dat2)[1] <- "Date"
# colnames(dat2)[2] <- "PX_LAST"
#
# dat2$Date<- strftime(strptime(dat2$Date,"%m/%d/%Y"),"%Y-%m-%d")
# PutCall <- xts(dat2[,2], as.Date(dat2[,1], format = "%Y-%m-%d", env = econ_data))
#
# rm(dat,dat2)

data2$Date<- strftime(strptime(data2$Date,"%m/%d/%Y"),"%Y-%m-%d")
PutCall <- xts(data2[,2], as.Date(dat2[,1], format = "%Y-%m-%d", env = econ_data))

#as.Date(data2$Date, format="%m-%d-%Y")
#function(x) as.Date(x[,1], format="%m-%d-%Y")

### Merge data - this will store it in a list
data_fundamental <- eapply(env = fundamental_data, FUN = merge.xts)
data_behavioral  <- eapply(env = behavioral_data, FUN = merge.xts)
data_catalyst    <- eapply(env = catalyst_data, FUN = merge.xts)

## Check Periodicities
periodicities <- as.data.frame(do.call(rbind, lapply(data, periodicity)))
#View(periodicities)
ls_periodicities <- lapply(data, periodicity)
periodicities$start <- as.Date(sapply(ls_periodicities, "[[", 3))
periodicities$end <- as.Date(sapply(ls_periodicities, "[[", 4))
periodicities # view periodicities
rm(ls_periodicities)

#Convert into quaterly
data_fin_1 <- lapply(data_fundamental, to.quarterly, OHLC=FALSE)
data_fin_2 <- lapply(data_behavioral, to.quarterly, OHLC=FALSE)
data_fin_3 <- lapply(data_catalyst, to.quarterly, OHLC=FALSE)

#clean-up
rm(data_fundamental)
rm(data_behavioral)
rm(data_catalyst)

#merge data
xts_data_quart_1 <- do.call(merge.xts, data_fin_1)
xts_data_quart_2 <- do.call(merge.xts, data_fin_2)
xts_data_quart_3 <- do.call(merge.xts, data_fin_3)

#clean-up
rm(data_fin_1)
rm(data_fin_2)
rm(data_fin_3)

#Include complete data
idx_complete_1 <- which(complete.cases(xts_data_quart_1) == TRUE)
idx_complete_2 <- which(complete.cases(xts_data_quart_2) == TRUE)
idx_complete_3 <- which(complete.cases(xts_data_quart_3) == TRUE)

#Final trimed Datasets
trim_data_fundamental = xts_data_quart_1[idx_complete_1]
trim_data_behavioral = xts_data_quart_2[idx_complete_2]
trim_data_catalyst = xts_data_quart_3[idx_complete_3]

#clean-up
rm(xts_data_quart_1)
rm(xts_data_quart_2)
rm(xts_data_quart_3)


#xts_data_quart = xts_data_quart["1974-12-01/2020-12-01"]

#Plot, Visualize and get some initial Insights
library(ggplot2)
ggplot(trim_data, aes(x = Index, y = trim_data$UMCSENT)) + geom_line()
autoplot(facet)

## SWfore()
require(MTS)
results <- SWfore(y = trim_data$UMCSENT, x = trim_data[,-c(1)], orig = length(idx_complete)-2, m = 5)
# results <- SWfore(y = trim_data$UMCSENT, x = trim_data[,-1], orig = length(idx_complete)-1, m = 5)
cbind(trim_data$UMCSENT[(length(idx_complete)-2+1):length(idx_complete)], results$yhat)

## Using SWFore to build Diffusion Indexes - Function
"DiffIdx" <- function(x,orig,m){
  ### Builds Stock and Watson's diffusion index prediction
  ### x: observed regressors
  ### orig: forecast origin
  ### m: selected number of PCs
  ###
  ### Output: Forecasts and MSE of forecasts (if data available)
  if(!is.matrix(x))x=as.matrix(x)
  nT=dim(x)[1]
  k=dim(x)[2]
  if(orig > nT)orig=nT
  if(m > k)m=k; if(m < 1)m=1
  # standardize the predictors
  x1=x[1:orig,]
  me=apply(x1,2,mean)
  se=sqrt(apply(x1,2,var))
  x1=x
  for (i in 1:k){
    x1[,i]=(x1[,i]-me[i])/se[i]
  }
  #
  V1=cov(x1[1:orig,])
  m1=eigen(V1)
  sdev=m1$values
  M=m1$vectors
  M1=M[,1:m]
  Dindex=x1%*%M1
  # y1=y[1:orig]
  # DF=Dindex[1:orig,]
  DF=Dindex
  # mm=lm(y1~DF)
  # coef=matrix(mm$coefficients,(m+1),1)
  # coef=matrix(mm$coefficients[-1],(m),1) # exclude the intercept
  #cat("coefficients: ","\n")
  #print(round(coef,4))
  yhat=NULL; MSE=NULL
  # if(orig < nT){
  #    newx=cbind(rep(1,(nT-orig)),Dindex[(orig+1):nT,])
  #    yhat=mm$coefficients[1]+(t(newx)%*%coef)
  #    err=y[(orig+1):nT]-yhat
  #    MSE=mean(err^2)
  #    cat("MSE of out-of-sample forecasts: ",MSE,"\n")
  # }

  DiffIdx <- DF
}

#Constructing Diffusion Index

#fundamental
fundamental_DI <- DiffIdx(trim_data_fundamental, orig = length(idx_complete_1)-2, m = 1)
head(fundamental_DI)

#behavioral
behavioral_DI <- DiffIdx(trim_data_behavioral, orig = length(idx_complete_2)-2, m = 1)
head(behavioral_DI)

#catalyst
catalyst_DI <- DiffIdx(trim_data_catalyst, orig = length(idx_complete_3)-2, m = 1)
head(catalyst_DI)


## Getting S&P 500 Data
SP_500 <- getSymbols (Symbols = 'SP500', src= 'FRED', auto.assign = FALSE)
SP_500_quart <- to.quarterly(SP_500, )
SP_500_quart <- lapply (SP_500, to.quarterly, OHLC=FALSE)

## Using Random Forest

require(ranger)
set.seed(123)

# Lag x values, build x_train and y_train
lag_x <- lag.xts(diffIdx)
x_train <- lag_x[2:(nrow(diffIdx)-2),] # train goes from 32 obs to 29, 1 lag and 2 obs for prediction
colnames(x_train) <- c("DI_1","DI_2","DI_3","DI_4", "DI_5")
# nrow(x_train)
y_train <- trim_data$UMCSENT[2:(nrow(trim_data)-2)]
# nrow(y_train)

# Check lagged x and y values
MDI_train <- cbind(y_train, x_train)

x_test <- lag_x[(nrow(diffIdx)-1):nrow(diffIdx),]
colnames(x_test) <- c("DI_1","DI_2","DI_3","DI_4","DI_5")
# nrow(x_test)
y_test <- trim_data$UMCSENT[(nrow(trim_data)-1):nrow(trim_data)]
# nrow(y_test)

MDI_test <- cbind(x_test, y_test)
MDI_test

MDI_ranger <- ranger(
  formula   = UMCSENT ~ .,
  data      = MDI_train,
  num.trees = 500,
  mtry      = 5
)


# Compare lagged and un-lagged datasets
head(cbind(MDI_train, cbind(trim_data$UMCSENT, diffIdx)))
tail(cbind(MDI_train, cbind(trim_data$UMCSENT, diffIdx)))


# Forecast
pred_ranger <- predict(MDI_ranger, MDI_test)
head(pred_ranger$predictions)

err <- y_test - pred_ranger$predictions
MSE <- mean(err^2)
MSE

cbind(y_test, pred_ranger$predictions)

## Look-ahead Ranger
x_train <- diffIdx[1:(nrow(diffIdx)-3),] # train goes from 32 obs to 29, 1 lag and 2 obs for prediction
colnames(x_train) <- c("DI_1","DI_2","DI_3","DI_4", "DI_5")
# nrow(x_train)
y_train <- trim_data$UMCSENT[1:(nrow(trim_data)-3)]
# nrow(y_train)

x_test <- diffIdx[(nrow(diffIdx)-2):(nrow(diffIdx)-1),]
colnames(x_test) <- c("DI_1","DI_2","DI_3","DI_4","DI_5")
# nrow(x_test)
y_test <- trim_data$UMCSENT[(nrow(trim_data)-2):(nrow(trim_data)-1)]
# nrow(y_test)

MDI_train <- cbind(x_train, y_train)

set.seed(123)
MDI_ranger <- ranger(
  formula   = UMCSENT ~ .,
  data      = MDI_train,
  num.trees = 500,
  mtry      = 5
)

MDI_test <- cbind(x_test, y_test)

pred_ranger <- predict(MDI_ranger, MDI_test)
head(pred_ranger$predictions)

err <- y_test - pred_ranger$predictions
MSE <- mean(err^2)
MSE

cbind(y_test, pred_ranger$predictions)


