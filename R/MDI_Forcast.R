### Add libraries
library(quantmod)

#Create new environment
fundamental_data <- new.env()
behavioral_data  <- new.env()
catalyst_data    <- new.env()

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

#fundamental
csvfiles <- list.files(path = "~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/Fundamental")
for(i in 1:length(csvfiles)) {
  temp = read.csv(paste0("~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/Fundamental/",
                         csvfiles[i]))
  temp$Date = as.Date(temp$Date, format ="%m-%d-%Y")
  temp <- xts(temp[,2, drop = FALSE], order.by = as.Date(temp[,1], format = "%Y-%m-%d"))
  assign(x = paste0('data', i), value = temp, envir = fundamental_data)
}

#behavioral
csvfiles <- list.files(path = "~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/Behavioral")
for(i in 1:length(csvfiles)) {
  temp = read.csv(paste0("~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/Behavioral/",
                         csvfiles[i]))
  temp$Date = as.Date(temp$Date, format ="%m-%d-%Y")
  temp <- xts(temp[,2, drop = FALSE], order.by = as.Date(temp[,1], format = "%Y-%m-%d"))
  assign(x = paste0('data', i), value = temp, envir = behavioral_data)
}

#catalyst
csvfiles <- list.files(path = "~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/Catalyst")
for(i in 1:length(csvfiles)) {
  temp = read.csv(paste0("~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/Catalyst/",
                         csvfiles[i]))
  temp$Date = as.Date(temp$Date, format ="%m-%d-%Y")
  temp <- xts(temp[,2, drop = FALSE], order.by = as.Date(temp[,1], format = "%Y-%m-%d"))
  assign(x = paste0('data', i), value = temp, envir = catalyst_data)
}


### Merge data - this will store it in a list
data_fundamental <- eapply(env = fundamental_data, FUN = merge.xts)
data_behavioral  <- eapply(env = behavioral_data, FUN = merge.xts)
data_catalyst    <- eapply(env = catalyst_data, FUN = merge.xts)

#clean-up
rm(temp)
rm(i)


## Check Periodicities
# Fundamental
ls_fun_periodicities <- lapply(data_fundamental, periodicity)
fun_periodicities <- as.data.frame(do.call(rbind, lapply(data_fundamental, periodicity)))
fun_periodicities$start <- as.Date(sapply(ls_fun_periodicities, "[[", 3))
fun_periodicities$end <- as.Date(sapply(ls_fun_periodicities, "[[", 4))
fun_periodicities

# Behavioral
ls_beh_periodicities <- lapply(data_behavioral, periodicity)
beh_periodicities <- as.data.frame(do.call(rbind, lapply(data_behavioral, periodicity)))
beh_periodicities$start <- as.Date(sapply(ls_beh_periodicities, "[[", 3))
beh_periodicities$end <- as.Date(sapply(ls_beh_periodicities, "[[", 4))
beh_periodicities

# Catalyst
ls_cat_periodicities <- lapply(data_catalyst, periodicity)
cat_periodicities <- as.data.frame(do.call(rbind, lapply(data_catalyst, periodicity)))
cat_periodicities$start <- as.Date(sapply(ls_cat_periodicities, "[[", 3))
cat_periodicities$end <- as.Date(sapply(ls_cat_periodicities, "[[", 4))
cat_periodicities


## Convert into Monthly
data_fin_1 <- lapply(data_fundamental, to.monthly, OHLC=FALSE)
data_fin_2 <- lapply(data_behavioral, to.monthly, OHLC=FALSE)
data_fin_3 <- lapply(data_catalyst, to.monthly, OHLC=FALSE)

#clean-up
rm(data_fundamental)
rm(data_behavioral)
rm(data_catalyst)

#merge data
xts_data_fun <- do.call(merge.xts, data_fin_1)
xts_data_fun <- na.locf(xts_data_fun, na.rm = TRUE)

xts_data_beh <- do.call(merge.xts, data_fin_2)
xts_data_beh <- na.locf(xts_data_beh, na.rm = TRUE)

xts_data_cat <- do.call(merge.xts, data_fin_3)
xts_data_cat <- na.locf(xts_data_cat, na.rm = TRUE)


## Align Data
xts_data_fun <- xts_data_fun["2007-12/"]
xts_data_beh <- xts_data_beh["2007-12/"]
xts_data_cat <- xts_data_cat["2007-12/"]


#clean-up
rm(data_fin_1)
rm(data_fin_2)
rm(data_fin_3)


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

  DiffIdx <- xts(DF, index(x1))
}

#Constructing Diffusion Index

#fundamental
fundamental_DI <- DiffIdx(xts_data_fun, orig = nrow(xts_data_fun), m = 1)
head(fundamental_DI)

#behavioral
behavioral_DI <- DiffIdx(xts_data_beh, orig = nrow(xts_data_beh), m = 1)
head(behavioral_DI)

#catalyst
catalyst_DI <- DiffIdx(xts_data_cat, orig = nrow(xts_data_cat), m = 1)
head(catalyst_DI)


## Plot DI(s)
plot_data <- cbind(fundamental_DI, behavioral_DI, catalyst_DI)
plot(plot_data, main = "Diffusion Index Plot")
addLegend("topright", on=1,
          legend.names = c("Fundamental DI", "Behavioral DI", "Catalyst DI"),
          lty=c(1, 1), lwd=c(2, 1))


#Scaling & Standardizing the observation
library(roll)
x <- plot_data # we can use plot_data...it is complete
x$DI <- rowMeans(coredata(plot_data))
scaled_DI <- roll_scale(x, width = 5, na_restore = FALSE)


# Plot Scaled DI(s)
plot(scaled_DI, main = "Diffusion Index Plot - scaled")
addLegend("topright", on=1,
          legend.names = c("Fundamental DI", "Behavioral DI", "Catalyst DI"),
          lty=c(1, 1), lwd=c(2, 1))


## Getting S&P 500 Data
SP_500 <- getSymbols (Symbols = 'SPY', src= 'yahoo', auto.assign = FALSE)

library(PerformanceAnalytics)
SP_500_period <- Cl(to.monthly(SP_500))

# We get data starting Jan 2007 for the S&P 500, so we remove everything before Dec 2007, the start of the Diffusion Indexes
SP_500_period <- SP_500_period["2007-12/"]
#Calculate S&P Returns
x$SPR <- Return.calculate(SP_500_period)

SPDI <- cbind(x$DI,x$SPR)
# Plot DI vs S&P 500
plot.default(x$DI,x$SPR)


# S&P Z-score Calculation (For reference - not necessary)
x2 <- SP_500_quart$SP500
roll_scale(x2, width = 5, na_restore = FALSE)

## Using SWfore()
require(MTS)
results <- SWfore(y = trim_data$UMCSENT, x = trim_data[,-c(1)], orig = length(idx_complete)-2, m = 5)
# results <- SWfore(y = trim_data$UMCSENT, x = trim_data[,-1], orig = length(idx_complete)-1, m = 5)
cbind(trim_data$UMCSENT[(length(idx_complete)-2+1):length(idx_complete)], results$yhat)



## Using Machine Learning

lag_order <- 6 # the desired number of lags (six months)
horizon <- 12 # the forecast horizon (twelve months)

y_train <- SPDI$SPR["2008-01/2020-12"] # the target
X_train <- SPDI$DI["2008-01/2020-12"] # everything but the target

y_test <- window(SPDI$SPR["2021-01/2021-08"])
X_test <- window(SPDI$DI["2021-01/2021-08"])

forecasts_rf <- numeric(horizon)
for (i in 1:horizon){
  # set seed
  set.seed(2019)
  # fit the model
  fit_rf <- randomForest(X_train, y_train)
  # predict using the test set
  forecasts_rf[i] <- predict(fit_rf, X_test)
  # here is where we repeatedly reshape the training data to reflect the time distance
  # corresponding to the current forecast horizon.
  y_train <- y_train[-1]
  X_train <- X_train[-nrow(X_train), ]
}



DI_lagged <- do.call(cbind, lapply(diff.lookbacks, function(n) lag(SPDI$DI, k = n)))
SPR_lagged <- do.call(cbind, lapply(diff.lookbacks, function(n) lag(SPDI$SPR, k = n)))

library(caret)
require(ranger)
set.seed(123)

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 36,
                              horizon = 12,
                              fixedWindow = FALSE,
                              allowParallel = TRUE,
                              verboseIter = TRUE)
# seeds = seeds)
train.y <- factor(SPDI$SPR)

index <- createDataPartition(SPDI$DI, p=0.80, list=FALSE)
trainSet <- SPDI[ index,]
testSet  <- SPDI[-index,]

tuneLength.num <- 5
mod.ranger <- train(SPDI$SPR,
                    SPDI$DI,
                    method = "ranger",
                    trControl = myTimeControl,
                    tuneLength=tuneLength.num)
plot(mod.ranger)

# Lag x values, build x_train and y_train
lag_x <- lag.xts(x$DI)
x_train <- lag_x[2:(nrow(x$DI)-2),] # train goes from 32 obs to 29, 1 lag and 2 obs for prediction
colnames(x_train) <- c("DI")
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


