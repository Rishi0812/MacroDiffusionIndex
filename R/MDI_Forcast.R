### Add libraries
library(quantmod)

#Create new environment
econ_data <- new.env()
csv_data <- new.env()

### Add symbols
symbols <- c('AMTMNO', 'AMTMTI', 'OECDLOLITOAASTSAM', 'ICSA', 'INDPRO', 'T10Y2Y', 'BAA10Y', 'MKTGDPHKA646NWDB', 'MKTGDPSGA646NWDB', 'RGDPNABRA666NRUG', # Economic Trend
             'NFCI', # Liquidity
             'M2V', 'TOTCI', 'BOGZ1FA895050005Q', # Velocity
             'UMCSENT', 'CSUSHPISA', 'SPCS20RSA', # Confirmation Bias: Surveys
             'VIXCLS', 'VXVCLS', 'EVZCLS', 'THREEFYTP10', # Representative Bias
             'EMVOVERALLEMV', # Cognitive Dissonance
             'BOGZ1FA483064105Q', # Overconfidence
             'CFNAIMA3', 'STLENI', 'STLFSI2', # Economic Surprise
             'WLEMUINDXD' # Geopolitics
)


### Get data
getSymbols(Symbols = symbols,
           src='FRED',
           env = econ_data)

rm(symbols)


## Get CSV Data

# csvfiles <- list.files(path = "~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data")
# for(i in 1:length(csvfiles)) {
#   assign(paste0('data', i),
#          read.csv2(paste0("~/Documents/Rishi/GSoC_2021/Bloomberg-Dataset/GSOC_macro_Bloomberg_data/",
#                           csvfiles[i])))
# }

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


## Check Periodicities
periodicities <- as.data.frame(do.call(rbind, lapply(data, periodicity)))
#View(periodicities)
ls_periodicities <- lapply(data, periodicity)
periodicities$start <- as.Date(sapply(ls_periodicities, "[[", 3))
periodicities$end <- as.Date(sapply(ls_periodicities, "[[", 4))
periodicities # view periodicities
rm(ls_periodicities)


data_fin <- lapply(data, to.quarterly, OHLC=FALSE)
data_fin[["STLENI"]] <- NULL

xts_data_quart <- do.call(merge.xts, data_fin)
rm(data_fin)
idx_complete <- which(complete.cases(xts_data_quart) == TRUE)
trim_data = xts_data_quart[idx_complete]
rm(xts_data_quart)


#xts_data_quart = xts_data_quart["1974-12-01/2020-12-01"]

#Plot, Visualize and get some initial Insights
library(ggplot2)
ggplot(trim_data, aes(x = Index, y = trim_data$UMCSENT)) + geom_line()


## SWfore()
require(MTS)
results <- SWfore(y = trim_data$UMCSENT, x = trim_data[,-c(1)], orig = length(idx_complete)-2, m = 5)
# results <- SWfore(y = trim_data$UMCSENT, x = trim_data[,-1], orig = length(idx_complete)-1, m = 5)
cbind(trim_data$UMCSENT[(length(idx_complete)-2+1):length(idx_complete)], results$yhat)


## SWfore without look-ahead bias
#UMCSENT
results <- SWfore(y = trim_data$UMCSENT[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$UMCSENT)-3, m = 5)
UMCSENT_Forecast <- cbind(trim_data$UMCSENT[(length(trim_data$UMCSENT)-2+1):length(trim_data$UMCSENT)], results$yhat)

#EVZCLS
results <- SWfore(y = trim_data$EVZCLS[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$EVZCLS)-3, m = 5)
EVZCLS_Forecast <- cbind(trim_data$EVZCLS[(length(trim_data$EVZCLS)-2+1):length(trim_data$EVZCLS)], results$yhat)

#OECDLOLITOAASTSAM
results <- SWfore(y = trim_data$OECDLOLITOAASTSAM[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$OECDLOLITOAASTSAM)-3, m = 5)
OECDLOLITOAASTSAM_Forecast <- cbind(trim_data$OECDLOLITOAASTSAM[(length(trim_data$OECDLOLITOAASTSAM)-2+1):length(trim_data$OECDLOLITOAASTSAM)], results$yhat)

#SPCS20RSA
results <- SWfore(y = trim_data$SPCS20RSA[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$SPCS20RSA)-3, m = 5)
SPCS20RSA_Forecast <- cbind(trim_data$SPCS20RSA[(length(trim_data$SPCS20RSA)-2+1):length(trim_data$SPCS20RSA)], results$yhat)

#CFNAIMA3
results <- SWfore(y = trim_data$CFNAIMA3[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_dataCFNAIMA3)-3, m = 5)
CFNAIMA3_Forecast <- cbind(trim_data$CFNAIMA3[(length(trim_data$CFNAIMA3)-2+1):length(trim_data$CFNAIMA3)], results$yhat)

#THREEFYTP10
results <- SWfore(y = trim_data$THREEFYTP10[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$THREEFYTP10)-3, m = 5)
THREEFYTP10_Forecast <- cbind(trim_data$THREEFYTP10[(length(trim_data$THREEFYTP10)-2+1):length(trim_data$THREEFYTP10)], results$yhat)

#MKTGDPHKA646NWDB
results <- SWfore(y = trim_data$MKTGDPHKA646NWDB[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$MKTGDPHKA646NWDB)-3, m = 5)
MKTGDPHKA646NWDB_Forecast <- cbind(trim_data$MKTGDPHKA646NWDB[(length(trim_data$MKTGDPHKA646NWDB)-2+1):length(trim_data$MKTGDPHKA646NWDB)], results$yhat)

#VIXCLS
results <- SWfore(y = trim_data$VIXCLS[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$VIXCLS)-3, m = 5)
VIXCLS_Forecast <- cbind(trim_data$VIXCLS[(length(trim_data$VIXCLS)-2+1):length(trim_data$VIXCLS)], results$yhat)

#BAA10Y
results <- SWfore(y = trim_data$BAA10Y[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$BAA10Y)-3, m = 5)
BAA10Y_Forecast <- cbind(trim_data$BAA10Y[(length(trim_data$BAA10Y)-2+1):length(trim_data$BAA10Y)], results$yhat)

#VXVCLS
results <- SWfore(y = trim_data$VXVCLS[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$VXVCLS)-3, m = 5)
VXVCLS_Forecast <- cbind(trim_data$VXVCLS[(length(trim_data$VXVCLS)-2+1):length(trim_data$VXVCLS)], results$yhat)

#BOGZ1FA483064105Q
results <- SWfore(y = trim_data$BOGZ1FA483064105Q[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$BOGZ1FA483064105Q)-3, m = 5)
BOGZ1FA483064105Q_Forecast <- cbind(trim_data$BOGZ1FA483064105Q[(length(trim_data$BOGZ1FA483064105Q)-2+1):length(trim_data$BOGZ1FA483064105Q)], results$yhat)

#MKTGDPSGA646NWDB
results <- SWfore(y = trim_data$MKTGDPSGA646NWDB[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$MKTGDPSGA646NWDB)-3, m = 5)
MKTGDPSGA646NWDB_Forecast <- cbind(trim_data$MKTGDPSGA646NWDB[(length(trim_data$MKTGDPSGA646NWDB)-2+1):length(trim_data$MKTGDPSGA646NWDB)], results$yhat)

#EMVOVERALLEMV
results <- SWfore(y = trim_data$EMVOVERALLEMV[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$EMVOVERALLEMV)-3, m = 5)
EMVOVERALLEMV_Forecast <- cbind(trim_data$EMVOVERALLEMV[(length(trim_data$EMVOVERALLEMV)-2+1):length(trim_data$EMVOVERALLEMV)], results$yhat)

#AMTMTI
results <- SWfore(y = trim_data$AMTMTI[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$AMTMTI)-3, m = 5)
AMTMTI_Forecast <- cbind(trim_data$AMTMTI[(length(trim_data$AMTMTI)-2+1):length(trim_data$AMTMTI)], results$yhat)

#T10Y2Y
results <- SWfore(y = trim_data$T10Y2Y[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$T10Y2Y)-3, m = 5)
T10Y2Y_Forecast <- cbind(trim_data$T10Y2Y[(length(trim_data$T10Y2Y)-2+1):length(trim_data$T10Y2Y)], results$yhat)

#WLEMUINDXD
results <- SWfore(y = trim_data$WLEMUINDXD[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$WLEMUINDXD)-3, m = 5)
WLEMUINDXD_Forecast <- cbind(trim_data$WLEMUINDXD[(length(trim_data$WLEMUINDXD)-2+1):length(trim_data$WLEMUINDXD)], results$yhat)

#NFCI
results <- SWfore(y = trim_data$NFCI[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$NFCI)-3, m = 5)
NFCI_Forecast <- cbind(trim_data$NFCI[(length(trim_data$NFCI)-2+1):length(trim_data$NFCI)], results$yhat)

#STLFSI2
results <- SWfore(y = trim_data$STLFSI2[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$STLFSI2)-3, m = 5)
STLFSI2_Forecast <- cbind(trim_data$STLFSI2[(length(trim_data$STLFSI2)-2+1):length(trim_data$STLFSI2)], results$yhat)

#AMTMNO
results <- SWfore(y = trim_data$AMTMNO[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$AMTMNO)-3, m = 5)
AMTMNO_Forecast <- cbind(trim_data$AMTMNO[(length(trim_data$AMTMNO)-2+1):length(trim_data$AMTMNO)], results$yhat)

#RGDPNABRA666NRUG
results <- SWfore(y = trim_data$RGDPNABRA666NRUG[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$RGDPNABRA666NRUG)-3, m = 5)
RGDPNABRA666NRUG_Forecast <- cbind(trim_data$RGDPNABRA666NRUG[(length(trim_data$RGDPNABRA666NRUG)-2+1):length(trim_data$RGDPNABRA666NRUG)], results$yhat)

#CSUSHPISA
results <- SWfore(y = trim_data$CSUSHPISA[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$CSUSHPISA)-3, m = 5)
CSUSHPISA_Forecast <- cbind(trim_data$CSUSHPISA[(length(trim_data$CSUSHPISA)-2+1):length(trim_data$CSUSHPISA)], results$yhat)

#TOTCI
results <- SWfore(y = trim_data$TOTCI[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$TOTCI)-3, m = 5)
TOTCI_Forecast <- cbind(trim_data$TOTCI[(length(trim_data$TOTCI)-2+1):length(trim_data$TOTCI)], results$yhat)

#ICSA
results <- SWfore(y = trim_data$ICSA[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$ICSA)-3, m = 5)
ICSA_Forecast <- cbind(trim_data$ICSA[(length(trim_data$ICSA)-2+1):length(trim_data$ICSA)], results$yhat)

#M2V
results <- SWfore(y = trim_data$M2V[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$M2V)-3, m = 5)
M2V_Forecast <- cbind(trim_data$M2V[(length(trim_data$M2V)-2+1):length(trim_data$M2V)], results$yhat)

#BOGZ1FA895050005Q
results <- SWfore(y = trim_data$BOGZ1FA895050005Q[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$BOGZ1FA895050005Q)-3, m = 5)
BOGZ1FA895050005Q_Forecast <- cbind(trim_data$BOGZ1FA895050005Q[(length(trim_data$BOGZ1FA895050005Q)-2+1):length(trim_data$BOGZ1FA895050005Q)], results$yhat)

#INDPRO
results <- SWfore(y = trim_data$INDPRO[-1], x = lag(trim_data[,-c(1)], 1)[-1], orig = length(trim_data$INDPRO)-3, m = 5)
INDPRO_Forecast <- cbind(trim_data$INDPRO[(length(trim_data$INDPRO)-2+1):length(trim_data$INDPRO)], results$yhat)


## Using SWFore to build Diffusion Indexes
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

diffIdx <- DiffIdx(trim_data[,-c(1)], orig = length(idx_complete)-2, m = 5)
head(diffIdx)


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


