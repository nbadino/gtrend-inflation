library(lubridate)
library(tidyverse)
library(forecast)

# import data
google <- read.csv("https://raw.githubusercontent.com/nbadino/gtrend-inflation/main/google.csv")
istat<-read.csv("https://raw.githubusercontent.com/nbadino/gtrend-inflation/main/istat.csv")
CPI <-read.csv("https://raw.githubusercontent.com/nbadino/gtrend-inflation/main/CPI.csv")

istat[196:196,2:8]<-(istat[195:195,2:8]+istat[197:197,2:8])/2
head(google)
delta_google <- (google$inflazione...Italia.*2-google$deflazione...Italia.)^2

#function for standardization
STD <- function (x) {
  std <-((x- mean(x))/sd(x))
  return(std)
}
# dataframe creation
DATA=data.frame(data=ym(google$Mese), google=log(2+STD(delta_google)))
DATA$attese <- STD((istat$saldo[73:209]))
DATA$CPI <- CPI$delta[73:209]*100

ggplot(data=DATA)+
  geom_line(aes(x=data, y=attese, color="attese"))+
  geom_line(aes(x=data,y=google, color="google"))+
  geom_line(aes(x=data,y=CPI, color="cpi"))+
  xlab("Dates")+
  ylab("Indexes")

# regressor 

regressori_gi <- cbind(
  lag_1 <- lag(DATA$google,1),
  lag_2 <- lag(DATA$google,2))

regressori_attese <- cbind(
  lag_1 <- lag(DATA$attese,1),
  lag_2 <- lag(DATA$attese,2))

DATA$google_lag1 <- regressori_gi[,1]
DATA$google_lag2 <- regressori_gi[,2]

DATA$attese_lag1 <- regressori_attese[,1]
DATA$attese_lag2 <- regressori_attese[,2]

covariates <- c("google_lag1", "google_lag2")
covariates2 <- c("attese_lag1", "attese_lag2")

train <- window(ts(DATA), end = 126)
test <- window(ts(DATA), start = 127, end= 132)


# modello cpi

autoplot(train[,"CPI"])+
  xlab("year")
ggAcf(train[,"CPI"])
ggPacf(train[,"CPI"])
modello_cpi <- auto.arima(train[,"CPI"])
summary(modello_cpi)
checkresiduals(modello_cpi)
fcast_cpi <- forecast(modello_cpi)
autoplot(fcast_cpi)
summary(fcast_cpi)
accuracy(fcast_cpi,test[,"CPI"])

# modello GI

modello_gi <- auto.arima(train[,"CPI"], xreg =train[,covariates])
summary(modello_gi)
checkresiduals(modello_gi)
fcast_gi <- forecast(modello_gi, xreg = test[,covariates])
autoplot(fcast_gi)
summary(fcast_gi)
accuracy(fcast_gi,test[,"CPI"])


# modello aspettative


modello_aspettative <- auto.arima(train[,"CPI"],xreg =train[,covariates2])
summary(modello_aspettative)
checkresiduals(modello_aspettative)
fcast_aspettative <- forecast(modello_aspettative, xreg = test[,covariates2])
autoplot(fcast_aspettative)
summary(fcast_aspettative)
accuracy(fcast_aspettative,test[,"CPI"])
