library(lubridate)
library(tidyverse)
library(forecast)
google <- read.csv("https://raw.githubusercontent.com/nbadino/gtrend-inflation/main/google.csv")
istat<-read.csv("https://raw.githubusercontent.com/nbadino/gtrend-inflation/main/istat.csv")
CPI <-read.csv("https://raw.githubusercontent.com/nbadino/gtrend-inflation/main/CPI.csv")
istat[196:196,2:8]<-(istat[195:195,2:8]+istat[197:197,2:8])/2
head(google)
delta_google <- (google$inflazione...Italia.*2-google$deflazione...Italia.)^2
STD <- function (x) {
  std <-((x- mean(x))/sd(x))
  return(std)
}
DATA=data.frame(data=ym(google$Mese), google=log(2+STD(delta_google)))

#prova <- ts(DATA$google, start=2010,end = 2021, 12)
#prova
#####
ggplot(prova, aes(,y=CPI))+
  geom_line()
###

### test con autoplot
autoplot(prova)+
  xlab("year")
#####
DATA$attese <- STD((istat$saldo[73:209]))
DATA$CPI <- CPI$delta[73:209]*100
#cor(DATA$google,DATA$attese)

ggplot(data=DATA)+
  geom_line(aes(x=data, y=attese, color="BLUE"))+
  geom_line(aes(x=data,y=google, color="Red"))+
  geom_line(aes(x=data,y=CPI, color="Green"))+
  xlab("Dates")+
  ylab("Indexes")
######
ggPacf(DATA$CPI)
ggCcf(DATA$attese,DATA$CPI)
Arima(DATA$CPI,order=c(3,1,1))
auto.arima(DATA$CPI, xreg=DATA$google)
auto.arima(DATA$CPI, xreg=DATA$attese)
modello <- dynlm(ts(DATA$CPI)~
                   L(ts(DATA$CPI),2)+
                   L(ts(DATA$CPI),12))
summary(modello)
modello2 <- dynlm(ts(DATA$CPI)~
                    L(ts(DATA$CPI),2)+
                    L(ts(DATA$CPI),12)+
                    L(ts(DATA$attese),2)+
                    L(ts(DATA$attese),3))
summary(modello2)
train <- window(ts(DATA), end = 108)
test <- window(ts(DATA), start = 109)
covariates <- c("google")
fit <- auto.arima(train[,"CPI"], xreg = train[, covariates])
summary(fit)
#da controllare
fcast <- forecast(fit, xreg = test[, covariates])
plot(fcast)
################
#cpi_ts <- ts(DATA$CPI, start=2010,end = 2021, 12)
#google_ts <- ts(DATA$google, start=2010, end=2021, 12)
#aspettative_ts <- ts(DATA$attese, start=2010,end = 2021, 12)

train <- window(ts(DATA), end = 126)
test <- window(ts(DATA), start = 127, end=132)

## model CPI ##
autoplot(train[,"CPI"])+
  xlab("year")
ggAcf(train[,"CPI"])
ggPacf(train[,"CPI"])
modello_cpi <- auto.arima(train[,"CPI"])
summary(modello_cpi)
checkresiduals(modello_cpi)
fcast_cpi <- forecast(modello_cpi, h=27)
autoplot(fcast_cpi)
summary(fcast_cpi)
#modello_cpi <- Arima(cpi_ts,order = c(2,0,0))
#summary(modello_cpi)

## model GI ##

regressori_gi <- cbind(
  lag_1 <- lag(DATA$google,1),
  lag_2 <- lag(DATA$google,2))
DATA$google_lag1 <- regressori_gi[,1]
DATA$google_lag2 <- regressori_gi[,2]
DATA <- DATA[3:137,]
train <- window(ts(DATA), end = 126)
test <- window(ts(DATA), start = 127, end= 132)

covariates <- c("google_lag1", "google_lag2")
modello_gi <- auto.arima(train[,"CPI"], xreg =train[,covariates])
summary(modello_gi)
checkresiduals(modello_gi)
fcast_gi <- forecast(modello_gi, xreg = test[,covariates])
autoplot(fcast_gi)
summary(fcast_gi)
#accuracy(fcast_gi, x=test)


### modello attese
regressori_attese <- cbind(
  lag_1 <- lag(DATA$attese,1),
  lag_2 <- lag(DATA$attese,2))
DATA$attese_lag1 <- regressori_attese[2:136,1]
DATA$attese_lag2 <- regressori_attese[3:137,2]
covariates2 <- c("attese_lag1", "attese_lag2")
train <- window(ts(DATA), end = 126)
test <- window(ts(DATA), start = 127, end= 132)

modello_aspettative <- auto.arima(train[,"CPI"],xreg =train[,covariates2])
summary(modello_aspettative)
checkresiduals(modello_aspettative)
fcast_aspettative <- forecast(modello_aspettative, xreg = test[,covariates2])
autoplot(fcast_aspettative)
summary(fcast_aspettative)
#plot(forecast(modello_1,xreg=cpi_ts))
#prova <- forecast(modello_cpi, h=27)
#autoplot(prova)
#summary(prova)


accuracy(prova, x=test)
