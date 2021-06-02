library(lubridate)
library(tidyverse)
library(forecast)
google <- read.csv(file="Documents/r/google.csv")
istat<-read.csv(url("https://raw.githubusercontent.com/nbadino/gtrend-inflation/main/istat.csv"))
CPI <-read.csv("https://raw.githubusercontent.com/nbadino/gtrend-inflation/main/CPI.csv")
istat[196:196,2:8]<-(istat[195:195,2:8]+istat[197:197,2:8])/2
head(google)
delta_google <- google$inflazione...Italia.-google$deflazione...Italia.
STD <- function (x) {
  std <-((x- mean(x))/sd(x))
  return(std)
}
DATA=data.frame(data=ym(google$Mese), google=STD(google$inflazione...Italia.-google$deflazione...Italia.))
ggplot(DATA, aes(x=data,y=CPI))+
  geom_line()
DATA$attese <- STD((istat$saldo[73:209]))
DATA$CPI <- CPI$delta[73:209]*100
cor(DATA$google,DATA$attese)
ggplot(data=DATA)+
  geom_line(aes(x=data, y=attese, color="BLUE"))+
  geom_line(aes(x=data,y=google, color="Red"))+
  geom_line(aes(x=data,y=CPI, color="Green"))+
  xlab("Dates")+
  ylab("Indexes")
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
fcast <- forecast(fit, xreg = test[, covariates])
plot(fcast)
