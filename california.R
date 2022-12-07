# install packages
#install.packages("forecast")
library(forecast)
library(dynlm)

# Load xls file
state <- readxl::read_xls(path = "~/Downloads/State_GDP_2.xls",sheet = 2)

head(state)

state$CANQGSP
plot(state$DATE,state$CANQGSP,main='California GDP',xlab='Year',ylab='GDP')

GDPGR_level <- as.numeric(state$CANQGSP[-1])
GDPGR_lags <- as.numeric(state$CANQGSP[-N])

rates <- (GDPGR_level - GDPGR_lags)/GDPGR_lags
rate_level <- rates[-1]
rate_lags <- rates[-N+1]

arone <- lm(rate_level ~ rate_lags)
arone
# assign GDP growth rate in 2022:Q1
new <- data.frame("rate_lags" = rate_level[N-2])
# forecast GDP growth rate in 2022:Q2
forecast(arone, newdata = new)
forecast
forecast(arone, newdata = new)$mean
forecast(arone, newdata = new)$mean*state$CANQGSP[N-1] + state$CANQGSP[N-1] 
state$CANQGSP[N] 
forecast(arone, newdata = new)$mean*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N] 
(forecast(arone, newdata = new)$mean*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N])^2

# estimate the AR(2) model 
GDPGR_AR2 <- dynlm(ts(rate_level) ~ L(ts(rate_level)) + L(ts(rate_level),2))
# AR(2) forecast of GDP growth in 2022:Q2 
forecast <- c("2022:Q2" = coef(GDPGR_AR2) %*% c(1, rate_level[N-2], rate_level[N-3]))
# compute AR(2) forecast error 
forecast
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] 
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N] 
(forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N])^2

# estimate the AR(3) model 
GDPGR_AR3 <- dynlm(ts(rate_level) ~ L(ts(rate_level)) + L(ts(rate_level),2) +  L(ts(rate_level),3))
# AR(3) forecast of GDP growth in 2022:Q2 
forecast <- c("2022:Q2" = coef(GDPGR_AR3) %*% c(1, rate_level[N-2], rate_level[N-3],rate_level[N-4]))
# compute AR(3) forecast error 
forecast
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] 
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N] 
(forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N])^2

# estimate the AR(4) model 
GDPGR_AR4 <- dynlm(ts(rate_level) ~ L(ts(rate_level)) + L(ts(rate_level),2) +  L(ts(rate_level),3) + L(ts(rate_level),4))
# AR(4) forecast of GDP growth in 2022:Q2 
forecast <- c("2022:Q2" = coef(GDPGR_AR4) %*% c(1, rate_level[N-2], rate_level[N-3],rate_level[N-4],rate_level[N-5]))
# compute AR(4) forecast error 
forecast
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] 
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N] 
(forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N])^2

# estimate the AR(5) model
GDPGR_AR5 <- dynlm(ts(rate_level) ~ L(ts(rate_level)) + L(ts(rate_level),2) + L(ts(rate_level),3) + L(ts(rate_level),4)+L(ts(rate_level),5))
# AR(5) forecast of GDP growth in 2022:Q2 
forecast <- c("2022:Q2" = coef(GDPGR_AR5) %*% c(1, rate_level[N-2], rate_level[N-3], rate_level[N-4], rate_level[N-5],rate_level[N-6]))
# compute AR(5) forecast error 
forecast
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] 
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N] 
(forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N])^2

# estimate the AR(6) model
GDPGR_AR6 <- dynlm(ts(rate_level) ~ L(ts(rate_level)) + L(ts(rate_level),2) + L(ts(rate_level),3) + L(ts(rate_level),4) + L(ts(rate_level),5) + L(ts(rate_level),6))
# AR(6) forecast of GDP growth in 2022:Q2 
forecast <- c("2022:Q2" = coef(GDPGR_AR6) %*% c(1, rate_level[N-2], rate_level[N-3], rate_level[N-4], rate_level[N-5],rate_level[N-6],rate_level[N-7]))
# compute AR(6) forecast error 
forecast
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] 
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N] 
(forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N])^2

# estimate the AR(8) model
GDPGR_AR8 <- dynlm(ts(rate_level) ~ L(ts(rate_level)) + L(ts(rate_level),2) + L(ts(rate_level),3) + L(ts(rate_level),4) + L(ts(rate_level),5) + L(ts(rate_level),6) + L(ts(rate_level),7) + L(ts(rate_level),8))
# AR(8) forecast of GDP growth in 2022:Q2 
forecast <- c("2022:Q2" = coef(GDPGR_AR8) %*% c(1, rate_level[N-2], rate_level[N-3], rate_level[N-4], rate_level[N-5],rate_level[N-6],rate_level[N-7],rate_level[N-8],rate_level[N-9]))
# compute AR(8) forecast error 
forecast
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] 
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N] 
(forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N])^2

# estimate the AR(12) model
GDPGR_AR12 <- dynlm(ts(rate_level) ~ L(ts(rate_level)) + L(ts(rate_level),2) + L(ts(rate_level),3) + L(ts(rate_level),4) + L(ts(rate_level),5) + L(ts(rate_level),6) + L(ts(rate_level),7) + L(ts(rate_level),8) + L(ts(rate_level),9) + L(ts(rate_level),10) + L(ts(rate_level),11) + L(ts(rate_level),12))
# AR(12) forecast of GDP growth in 2022:Q2 
forecast <- c("2022:Q2" = coef(GDPGR_AR12) %*% c(1, rate_level[N-2],rate_level[N-3], rate_level[N-4], rate_level[N-5],rate_level[N-6],rate_level[N-7],rate_level[N-8],rate_level[N-9],rate_level[N-10],rate_level[N-11],rate_level[N-12],rate_level[N-13]))
# compute AR(12) forecast error 
forecast
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] 
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N] 
(forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N])^2

# estimate the AR(16) model
GDPGR_AR16 <- dynlm(ts(rate_level) ~ L(ts(rate_level)) + L(ts(rate_level),2) + L(ts(rate_level),3) + L(ts(rate_level),4) + L(ts(rate_level),5) + L(ts(rate_level),6) + L(ts(rate_level),7) + L(ts(rate_level),8) + L(ts(rate_level),9) + L(ts(rate_level),10) + L(ts(rate_level),11) + L(ts(rate_level),12) + L(ts(rate_level),13) + L(ts(rate_level),14) + L(ts(rate_level),15) + L(ts(rate_level),16))
# AR(16) forecast of GDP growth in 2022:Q2 
forecast <- c("2022:Q2" = coef(GDPGR_AR16) %*% c(1, rate_level[N-2], rate_level[N-3], rate_level[N-4], rate_level[N-5],rate_level[N-6],rate_level[N-7],rate_level[N-8],rate_level[N-9],rate_level[N-10],rate_level[N-11],rate_level[N-12],rate_level[N-13],rate_level[N-14],rate_level[N-15],rate_level[N-16],rate_level[N-17]))
# compute AR(16) forecast error 
forecast
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] 
forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N] 
(forecast*state$CANQGSP[N-1] + state$CANQGSP[N-1] - state$CANQGSP[N])^2

CAplus1 = state$CANQGSP
CAplus1_level <- as.numeric(CAplus1[-1])
CAplus1_lags <- as.numeric(CAplus1[-N])
ratesplus1 <- (CAplus1_level - CAplus1_lags)/CAplus1_lags
rateplus1_level <- ratesplus1[-1]
rateplus1_lags <- ratesplus1[-N]

# estimate the AR(4) model 
AR4plus1 <- dynlm(ts(rateplus1_level) ~ L(ts(rateplus1_level)) + L(ts(rateplus1_level),2) +  L(ts(rateplus1_level),3) + L(ts(rateplus1_level),4))
# AR(4) forecast of GDP growth in 2022:Q3 
forecast <- c("2022:Q3" = coef(AR4plus1) %*% c(1, rateplus1_level[N-2], rateplus1_level[N-3],rateplus1_level[N-4],rateplus1_level[N-5]))
# compute AR(4) forecast error 
forecast

CAplus1[71] <- forecast*CAplus1[N] + CAplus1[N] 

CAplus2 = CAplus1
CAplus2_level <- as.numeric(CAplus2[-1])
CAplus2_lags <- as.numeric(CAplus2[-(N+1)])

ratesplus2<- (CAplus2_level - CAplus2_lags)/CAplus2_lags
rateplus2_level <- ratesplus2[-1]
rateplus2_lags <- ratesplus2[-(N+1)]

# estimate the AR(4) model 
AR4plus2 <- dynlm(ts(rateplus2_level) ~ L(ts(rateplus2_level)) + L(ts(rateplus2_level),2) +  L(ts(rateplus2_level),3) + L(ts(rateplus2_level),4))
# AR(4) forecast of GDP growth in 2022:Q4 
forecast <- c("2022:Q4" = coef(AR4plus2) %*% c(1, rateplus2_level[N-1], rateplus2_level[N-2],rateplus2_level[N-3],rateplus2_level[N-4]))
# compute AR(4) forecast error 
forecast
forecast*CAplus2[N+1] + CAplus2[N+1]
CAplus2[72] <- forecast*CAplus1[N+1] + CAplus1[N+1]

CAplus3 = CAplus2
CAplus3_level <- as.numeric(CAplus3[-1])
CAplus3_lags <- as.numeric(CAplus3[-(N+2)])

ratesplus3<- (CAplus3_level - CAplus3_lags)/CAplus3_lags
rateplus3_level <- ratesplus3[-1]
rateplus3_lags <- ratesplus3[-(N+2)]

# estimate the AR(4) model 
AR4plus3 <- dynlm(ts(rateplus3_level) ~ L(ts(rateplus3_level)) + L(ts(rateplus3_level),2) +  L(ts(rateplus3_level),3) + L(ts(rateplus3_level),4))
# AR(4) forecast of GDP growth in 2023:Q1 
forecast <- c("2023:Q1" = coef(AR4plus3) %*% c(1, rateplus3_level[N], rateplus3_level[N-1],rateplus3_level[N-2],rateplus3_level[N-3]))
# compute AR(4) forecast error 
forecast
forecast*CAplus3[N+2] + CAplus3[N+2]

CAplus3[73] <- forecast*CAplus3[N+2] + CAplus3[N+2]

DATE <- state$DATE
DATE[71] <- "2022-07-01 UTC"
DATE[72] <- "2022-10-01 UTC"
DATE[73] <- "2023-01-01 UTC"
cols <- rep("black",70)
for(i in {x <- 71:73;x})cols[i] <- "red"
plot(DATE,CAplus3,main='California',xlab='Year',ylab='GDP',col=cols)

# To compute the MSFE, we train our model on the first 50 data points,
# and compare the predictions for the remaining 20 data points with the
# actual values, averaging the square of the (naive) forecasting error.
fifty <- state$AZNQGSP[1:50]
M <- length(fifty)

predictions <- fifty
for (i in 1:20) {
  new_level <- as.numeric(predictions[-1])
  new_lags <- as.numeric(predictions[-M])
  
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-M+i]
  
  # estimate the AR(4) model 
  AR4 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2) +  L(ts(new_rate_level),3) + L(ts(new_rate_level),4))
  # AR(4) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR4) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4],new_rate_level[M+i-5],new_rate_level[M+i-6]))
  # compute AR(4) forecast error 
  forecast
  forecast*predictions[M] + predictions[M]
  predictions[M+i] <- forecast*predictions[M] + predictions[M]
}
#predictions[51]

(predictions - state$CANQGSP)[51:70]
four <- mean(((predictions - state$CANQGSP)^2)[51:70])

ar2predictions <- fifty

for (i in 1:20) {
  new_level <- as.numeric(ar2predictions[-1])
  new_lags <- as.numeric(ar2predictions[-M])
  
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-M+i]
  
  # estimate the AR(2) model 
  AR2 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2))
  # AR(2) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR2) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4]))
  # compute AR(2) forecast error 
  forecast
  forecast*ar2predictions[M] + ar2predictions[M]
  ar2predictions[M+i] <- forecast*ar2predictions[M] + ar2predictions[M]
}

(ar2predictions - state$CANQGSP)[51:70]
two <- mean(((ar2predictions - state$CANQGSP)^2)[51:70])

ar3predictions <- fifty

for (i in 1:20) {
  new_level <- as.numeric(ar3predictions[-1])
  new_lags <- as.numeric(ar3predictions[-M])
  
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-M+i]
  
  # estimate the AR(3) model 
  AR3 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2) + L(ts(new_rate_level),3))
  # AR(3) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR3) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4],new_rate_level[M+i-5]))
  # compute AR(3) forecast error 
  forecast
  forecast*ar3predictions[M] + ar3predictions[M]
  ar3predictions[M+i] <- forecast*ar3predictions[M] + ar3predictions[M]
}

(ar3predictions - state$CANQGSP)[51:70]
three <- mean(((ar3predictions - state$CANQGSP)^2)[51:70])

ar5predictions <- fifty
ar6predictions <- fifty
ar8predictions <- fifty
ar12predictions <- fifty
ar16predictions <- fifty
ar1predictions <- fifty

for (i in 1:20) {
  new_level <- as.numeric(ar5predictions[-1])
  new_lags <- as.numeric(ar5predictions[-M])
  
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-M+i]
  
  # estimate the AR(5) model 
  AR5 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2) + L(ts(new_rate_level),3) + L(ts(new_rate_level),4) + L(ts(new_rate_level),5))
  # AR(5) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR5) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4],new_rate_level[M+i-5],new_rate_level[M+i-6],new_rate_level[M+i-7]))
  # compute AR(5) forecast error 
  forecast
  forecast*ar5predictions[M] + ar5predictions[M]
  ar5predictions[M+i] <- forecast*ar5predictions[M] + ar5predictions[M]
}

(ar5predictions - state$CANQGSP)[51:70]
five <- mean(((ar5predictions - state$CANQGSP)^2)[51:70])

for (i in 1:20) {
  new_level <- as.numeric(ar6predictions[-1])
  new_lags <- as.numeric(ar6predictions[-M])
  
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-M+i]
  
  # estimate the AR(6) model 
  AR6 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2) + L(ts(new_rate_level),3) + L(ts(new_rate_level),4) + L(ts(new_rate_level),5) + L(ts(new_rate_level),6))
  # AR(6) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR6) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4],new_rate_level[M+i-5],new_rate_level[M+i-6],new_rate_level[M+i-7],new_rate_level[M+i-8]))
  # compute AR(6) forecast error 
  forecast
  forecast*ar6predictions[M] + ar6predictions[M]
  ar6predictions[M+i] <- forecast*ar6predictions[M] + ar6predictions[M]
}

(ar6predictions - state$CANQGSP)[51:70]
six <- mean(((ar6predictions - state$CANQGSP)^2)[51:70])

for (i in 1:20) {
  new_level <- as.numeric(ar8predictions[-1])
  new_lags <- as.numeric(ar8predictions[-M])
  
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-M+i]
  
  # estimate the AR(8) model 
  AR8 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2) + L(ts(new_rate_level),3) + L(ts(new_rate_level),4) + L(ts(new_rate_level),5) + L(ts(new_rate_level),6) + L(ts(new_rate_level),7) + L(ts(new_rate_level),8))
  # AR(8) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR8) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4],new_rate_level[M+i-5],new_rate_level[M+i-6],new_rate_level[M+i-7],new_rate_level[M+i-8],new_rate_level[M+i-9],new_rate_level[M+i-10]))
  # compute AR(8) forecast error 
  forecast
  forecast*ar8predictions[M] + ar8predictions[M]
  ar8predictions[M+i] <- forecast*ar8predictions[M] + ar8predictions[M]
}

(ar8predictions - state$CANQGSP)[51:70]
eight <- mean(((ar8predictions - state$CANQGSP)^2)[51:70])

for (i in 1:20) {
  new_level <- as.numeric(ar12predictions[-1])
  new_lags <- as.numeric(ar12predictions[-M])
  
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-M+i]
  
  # estimate the AR(12) model 
  AR12 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2) + L(ts(new_rate_level),3) + L(ts(new_rate_level),4) + L(ts(new_rate_level),5) + L(ts(new_rate_level),6) + L(ts(new_rate_level),7) + L(ts(new_rate_level),8) + L(ts(new_rate_level),9) + L(ts(new_rate_level),10) + L(ts(new_rate_level),11) + L(ts(new_rate_level),12))
  # AR(12) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR12) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4],new_rate_level[M+i-5],new_rate_level[M+i-6],new_rate_level[M+i-7],new_rate_level[M+i-8],new_rate_level[M+i-9],new_rate_level[M+i-10],new_rate_level[M+i-11],new_rate_level[M+i-12],new_rate_level[M+i-13],new_rate_level[M+i-14]))
  # compute AR(12) forecast error 
  forecast
  forecast*ar12predictions[M] + ar12predictions[M]
  ar12predictions[M+i] <- forecast*ar12predictions[M] + ar12predictions[M]
}

(ar12predictions - state$CANQGSP)[51:70]
twelve <- mean(((ar12predictions - state$CANQGSP)^2)[51:70])

for (i in 1:20) {
  new_level <- as.numeric(ar16predictions[-1])
  new_lags <- as.numeric(ar16predictions[-M])
  
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-M+i]
  
  # estimate the AR(16) model 
  AR16 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2) + L(ts(new_rate_level),3) + L(ts(new_rate_level),4) + L(ts(new_rate_level),5) + L(ts(new_rate_level),6) + L(ts(new_rate_level),7) + L(ts(new_rate_level),8) + L(ts(new_rate_level),9) + L(ts(new_rate_level),10) + L(ts(new_rate_level),11) + L(ts(new_rate_level),12) + L(ts(new_rate_level),13) + L(ts(new_rate_level),14) + L(ts(new_rate_level),15) + L(ts(new_rate_level),16))
  # AR(16) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR16) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4],new_rate_level[M+i-5],new_rate_level[M+i-6],new_rate_level[M+i-7],new_rate_level[M+i-8],new_rate_level[M+i-9],new_rate_level[M+i-10],new_rate_level[M+i-11],new_rate_level[M+i-12],new_rate_level[M+i-13],new_rate_level[M+i-14],new_rate_level[M+i-15],new_rate_level[M+i-16],new_rate_level[M+i-17],new_rate_level[M+i-18]))
  # compute AR(16) forecast error 
  forecast
  forecast*ar16predictions[M] + ar16predictions[M]
  ar16predictions[M+i] <- forecast*ar16predictions[M] + ar16predictions[M]
}

(ar16predictions - state$CANQGSP)[51:70]
sixteen <- mean(((ar16predictions - state$CANQGSP)^2)[51:70])

for (i in 1:20) {
  new_level <- as.numeric(ar1predictions[-1])
  new_lags <- as.numeric(ar1predictions[-M])
  
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-M+i]
  # estimate the AR(1) model 
  AR1 <- lm(new_rate_level ~ new_rate_lags)
  # AR(1) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR1) %*% c(1, new_rate_level[M+i-3]))
  # compute AR(1) forecast error 
  forecast
  forecast*ar1predictions[M] + ar1predictions[M]
  ar1predictions[M+i] <- forecast*ar1predictions[M] + ar1predictions[M]
}

(ar1predictions - state$CANQGSP)[51:70]
one <- mean(((ar1predictions - state$CANQGSP)^2)[51:70])

one
two
three
four
five
six
eight
twelve
sixteen

summary(AR1)$r.squared
summary(AR2)$r.squared
summary(AR3)$r.squared
summary(AR4)$r.squared
summary(AR5)$r.squared
summary(AR6)$r.squared
summary(AR8)$r.squared
summary(AR12)$r.squared
summary(AR16)$r.squared

