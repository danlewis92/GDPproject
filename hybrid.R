# install packages
#install.packages("forecast")
library(forecast)
library(dynlm)
library(ggplot2)
library(dplyr)

# Load CSV file
state <- readxl::read_xls(path = "~/Downloads/Round2.xls",sheet = 2)

head(state)
tail(state)
state$AZNQGSP
plot(state$DATE,state$AZNQGSP,main='Arizona GDP',xlab='Year',ylab='GDP')
state

N <- length(state$AZNQGSP)

GDPGR_level <- as.numeric(state$AZNQGSP[-1])

GDPGR_level[71] <- NaN
#diffs <- GDPGR_level - state$AZNQGSP
quots <- GDPGR_level / state$AZNQGSP
#mean(diffs[1:70])
rate <- mean(quots[1:50])
rate 
# For the constant growth model, we estimate 0.85% economic growth year-on-year.

constant <- state$AZNQGSP[1:2]
constant[2] <- state$AZNQGSP[1] * rate 
for (i in 2:N) {
  constant[i] <- constant[i-1] * rate 
}

state[3] <-constant
state <- rename(state, const = ...3)
tail(state)

N <- length(state$AZNQGSP)
GDPGR_level <- as.numeric(state$AZNQGSP[-1])
GDPGR_lags <- as.numeric(state$AZNQGSP[-N])

rates <- (GDPGR_level - GDPGR_lags)/GDPGR_lags
rate_level <- rates[-1]
rate_lags <- rates[-N+1]

# To compute the MSFE, we train our model on the first 50 data points,
# and compare the predictions for the remaining 20 data points with the
# actual values, averaging the square of the (naive) forecasting error.
fifty <- state$AZNQGSP[1:50]
M <- length(fifty)

predictions <- fifty

for (i in 1:21) {
  new_level <- as.numeric(predictions[-1])
  new_lags <- as.numeric(predictions[-(M+i-1)])
  
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-(M+i-1)]
  
  # estimate the AR(4) model 
  AR4 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2) +  L(ts(new_rate_level),3) + L(ts(new_rate_level),4))
  # AR(4) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR4) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4],new_rate_level[M+i-5],new_rate_level[M+i-6]))
  # compute AR(4) forecast error 
  forecast
  forecast*predictions[M+i-1] + predictions[M+i-1]
  predictions[M+i] <- forecast*predictions[M+i-1] + predictions[M+i-1]
}
#predictions[51]

predictions
state[4] <- predictions
state <- rename(state, ARfour = ...4)
state
hybrid <- (state$const + state$ARfour)/2
state[5] <- hybrid
state <- rename(state, hybrid = ...5)
state

plot1 <- ggplot(state, aes(DATE,AZNQGSP)) + geom_point()
plot1
library("tidyr")
library("reshape2")
test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "const", "ARfour", 'hybrid'))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

(predictions - state$AZNQGSP)[51:N]
four <- mean(((predictions - state$AZNQGSP)^2)[51:N])
four

(state$const - state$AZNQGSP)[51:N]
consta <- mean(((state$const - state$AZNQGSP)^2)[51:N])
consta

(state$hybrid - state$AZNQGSP)[51:N]
hyb <- mean(((state$hybrid - state$AZNQGSP)^2)[51:N])
hyb
hyb < four
consta < hyb


# THE TIMING SECTION: SKIP IF NOT A CONCERN
# COMPUTE R^2'S, RUNTIMES AND COMPARE WITH AR(16) AND ITS HYBRID
#
#

start <- Sys.time()
for (i in 1:21) {
  new_level <- as.numeric(predictions[-1])
  new_lags <- as.numeric(predictions[-(M+i-1)])
  
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-(M+i-1)]
  
  # estimate the AR(4) model 
  AR4 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2) +  L(ts(new_rate_level),3) + L(ts(new_rate_level),4))
  # AR(4) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR4) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4],new_rate_level[M+i-5],new_rate_level[M+i-6]))
  # compute AR(4) forecast error 
  forecast
  forecast*predictions[M+i-1] + predictions[M+i-1]
  predictions[M+i] <- forecast*predictions[M+i-1] + predictions[M+i-1]
}
end <- Sys.time()
end - start

start <- Sys.time()
GDPGR_level <- as.numeric(state$AZNQGSP[-1])
GDPGR_level[71] <- NaN
#diffs <- GDPGR_level - state$AZNQGSP
quots <- GDPGR_level / state$AZNQGSP
#mean(diffs[1:70])
rate <- mean(quots[1:50])
rate 
# For the constant growth model, we estimate 0.85% economic growth year-on-year.
constant <- state$AZNQGSP[1:2]
constant[2] <- state$AZNQGSP[1] * rate 
for (i in 2:N) {
  constant[i] <- constant[i-1] * rate 
}
end <- Sys.time()
end - start

start <- Sys.time()
N <- length(state$AZNQGSP)
GDPGR_level <- as.numeric(state$AZNQGSP[-1])
GDPGR_level[71] <- NaN
quots <- GDPGR_level / state$AZNQGSP
rate <- mean(quots[1:50])
rate 
constant <- state$AZNQGSP[1:2]
constant[2] <- state$AZNQGSP[1] * rate 
for (i in 2:N) {
  constant[i] <- constant[i-1] * rate 
}
state[3] <-constant
#state <- rename(state, const = ...3)
tail(state)
N <- length(state$AZNQGSP)
GDPGR_level <- as.numeric(state$AZNQGSP[-1])
GDPGR_lags <- as.numeric(state$AZNQGSP[-N])
rates <- (GDPGR_level - GDPGR_lags)/GDPGR_lags
rate_level <- rates[-1]
rate_lags <- rates[-N+1]
fifty <- state$AZNQGSP[1:50]
M <- length(fifty)
predictions <- fifty
for (i in 1:21) {
  new_level <- as.numeric(predictions[-1])
  new_lags <- as.numeric(predictions[-(M+i-1)])
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-(M+i-1)]
  # estimate the AR(4) model 
  AR4 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2) +  L(ts(new_rate_level),3) + L(ts(new_rate_level),4))
  # AR(4) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR4) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4],new_rate_level[M+i-5],new_rate_level[M+i-6]))
  # compute AR(4) forecast error 
  forecast
  forecast*predictions[M+i-1] + predictions[M+i-1]
  predictions[M+i] <- forecast*predictions[M+i-1] + predictions[M+i-1]
}
predictions
state[4] <- predictions
#state <- rename(state, ARfour = ...4)
state
hybrid <- (state$const + state$ARfour)/2
end <- Sys.time()
end - start

ar16predictions <- fifty
start <- Sys.time()
for (i in 1:21) {
  new_level <- as.numeric(ar16predictions[-1])
  new_lags <- as.numeric(ar16predictions[-(M+i-1)])
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-(M+i-1)]
  # estimate the AR(16) model 
  AR16 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2) + L(ts(new_rate_level),3) + L(ts(new_rate_level),4) + L(ts(new_rate_level),5) + L(ts(new_rate_level),6) + L(ts(new_rate_level),7) + L(ts(new_rate_level),8) + L(ts(new_rate_level),9) + L(ts(new_rate_level),10) + L(ts(new_rate_level),11) + L(ts(new_rate_level),12) + L(ts(new_rate_level),13) + L(ts(new_rate_level),14) + L(ts(new_rate_level),15) + L(ts(new_rate_level),16))
  # AR(16) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR16) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4],new_rate_level[M+i-5],new_rate_level[M+i-6],new_rate_level[M+i-7],new_rate_level[M+i-8],new_rate_level[M+i-9],new_rate_level[M+i-10],new_rate_level[M+i-11],new_rate_level[M+i-12],new_rate_level[M+i-13],new_rate_level[M+i-14],new_rate_level[M+i-15],new_rate_level[M+i-16],new_rate_level[M+i-17],new_rate_level[M+i-18]))
  # compute AR(16) forecast error 
  forecast
  forecast*ar16predictions[M+i-1] + ar16predictions[M+i-1]
  ar16predictions[M+i] <- forecast*ar16predictions[M+i-1] + ar16predictions[M+i-1]
}
(ar16predictions - state$AZNQGSP)[51:71]
sixteen <- mean(((ar16predictions - state$AZNQGSP)^2)[51:71])
end <- Sys.time()
end - start
sixteen

state[6] <- ar16predictions
state <- rename(state, AR16 = ...6)
hybrid16 <- (state$const + state$AR16)/2
state[7] <- hybrid16
state <- rename(state, hybrid16 = ...7)
state

(state$hybrid16 - state$AZNQGSP)[51:N]
hyb16 <- mean(((state$hybrid16 - state$AZNQGSP)^2)[51:N])
hyb16

start <- Sys.time()
ar16predictions <- fifty
GDPGR_level <- as.numeric(state$AZNQGSP[-1])
GDPGR_level[71] <- NaN
quots <- GDPGR_level / state$AZNQGSP
rate <- mean(quots[1:50])
rate 
constant <- state$AZNQGSP[1:2]
constant[2] <- state$AZNQGSP[1] * rate 
for (i in 2:N) {
  constant[i] <- constant[i-1] * rate 
}
state[3] <-constant
#state <- rename(state, const = ...3)
tail(state)
for (i in 1:21) {
  new_level <- as.numeric(ar16predictions[-1])
  new_lags <- as.numeric(ar16predictions[-(M+i-1)])
  newrates<- (new_level - new_lags)/new_lags
  new_rate_level <- newrates[-1]
  new_rate_lags <- newrates[-(M+i-1)]
  # estimate the AR(16) model 
  AR16 <- dynlm(ts(new_rate_level) ~ L(ts(new_rate_level)) + L(ts(new_rate_level),2) + L(ts(new_rate_level),3) + L(ts(new_rate_level),4) + L(ts(new_rate_level),5) + L(ts(new_rate_level),6) + L(ts(new_rate_level),7) + L(ts(new_rate_level),8) + L(ts(new_rate_level),9) + L(ts(new_rate_level),10) + L(ts(new_rate_level),11) + L(ts(new_rate_level),12) + L(ts(new_rate_level),13) + L(ts(new_rate_level),14) + L(ts(new_rate_level),15) + L(ts(new_rate_level),16))
  # AR(16) forecast of GDP growth in quarter in question
  forecast <- c("Quarter in question" = coef(AR16) %*% c(1, new_rate_level[M+i-3], new_rate_level[M+i-4],new_rate_level[M+i-5],new_rate_level[M+i-6],new_rate_level[M+i-7],new_rate_level[M+i-8],new_rate_level[M+i-9],new_rate_level[M+i-10],new_rate_level[M+i-11],new_rate_level[M+i-12],new_rate_level[M+i-13],new_rate_level[M+i-14],new_rate_level[M+i-15],new_rate_level[M+i-16],new_rate_level[M+i-17],new_rate_level[M+i-18]))
  # compute AR(16) forecast error 
  forecast
  forecast*ar16predictions[M+i-1] + ar16predictions[M+i-1]
  ar16predictions[M+i] <- forecast*ar16predictions[M+i-1] + ar16predictions[M+i-1]
}
state[6] <- ar16predictions
#state <- rename(state, AR16 = ...6)
hybrid16 <- (state$const + state$AR16)/2
end <- Sys.time()
end - start



test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "const", "ARfour", "hybrid", "AR16", "hybrid16"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "ARfour"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "ARfour", "const"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "ARfour", "const", "hybrid"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

