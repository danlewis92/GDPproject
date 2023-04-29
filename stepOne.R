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
rate <- mean(quots[1:70])
rate 
# For the constant growth model, we estimate 1.08% economic growth year-on-year.

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

seventy <- state$AZNQGSP[1:70]
M <- length(seventy)

predictions <- seventy

for (i in 1:1) {
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
tail(state)

plot1 <- ggplot(state, aes(DATE,AZNQGSP)) + geom_point()
plot1
library("tidyr")
library("reshape2")

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "ARfour"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "const", "ARfour"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "const", "ARfour", 'hybrid'))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

state$AZNQGSP[71]-state$const[71]
state$AZNQGSP[71]-state$ARfour[71]
state$AZNQGSP[71]-state$hybrid 


# THE TIMING SECTION: SKIP IF NOT A CONCERN
# COMPUTE R^2'S, RUNTIMES AND COMPARE WITH AR(16) AND ITS HYBRID
#
#
start <- Sys.time()
predictions <- seventy
for (i in 1:1) {
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
rate <- mean(quots[1:70])
rate 
# For the constant growth model, we estimate 1.08% economic growth year-on-year.
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
rate <- mean(quots[1:70])
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
fifty <- state$AZNQGSP[1:70]
predictions <- seventy
for (i in 1:1) {
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

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "ARfour"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "ARfour", "const"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "ARfour", "const", "hybrid"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

