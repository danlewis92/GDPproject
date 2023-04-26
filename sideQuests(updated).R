# Train the AR(4) model on data up to 2007 Q3, project ahead to 2015

eleven <- state$AZNQGSP[1:11]
M <- length(eleven)

predictions <- eleven

for (i in 1:29) {
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

plot(state$DATE[1:40],state$AZNQGSP[1:40],main='Arizona GDP',xlab='Year',ylab='GDP')
points(state$DATE[1:40],predictions[1:40],col="green")


# Train the AR(4) model on data up to 2019 Q4, project ahead to 2022 Q2
sixty <- state$AZNQGSP[1:60]
M <- length(sixty)

predictions <- sixty

for (i in 1:11) {
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
  predictions[M+i] <- forecast*predictions[M+i-1] + predictions[M+i-1]
}

plot(state$DATE,state$AZNQGSP,main='Arizona GDP',xlab='Year',ylab='GDP')
points(state$DATE,predictions,col="green")
predictions

state[4] <- predictions

GDPGR_level <- as.numeric(state$AZNQGSP[-1])

GDPGR_level[71] <- NaN
#diffs <- GDPGR_level - state$AZNQGSP
quots <- GDPGR_level / state$AZNQGSP
#mean(diffs[1:70])
rate <- mean(quots[1:60])
rate 
# For the constant growth model, we estimate 0.92% economic growth year-on-year.

constant <- state$AZNQGSP[1:2]
constant[2] <- state$AZNQGSP[1] * rate 
for (i in 2:N) {
  constant[i] <- constant[i-1] * rate 
}
state[3] <-constant
state <- rename(state, const = ...3)
tail(state)

hybrid <- (state$const + state$ARfour)/2
state[5] <- hybrid

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "ARfour"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "ARfour", "const"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "ARfour", "const", "hybrid"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")


ar16predictions <- sixty
for (i in 1:11) {
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
hybrid16 <- (state$const + state$AR16)/2
state[7] <- hybrid16

test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "const", "ARfour", "hybrid", "AR16", "hybrid16"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

(state$hybrid16 - state$AZNQGSP)[61:N]
hyb16 <- mean(((state$hybrid16 - state$AZNQGSP)^2)[61:N])
hyb16
