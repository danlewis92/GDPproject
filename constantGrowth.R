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

cols <- rep("black",70)
for(i in {x <- 71:73;x})cols[i] <- "red"

plot1 <- ggplot(state, aes(DATE,AZNQGSP)) + geom_point()
#plot2 <- ggplot(state, aes(DATE)) + geom_point()

#plot2 + 
#  geom_point(state,aes(y = AZNQGSP, color="AZNQGSP")) + 
#  geom_point(state,aes(y = ...3, color="...3"))

plot1
#plot2

library("tidyr")
library("reshape2")

## convert to long format with tidyr::pivot_longer
state <- rename(state, const = ...3)
test_data_long_tidyr <- pivot_longer(state, cols = c("AZNQGSP", "const"))
ggplot(data=test_data_long_tidyr, aes(x=DATE, y=value, colour=name)) + geom_point() + ggtitle("Arizona GDP") + labs(y = "Total (million $)")

