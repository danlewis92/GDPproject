# install packages
#install.packages("forecast")
library(forecast)
library(dynlm)

# Load xls file
state <- readxl::read_xls(path = "~/Downloads/State_GDP_2.xls",sheet = 2)

head(state)

state$AZNQGSP
plot(state$DATE,state$AZNQGSP,main='Arizona GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$AKNQGSP,main='Alaska GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$ALNQGSP,main='Alabama GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$ARNQGSP,main='Arkansas GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$CANQGSP,main='California GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$CONQGSP,main='Colorado GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$CTNQGSP,main='Connecticut GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$DCNQGSP,main='Washington DC GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$DENQGSP,main='Delaware GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$FLNQGSP,main='Florida GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$GANQGSP,main='Georgia GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$HINQGSP,main='Hawaii GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$IANQGSP,main='Iowa GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$IDNQGSP,main='Idaho GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$ILNQGSP,main='Illinois GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$INNQGSP,main='Indiana GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$KSNQGSP,main='Kansas GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$KYNQGSP,main='Kentucky GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$LANQGSP,main='Louisiana GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$MANQGSP,main='Massachusetts GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$MDNQGSP,main='Maryland GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$MENQGSP,main='Maine GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$MINQGSP,main='Michigan GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$MNNQGSP,main='Minnesota GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$MONQGSP,main='Missouri GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$MSNQGSP,main='Mississippi GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$MTNQGSP,main='Montana GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$NCNQGSP,main='North Carolina GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$NDNQGSP,main='North Dakota GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$NENQGSP,main='Nebraska GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$NHNQGSP,main='New Hampshire GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$NJNQGSP,main='New Jersey GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$NMNQGSP,main='New Mexico GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$NVNQGSP,main='Nevada GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$NYNQGSP,main='New York GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$OHNQGSP,main='Ohio GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$OKNQGSP,main='Oklahoma GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$ORNQGSP,main='Oregon GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$PANQGSP,main='Pennsylvania GDP',xlab='Year',ylab='GDP')

plot(state$DATE,state$RINQGSP,main='Rhode Island',xlab='Year',ylab='GDP')

plot(state$DATE,state$SCNQGSP,main='South Carolina',xlab='Year',ylab='GDP')

plot(state$DATE,state$SDNQGSP,main='South Dakota',xlab='Year',ylab='GDP')

plot(state$DATE,state$TNNQGSP,main='Tennessee',xlab='Year',ylab='GDP')

plot(state$DATE,state$TXNQGSP,main='Texas',xlab='Year',ylab='GDP')

plot(state$DATE,state$UTNQGSP,main='Utah',xlab='Year',ylab='GDP')

plot(state$DATE,state$VANQGSP,main='Virginia',xlab='Year',ylab='GDP')

plot(state$DATE,state$VTNQGSP,main='Vermont',xlab='Year',ylab='GDP')

plot(state$DATE,state$WANQGSP,main='Washington',xlab='Year',ylab='GDP')

plot(state$DATE,state$WINQGSP,main='Wisconsin',xlab='Year',ylab='GDP')

plot(state$DATE,state$WVNQGSP,main='West Virginia',xlab='Year',ylab='GDP')

plot(state$DATE,state$WYNQGSP,main='Wyoming',xlab='Year',ylab='GDP')