###############################################################
# Title:        ps3.r
# Author:       Michael Sturm
# Date:         2018-03-27
# Description:  Turn-in product for problem set 3
###############################################################

rm(list=ls(all=TRUE))

## Import packages
library(data.table)
library(sandwich)
library(lmtest)
library(tseries)
library(plm)

## QUESTION 1 ##########################################
## Data import and validation
context1    <- fread('hprice1.csv')
summary(context1)

model1 <- lm(price~bdrms+lotsize+sqrft, data=context1)

#OLS Significance Test
vcov(model1)
coeftest(model1,vcov.=vcov) 

#correct
coeftest(model1)

#White Corrected Significance Test
vcovHC(model1)
coeftest(model1,vcov.=vcovHC)

model2 <- lm(log(price)~bdrms+log(lotsize)+log(sqrft), data=context1)

#OLS Significance Test
vcov(model2)
coeftest(model2,vcov.=vcov) 

#correct
coeftest(model2)

#White Corrected Significance Test
vcovHC(model2)
coeftest(model2,vcov.=vcovHC)

## QUESTION 2 ###########################################
context2  <- fread('beveridge.csv')
summary(context2)

model3 <- lm(urate~vrate, data = context2)

#Compute the OLS significance and NeweyWest significance tests
vcov(model3)
coeftest(model3,vcov.=vcov) 
vcov=NeweyWest(model3, lag=5)
coeftest (model3,vcov=NeweyWest(model3, lag=5))

#Perform the level and trend KPSS tests on uratet and vratet (4 tests total):
kpss.test(context2$urate, null="Level")
kpss.test(context2$urate, null="Trend")
kpss.test(context2$vrate, null="Level")
kpss.test(context2$vrate, null="Trend")

#Perform the level and trend KPSS tests on diff uratet and  diff vratet (4 tests total):
kpss.test(diff(context2$urate), null="Level")
kpss.test(diff(context2$urate), null="Trend")
kpss.test(diff(context2$vrate), null="Level")
kpss.test(diff(context2$vrate), null="Trend")

#Perform the level and trend KPSS tests on  diff(diff uratet) and diff(diff vratet) (4 tests total):
diffurate2 <- diff(diff(context2$urate))
diffvrate2 <- diff(diff(context2$vrate))
kpss.test(diffurate2, null="Level")
kpss.test(diffurate2, null="Trend")
kpss.test(diffvrate2, null="Level")
kpss.test(diffvrate2, null="Trend")

model4 <- lm(diff(urate)~diff(vrate), data=context2)

#Compute the OLS significance test results and the 
#Newey-West-corrected significance test 
#(5 lags; NeweyWest) results for model4.
vcov(model4)
coeftest(model4,vcov=NeweyWest(model4, lag=5))

## QUESTION 3 ###########################################
context3 <- fread('JTRAIN.csv')
summary(context3)

#declare panel
context3 <- plm.data(context3, indexes=c('fcode','year'))

#create new variable d88
context3$d88 <- ifelse(context3$year==1988,1,0)

#create new variable d89
context3$d89 <- ifelse(context3$year==1989,1,0)

# generate lag(grant)
#context3$grant_lag <- rep(0,471) #another way to do this
#for(j in 1:471)
# if(context3$year[j]!=1987)
#   context3$grant_lag[j] <- context3$grant[j-1]
  

#create new variable indicating if the firm had a grant last year
context3[ , grantLastYear := ifelse(context3$year==1987,0,shift(context3$grant)) ]

#run the pooled linear model
model5 <- plm(log(scrap)~d88+d89+grant+grantLastYear,model="pooling",data = context3 )

summary(model5)

coeftest(model5,vcov=vcovHC(model5, method="arellano"))

#run the fixed effects model
model6 <- plm(log(scrap)~d88+d89+grant+grantLastYear,model="within",data = context3 )

summary(model6)

#Compute the OLS and #HAC Arellano tests for model 6
vcov(model6)
coeftest(model6,vcov.=vcov) 
summary(model6, vcov=vcovHC(model6, method="arellano"))
coeftest(model6,vcov=vcovHC(model6, method="arellano"))

