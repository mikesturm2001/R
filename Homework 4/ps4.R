###############################################################
# Title:        ps4.r
# Author:       Michael Sturm
# Date:         2018-04-17
# Description:  Turn-in product for problem set 4
###############################################################

rm(list=ls(all=TRUE))

## Import packages
library(data.table)
library(sandwich)
library(lmtest)
library(sandwich)
library(tree)
library(party)
library(evtree)

########################### QUESTION 1 ##########################

## Data import and validaticon
context1    <- fread('htv.csv')

model1 <- lm(log(wage)~abil+educ+exper,data=context1)
summary(model1)

# find the AIC and BIC of Model 1
c(AIC(model1),BIC(model1))

context1$abilsq    <- context1$abil^2
context1$educsq    <- context1$educ^2
context1$expersq   <- context1$exper^2
context1$abilxeduc  <- context1$abil*context1$educ
context1$abilxexper   <- context1$abil*context1$exper
context1$educxecper  <- context1$educ*context1$exper

model2 <- lm(log(wage)~abil+educ+abilsq+educsq+expersq+abilxeduc+abilxexper+educxecper, data=context1)

# find the AIC and BIC of Model 2
c(AIC(model2),BIC(model2))

#remove as many variables as you can from model2 to find the key subset of variables that makes model 2
# have the lowest BIC possible

#remove variables one at a time to find the lowest BIC. 

#takes the derivative when trying to understand (var x var) aka interaction
#when you add education expierence becomes more valuable. these two
#factors are compliments. (positive after the derative) negative hurting.

############################ QUESTION 2 ###########################

#model 25?

context2 <- fread('loanapp.csv')

model3 <- glm(approve~white,data=context2)
summary(model3)

#Compute the White heteroskedasticity robust standard errors for model 3
coeftest(model3)
coeftest(model3,vcov.=vcovHC)

#margins! to give you the magnitude (model 3 from instructor)
margins(model3)

model4 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,family=binomial(link="logit"),data = context2)
summary(model4)

#margins model 4
margins(model4)

coeftest(model4)
coeftest(model4,vcov.=vcovHC)

context2$whitexobrat <- context2$white*context2$obrat

model5 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr+whitexobrat,family=binomial(link="logit"),data = context2)
summary(model5)

coeftest(model5)
coeftest(model5,vcov.=vcovHC)
#obrat changes when you are white vs other values.
#other obligations. race affects lending via other obligations. 

################################ QUESTION 3 ##################################

context3 <- fread('smoke.csv')

context3$agesq <- context3$age^2

model6 <- glm(cigs~educ+age+agesq+log(income)+restaurn,family=poisson(link="log"),data=context3)
summary(model6)

#when interpereting a binomial margin use the margins
#for posson g(y) = ln(y)
#interpret via percent change

coeftest(model6,vcov.=vcovHC)
#take derative for age squared. combined plug in age and it give you values.

#intensive margin
#how intense for one. each person smokes 6% more.
#each person stops smoking 5% fewer cigs
#extensive margin
#how broad. 6% more of the population starts smoking.
#5% decrease in the population that smokes.

############################## QUESTION 4 #####################################

context4 <-fread('hdisease.csv')

# create numeric outcome variable (instead of the character var "exang")
context4$exang		<-	ifelse(context4$exang=="yes",1,0)

# Formula to estimate
frmla <- hdisease ~ age + cp + trestbps + thalach + exang


# recursive partitioning model using the ctree function
model7		<-	ctree(frmla, data=context4)
model7
# display tree; looks better
plot(model7,main="Conditional Inference Tree (Heart Disease)")

# recursive partitioning model using the evtree function
model8		<-	evtree(frmla,data=context4)
# model took MUCH longer; plot of tree is MUCH better
model8
plot(model8)

context45 <- fread('hdisease-new.csv')

# create numeric outcome variable (instead of the character var "exang")
context45$exang		<-	ifelse(context45$exang=="yes",1,0)

hdisease_pred = predict(model8)

################################ QUESTION 5 #####################################

context5 <- fread('WAGE1.csv')

############################
## K-means Estimation
############################
?kmeans
seed        <-	2	# NOT a good random seed!
maxClusters	<-	10 #try with 50, then with 15

##need all the madness. will want to plot. this is the elbow test.


## Run the model
set.seed(seed)
model9 <- kmeans(context5,centers=3,nstart=10)
model9$centers
groups1 <- model9$cluster
groups1
context5$clusters <- groups1

model10 <- lm(log(wage)~educ+exper+tenure,data=subset(context5,clusters==1))
model11 <- lm(log(wage)~educ+exper+tenure,data=subset(context5,clusters==2))
model12 <- lm(log(wage)~educ+exper+tenure,data=subset(context5,clusters==3))
summary(model10)
summary(model11)
summary(model12)

################################## QUESTION 6 ##################################

context6 <- fread('murder.csv')

#before this
#make data stationary
xdata <- context6[1:50,2:52]
xdata <- context6[2:50,53:103]
model13 <- prcomp(xdata)
eig < model13$sdev
variance <- sum(eig) - cumsum(eig)
plot(0:10,variance[1:11])
lines(0:10,variance[1:11])
screeplot(model13,type="lines") # looks like there are 2 principal components

factor <- model13$x[.1]
ts.plot(factor)
context6[21,1]
screeplot(model13)