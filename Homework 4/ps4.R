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


############################ QUESTION 2 ###########################

context2 <- fread('loanapp.csv')

model3 <- glm(approve~white,data=context2)
summary(model3)

#Compute the White heteroskedasticity robust standard errors for model 3
coeftest(model3)
coeftest(model3,vcov.=vcovHC)

model4 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,family=binomial(link="logit"),data = context2)
summary(model4)

coeftest(model4)
coeftest(model4,vcov.=vcovHC)

context2$whitexobrat <- context2$white*context2$obrat

model5 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr+whitexobrat,family=binomial(link="logit"),data = context2)
summary(model5)

coeftest(model5)
coeftest(model5,vcov.=vcovHC)

################################ QUESTION 3 ##################################

context3 <- fread('smoke.csv')

context3$agesq <- context3$age^2

model6 <- glm(cigs~educ+age+agesq+log(income)+restaurn,family=poisson(link="log"),data=context3)
summary(model6)

coeftest(model6)
coeftest(model6,vcov.=vcovHC)

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
plot(fitEv)

context5 <- fread('hdisease-new.csv')

# create numeric outcome variable (instead of the character var "exang")
context5$exang		<-	ifelse(context5$exang=="yes",1,0)

hdisease_pred = predict(model8)

################################ QUESTION 5 #####################################

context6 <- fread('WAGE1.csv')

############################
## K-means Estimation
############################
?kmeans
seed        <-	2	# NOT a good random seed!
maxClusters	<-	10 #try with 50, then with 15

## Run the model
set.seed(seed)
model9 <- kmeans(context6,centers=3,nstart=10)
model9$centers


groups1 <- model9$cluster
groups1
context6$cluster <- groups1

model10 <- lm(wage~educ+exper+tenure,data=context6[cluster==1])
model11 <- lm(wage~educ+exper+tenure,data=context6[cluster==2])
model12 <- lm(wage~educ+exper+tenure,data=context6[cluster==3])

################################## QUESTION 6 ##################################

context7 <- fread('murder.csv')


model13 <- prcomp(context7)
screeplot(model3,type="lines") # looks like there are 2 principal components


