###############################################################
# Title:        ps2.r
# Author:       Michael Sturm
# Date:         2018-02-13
# Description:  Turn-in product for problem set 2
###############################################################

rm(list=ls(all=TRUE))

## Import packages
library(data.table)

## Data import and validation
context1    <- fread('attend.csv')
summary(context1)
# attend         termGPA          priGPA           ACT            final      
# Min.   : 2.00   Min.   :0.000   Min.   :0.857   Min.   :13.00   Min.   :10.00  
# 1st Qu.:24.00   1st Qu.:2.138   1st Qu.:2.190   1st Qu.:20.00   1st Qu.:22.00  
# Median :28.00   Median :2.670   Median :2.560   Median :22.00   Median :26.00  
# Mean   :26.15   Mean   :2.601   Mean   :2.587   Mean   :22.51   Mean   :25.89  
# 3rd Qu.:30.00   3rd Qu.:3.120   3rd Qu.:2.942   3rd Qu.:25.00   3rd Qu.:29.00  
# Max.   :32.00   Max.   :4.000   Max.   :3.930   Max.   :32.00   Max.   :39.00  
# frosh             soph              hw       
# Min.   :0.0000   Min.   :0.0000   Min.   :0.000  
# 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:7.000  
# Median :0.0000   Median :1.0000   Median :8.000  
# Mean   :0.2324   Mean   :0.5765   Mean   :6.971  
# 3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:8.000  
# Max.   :1.0000   Max.   :1.0000   Max.   :8.000  

attendrt <- context1$attend
hwrt <- context1$hw

model1 <- lm(termGPA~priGPA+ACT+attendrt+hw, data=context1)

summary(model1)

# coefficients(model1) # model coefficients

# A. term GPA changes by .032 for each class attended
# termGPA increases by .114 for each homework turned in
# C. (2.88) GPA <- (32*.036) + (2.2*.549) + (28*.032) + (8*.114) - 1.287 
# D. (3.38) GPA <- (20*.036) + (3.9*.549) + (28*.032) + (8*.114) - 1.287
# E. PriGPA is more important than the ACT because it has a larger coefficient
# F. (2.74) GPA <- (25*.036) + (3.0*.549) + (32*.032) + (4*.114) - 1.287 
# G. (2.68) GPA <- (25*.036) + (3.0*.549) + (16*.032) + (8*.114) - 1.287
# H. Attendance is slighly higher than homework completion
# I. Homework and Attendance are directly correlated to the termGPA. PriGPA and ACT are not.

###############################################################

#Question 2

###############################################################

context2 <- fread('CEOSAL2.csv')
summary(context2)
#salary            age           college            grad            comten    
# Min.   : 100.0   Min.   :33.00   Min.   :0.0000   Min.   :0.0000   Min.   : 2.0  
# 1st Qu.: 471.0   1st Qu.:52.00   1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:12.0  
# Median : 707.0   Median :57.00   Median :1.0000   Median :1.0000   Median :23.0  
# Mean   : 865.9   Mean   :56.43   Mean   :0.9718   Mean   :0.5311   Mean   :22.5  
# 3rd Qu.:1119.0   3rd Qu.:62.00   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:33.0  
# Max.   :5299.0   Max.   :86.00   Max.   :1.0000   Max.   :1.0000   Max.   :58.0  
# ceoten           sales          profits           mktval     
# Min.   : 0.000   Min.   :   29   Min.   :-463.0   Min.   :  387  
# 1st Qu.: 3.000   1st Qu.:  561   1st Qu.:  34.0   1st Qu.:  644  
# Median : 6.000   Median : 1400   Median :  63.0   Median : 1200  
# Mean   : 7.955   Mean   : 3529   Mean   : 207.8   Mean   : 3600  
# 3rd Qu.:11.000   3rd Qu.: 3500   3rd Qu.: 208.0   3rd Qu.: 3500  
# Max.   :37.000   Max.   :51300   Max.   :2700.0   Max.   :45400  

model2 <- lm(log(salary)~log(mktval)+profits+ceoten, data=context2)
model3 <- lm(log(salary)~log(mktval)+profits+ceoten+log(sales), data=context2)

summary(model2)
summary(model3)

# A. LN give you the percentage change. 
# We do not take the Log of the profits because we don't want to measure the percent change in profits to 
# measure the salary of the CEO. We want to measure by what percentage an increase in the profits increase the salary.
# B. When the market value of the company increases by 1 percent the CEOs salary increases by .2386x100 percent
# C. When the market value of the company increases by 1 percent the CEOs salary increases by .1018x100 percent
# D. omitted variable bias. since we left out the sales in model2 
# E. the coefficient is still significant for large profits since we did not take the ln of profits
# Since some profits are 4 digits it becomes signficant
# F. for every 1 percent increase in sales the CEOs salary increases by .1622x100 percent

###############################################################

# Question 3

###############################################################

context3 <- fread('hprice1.csv')
summary(context3)
# price           assess          bdrms          lotsize          sqrft     
# Min.   :111.0   Min.   :198.7   Min.   :2.000   Min.   : 1000   Min.   :1171  
# 1st Qu.:230.0   1st Qu.:253.9   1st Qu.:3.000   1st Qu.: 5733   1st Qu.:1660  
# Median :265.5   Median :290.2   Median :3.000   Median : 6430   Median :1845  
# Mean   :293.5   Mean   :315.7   Mean   :3.568   Mean   : 9020   Mean   :2014  
# 3rd Qu.:326.2   3rd Qu.:352.1   3rd Qu.:4.000   3rd Qu.: 8583   3rd Qu.:2227  
# Max.   :725.0   Max.   :708.6   Max.   :7.000   Max.   :92681   Max.   :3880  
# colonial     
# Min.   :0.0000  
# 1st Qu.:0.0000  
# Median :1.0000  
# Mean   :0.6932  
# 3rd Qu.:1.0000  
# Max.   :1.0000  

model4 <- lm(price~bdrms+log(lotsize)+log(sqrft)+colonial, data=context3)
model5 <- lm(log(price)~bdrms+log(lotsize)+log(sqrft)+colonial, data=context3)

summary(model4)
summary(model5)

# A. When the lot size increases by 1% the price goes up by 61.446 dollars
# B. When the lot size increase by 1 percent the house size goes up by .16782x100 percent
# C. When the house is a colonial the price goes up by 4.134
# D. Model 4 fits better because homes are usually measured in price. we don't care about percent change of a home.
# a more expensive home will have less percentage changes than a cheaper priced home
# E. The price of the home would increase by 41,1318.00 dollars which is greater than 20k so we are adding the room.
#price <- (1*18.572)+(10*(225.598/100))
#41.1318
#########################################################

# Question 4

#########################################################

context4 <- fread('JTRAIN2.csv')
summary(context4)
# train             age             educ          black             hisp        
# Min.   :0.0000   Min.   :17.00   Min.   : 3.0   Min.   :0.0000   Min.   :0.00000  
# 1st Qu.:0.0000   1st Qu.:20.00   1st Qu.: 9.0   1st Qu.:1.0000   1st Qu.:0.00000  
# Median :0.0000   Median :24.00   Median :10.0   Median :1.0000   Median :0.00000  
# Mean   :0.4157   Mean   :25.37   Mean   :10.2   Mean   :0.8337   Mean   :0.08764  
# 3rd Qu.:1.0000   3rd Qu.:28.00   3rd Qu.:11.0   3rd Qu.:1.0000   3rd Qu.:0.00000  
# Max.   :1.0000   Max.   :55.00   Max.   :16.0   Max.   :1.0000   Max.   :1.00000  
# married          nodegree        mosinex           re74              re75       
# Min.   :0.0000   Min.   :0.000   Min.   : 5.00   Min.   : 0.0000   Min.   : 0.000  
# 1st Qu.:0.0000   1st Qu.:1.000   1st Qu.:14.00   1st Qu.: 0.0000   1st Qu.: 0.000  
# Median :0.0000   Median :1.000   Median :21.00   Median : 0.0000   Median : 0.000  
# Mean   :0.1685   Mean   :0.782   Mean   :18.12   Mean   : 2.1023   Mean   : 1.377  
# 3rd Qu.:0.0000   3rd Qu.:1.000   3rd Qu.:23.00   3rd Qu.: 0.8244   3rd Qu.: 1.221  
# Max.   :1.0000   Max.   :1.000   Max.   :24.00   Max.   :39.5707   Max.   :25.142  
# re78            unem74           unem75           unem78      
# Min.   : 0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
# 1st Qu.: 0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
# Median : 3.702   Median :1.0000   Median :1.0000   Median :0.0000  
# Mean   : 5.301   Mean   :0.7326   Mean   :0.6494   Mean   :0.3079  
# 3rd Qu.: 8.125   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
# Max.   :60.308   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000 

model6 <- lm(re78~re75+train+educ+black, data=context4)

summary(model6)

#A. real earning of 75 increase by 1 real earnings of 78 increase by .14697
#B. if training is 1 earnings for 78 increase by 1.68422 or 1684.22 dollars. it is of moderate significance 
#C. If the employee is black they are expected to make -2.11277 or 2112.77 dollars less :( 

