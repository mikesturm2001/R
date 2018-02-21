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

attendrt <- context1$attend/32
hwrt <- context1$hw/8

model1 <- lm(termGPA~priGPA+ACT+attendrt+hwrt, data=context1)

summary(model1)

# Call:
#   lm(formula = termGPA ~ priGPA + ACT + attendrt + hwrt, data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.87210 -0.28100  0.00001  0.30164  1.49711 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.286983   0.164169  -7.839 1.77e-14 ***
#   priGPA       0.548962   0.042418  12.942  < 2e-16 ***
#   ACT          0.036099   0.006051   5.966 3.92e-09 ***
#   attendrt     1.052246   0.155436   6.770 2.81e-11 ***
#   hwrt         0.913031   0.116932   7.808 2.22e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4788 on 675 degrees of freedom
# Multiple R-squared:   0.58,	Adjusted R-squared:  0.5775 
# F-statistic:   233 on 4 and 675 DF,  p-value: < 2.2e-16


#coefs = summary(model1)$coefficients
#intercept <- coefs[1,1]
#priGPA <- coefs[2,1]
#ACT <- coefs[3,1]
#attendrt <- coefs[4,1]
#hw <- coefs[5,1]

sum(c(1,2.2,32,28/32,8/8)*coef(model1))

(3.2*0.548962)
(32*0.036099)
((28/32)*1.052246)
((8/8)*0.913031)

GPA <- (2.2*0.548962) + (32*0.036099) + ((28/32)*1.052246) + ((8/8)*0.913031) - 1.286983
GPA2 <- (3.9*0.548962) + (20*0.036099) + ((28/32)*1.052246) + ((8/8)*0.913031) - 1.286983
# coefficients(model1) # model coefficients

# A. term GPA changes by .032 for each class attended or 1.052246 on attendance rate
# termGPA increases by .114 for each homework turned in or .913031 on hw completion rate
# For interpertations i used the model without the rates
# C. (2.91) GPA <- (2.2*0.548962) + (32*0.036099) + ((28/32)*1.052246) + ((8/8)*0.913031) - 1.286983 
# D. (3.41) GPA <- (3.9*0.548962) + (20*0.036099) + ((28/32)*1.052246) + ((8/8)*0.913031) - 1.286983
# E. PriGPA is more important than the ACT because it has a larger coefficient
# F. (2.77) GPA <- (3.0*0.548962) + (25*0.036099) + ((32/32)*1.052246) + ((4/8)*0.913031) - 1.286983
# G. (2.70) GPA <- (3.0*0.548962) + (25*0.036099) + ((16/32)*1.052246) + ((8/8)*0.913031) - 1.286983
# H. Attendance is slighly higher than homework completion
# I. Homework and Attendance are directly correlated to the termGPA. PriGPA and ACT are not.
# the units are the same

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

# Call:
#   lm(formula = log(salary) ~ log(mktval) + profits + ceoten, data = context2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.63382 -0.34660  0.00627  0.35059  1.96220 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.7095052  0.3954502  11.909  < 2e-16 ***
#   log(mktval) 0.2386220  0.0559166   4.267 3.25e-05 ***
#   profits     0.0000793  0.0001566   0.506   0.6132    
# ceoten      0.0114646  0.0055816   2.054   0.0415 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5289 on 173 degrees of freedom
# Multiple R-squared:  0.2514,	Adjusted R-squared:  0.2384 
# F-statistic: 19.36 on 3 and 173 DF,  p-value: 7.141e-11

summary(model3)

# Call:
#   lm(formula = log(salary) ~ log(mktval) + profits + ceoten + log(sales), 
#      data = context2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.48792 -0.29369  0.00827  0.29951  1.85524 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.558e+00  3.803e-01  11.986  < 2e-16 ***
#   log(mktval) 1.018e-01  6.303e-02   1.614   0.1083    
# profits     2.905e-05  1.503e-04   0.193   0.8470    
# ceoten      1.168e-02  5.342e-03   2.187   0.0301 *  
#   log(sales)  1.622e-01  3.948e-02   4.109 6.14e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5062 on 172 degrees of freedom
# Multiple R-squared:  0.3183,	Adjusted R-squared:  0.3024 
# F-statistic: 20.08 on 4 and 172 DF,  p-value: 1.387e-13

# A. LN give you the percentage change. 
# We do not take the Log of the profits because we don't want to measure the percent change in profits to 
# measure the salary of the CEO. We want to measure by how much an x increase in  profits increases the salary percentage.
# since profits can often go postive and negative and dramatically increase.
# B. When the market value of the company increases by 1 percent the CEOs salary increases by .2386 percent
# C. When the market value of the company increases by 1 percent the CEOs salary increases by .1018 percent
# D. omitted variable bias. since we left out the sales in model2 
# E. the coefficient is still significant for large profits since we did not take the ln of profits
# Since some profits are 4 digits it becomes signficant
# F. for every 1 percent increase in sales the CEOs salary increases by .1622 percent

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

# Call:
#   lm(formula = price ~ bdrms + log(lotsize) + log(sqrft) + colonial, 
#      data = context3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -109.603  -38.258   -4.325   22.984  220.766 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -2030.455    210.967  -9.625 3.68e-15 ***
#   bdrms           18.572      9.308   1.995   0.0493 *  
#   log(lotsize)    61.446     12.372   4.966 3.60e-06 ***
#   log(sqrft)     225.508     30.072   7.499 6.41e-11 ***
#   colonial         4.134     14.509   0.285   0.7764    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 59.66 on 83 degrees of freedom
# Multiple R-squared:  0.6781,	Adjusted R-squared:  0.6626 
# F-statistic: 43.71 on 4 and 83 DF,  p-value: < 2.2e-16

summary(model5)

# Call:
#   lm(formula = log(price) ~ bdrms + log(lotsize) + log(sqrft) + 
#        colonial, data = context3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.69479 -0.09750 -0.01619  0.09151  0.70228 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.34959    0.65104  -2.073   0.0413 *  
#   bdrms         0.02683    0.02872   0.934   0.3530    
# log(lotsize)  0.16782    0.03818   4.395 3.25e-05 ***
#   log(sqrft)    0.70719    0.09280   7.620 3.69e-11 ***
#   colonial      0.05380    0.04477   1.202   0.2330    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1841 on 83 degrees of freedom
# Multiple R-squared:  0.6491,	Adjusted R-squared:  0.6322 
# F-statistic: 38.38 on 4 and 83 DF,  p-value: < 2.2e-16

# A. When the lot size increases by 100% the price goes up by 61.446x1000 dollars
# B. When the lot size increase by 1 percent the house size goes up by .16782 percent
# C. When the house is a colonial the price goes up by 4.134x(100)
# D. Model 4 fits better because homes are usually measured in price. we don't care about percent change of a home.
# a more expensive home will have less percentage changes than a cheaper priced home
# NEED TO LOOK AT THE R SQUARED
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

# Call:
#   lm(formula = re78 ~ re75 + train + educ + black, data = context4)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -9.120 -4.377 -1.756  3.353 54.058 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  1.97686    1.89028   1.046   0.2962   
# re75         0.14697    0.09811   1.498   0.1349   
# train        1.68422    0.62700   2.686   0.0075 **
#   educ         0.41026    0.17267   2.376   0.0179 * 
#   black       -2.11277    0.82941  -2.547   0.0112 * 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 6.496 on 440 degrees of freedom
# Multiple R-squared:  0.04917,	Adjusted R-squared:  0.04053 
# F-statistic: 5.688 on 4 and 440 DF,  p-value: 0.00018

#A. real earning of 75 increase by 1 real earnings of 78 increase by .14697
#B. if training is 1 earnings for 78 increase by 1.68422 or 1684.22 dollars. it is of moderate significance 
#C. If the employee is black they are expected to make -2.11277 or 2112.77 dollars less :( 

