######################################
## Author:   Michael Sturm
## Date:     2018-01-16
## Title:    ps1.r
## Purpose:  Wage data homework assignment
######################################

rm(list=ls(all=TRUE)) #drop all variables

## Package import
######################################
library(data.table)

#ANALYSIS

# Read the data Wage1.csv into a new variable using the data.table package
context1 <- fread('WAGE1.csv')

#Use the summary statistics 
summary(context1)

lwage <- log(context1$wage)

#run the following linear model: wage = Bo + B1educ + e
model1 <- lm(wage~educ, data=context1)
summary(model1)
# Call:
#   lm(formula = wage ~ educ, data = context1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.3707 -2.1578 -0.9854  1.1864 16.3975 
# 
# Coefficients:
#             Estimate    Std. Error  t value   Pr(>|t|)    
# (Intercept) -0.93389    0.68769     -1.358    0.175    
# educ         0.54470    0.05346     10.189    <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.392 on 524 degrees of freedom
# Multiple R-squared:  0.1654,	Adjusted R-squared:  0.1638 
# F-statistic: 103.8 on 1 and 524 DF,  p-value: < 2.2e-16

############################################################################################################

# Plotting to visualize data
# plot_basic <- ggplot(context1, aes(x=educ,y=wage)) + geom_point() 
# plot_basic <- plot_basic + scale_x_continuous(name="education years") + scale_y_continuous(name="salary")
# plot_basic
# plot_ols   <- plot_basic + geom_line(aes(y=predict(model1)),color="red",size=2)
# plot_ols

############################################################################################################

#run the following linear model: wage = B0+ B1educ + B2exper + B3Tenure + e
model2 <- lm(wage~educ+exper+tenure, data=context1)
summary(model2)

# Call:
#   lm(formula = wage ~ educ + exper + tenure, data = context1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.6498 -1.7708 -0.6407  1.2051 14.7201 
# 
# Coefficients:
#             Estimate   Std. Error t value Pr(>|t|)    
# (Intercept) -2.91354   0.73172    -3.982  7.81e-05 ***
#   educ      0.60268    0.05148    11.708  < 2e-16 ***
#   exper     0.02252    0.01210    1.861   0.0633 .  
# tenure      0.17002    0.02173    7.825   2.83e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.096 on 522 degrees of freedom
# Multiple R-squared:  0.3072,	Adjusted R-squared:  0.3032 
# F-statistic: 77.15 on 3 and 522 DF,  p-value: < 2.2e-16

#run the following linear model: lwage = B0 + B1educ + B2exper + B3tenure + e

# using the log tracks everything as a perent change in wage #

model3 <- lm(lwage~educ+exper+tenure, data=context1)
summary(model3)

# Call:
#   lm(formula = log(wage) ~ educ + exper + tenure, data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.05911 -0.29563 -0.03302  0.28590  1.42657 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.282635   0.104331   2.709  0.00697 ** 
#   educ        0.092256   0.007340  12.569  < 2e-16 ***
#   exper       0.004137   0.001726   2.397  0.01687 *  
#   tenure      0.022112   0.003098   7.138 3.19e-12 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4415 on 522 degrees of freedom
# Multiple R-squared:  0.3165,	Adjusted R-squared:  0.3125 
# F-statistic: 80.56 on 3 and 522 DF,  p-value: < 2.2e-16


#INTERPRETATIONS
#a: as the education of a worker increases their wage increases by 0.54470 dollars per hour.
# the .54 suggest there is a strong correlation between the years of education and a worker's salary
#b: as the education of a worker increases their wage increases by 0.60268 dollars per hour
# when other factors are added to the regression the correlation between education and salary increases.
#c: as the experience of a worker increases their wage increases by 0.02252 dollars per hour
# the .022 shows there isn't a strong correlation between experience and education. Education is more important 
# than experience for workers seeking a higher salary.
#d: as the tenure of a worker increases their wage increases by 0.17002 dollars per hour
# tenure is .17 which suggests there is some correlation between tenure and a higher wage. 
#e: The intercept means that people with no education and experience will not be making money and 
#   be a liability instead. hence why there is a  -2.91354 intercept. this makes sense as someone with no education 
# experience or tenure would not be making money.
#f: for every year of education a worker has their wage increases by 0.092256 or 9.2% increase
#g: for every year of experience a worker has their wage increases by 0.004137 or .4% increase 
#h: for every year of tenure a worker has their wage increases by 0.022112 or 2.2% increase


