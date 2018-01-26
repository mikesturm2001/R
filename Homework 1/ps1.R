######################################
## Author:   Michael Sturm
## Date:     2018-01-16
## Title:    ps1.r
## Purpose:  Wage data homework assignment
######################################

rm(list=ls()) #drop all variables

## Package import
######################################
library(tidyverse)
library(data.table)

# Read the data Wage1.csv into a new variable using the data.table package
context1 <- fread('WAGE1.csv')

#Use the summary statistics 
summary(context1)

lwage <- 