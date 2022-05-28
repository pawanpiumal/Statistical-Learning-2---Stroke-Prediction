#libraries
library(dplyr)
library(ggplot2)
library(glmnet)
library(caret)
library(vip)
library(randomForest)
library(xgboost)
library(leaps)
library(unbalanced)
library(pROC)
library(tibble)
library(ROSE)
library(beepr)
library(plumber)
setwd("C:\\Users\\pawan\\Downloads\\3rd Year 2nd Semester - UOC\\ST3082 - Statistical Learning I\\Project2\\Project 2\\Project\\Backend")

model = readRDS('model.RDS')
train = readRDS('train.RDS')


pr("plumber.R") %>%
  pr_run(port=8000)

