#---------------------------------------------------------#
# Spring 2023                                             #
# PSYC 86001 SEM                                          #
# Assignment 2 -- Kretschmer                              #
#---------------------------------------------------------#
library(lavaan)

#### Step 1: Loading the data

cor.tri <- '
1.00
0.77     1.00
0.25     0.26     1.00
0.57     0.52     0.37     1.00
'

cor.table <- getCov(cor.tri, names=c("Parenting", "Teaching", "Learning", "ExamPerf"))
cor.table

#### Step 2: Specifying Model 1
# Specify path model as structural equation model (without error)
# e.g., Y = aX1 + bX2 + cX3

path.model1 <- '
Learning ~ a*Parenting + b*Teaching
ExamPerf ~ c*Learning
indParent := a*c
indTeaching := b*c
'

#### Step 3: Fitting and Estimating Model 1
path.fit1 <- sem(path.model1, sample.cov = cor.table, sample.nobs = 1000)

#### Step 4: Summarizing the Results of Model 1
## Extract parameters 
parTable(path.fit1)

# summary of results
summary(path.fit1, rsquare = T, standardized = T, ci = T)

# Return all parameter estimates
# parameter estimates
parameterEstimates(path.fit1)

#?parameterEstimates
#?standardizedSolution

# Return standardized parameter estimates
# parameter estimates
standardizedSolution(path.fit1)

####################################################
#### Step 5: Specifying Model 2
path.model2 <- '
Learning ~ a*Parenting + b*Teaching
ExamPerf ~ c*Learning + d*Parenting + e*Teaching
indParent := a*c
indTeaching := b*c
'

#### Step 6: Fitting and Estimating Model 2
path.fit2 <- sem(path.model2, sample.cov = cor.table, sample.nobs = 1000)

#### Step 7: Summarizing the Results of Model 2
## Extract parameters 
parTable(path.fit2)

# summary of results
summary(path.fit2, rsquare = T, standardized = T, ci = T)

# Return all parameter estimates
# parameter estimates
parameterEstimates(path.fit2)

# Return standardized parameter estimates
# parameter estimates
standardizedSolution(path.fit2)

#### Step 8: Reporting the results of the two models in your submission 
