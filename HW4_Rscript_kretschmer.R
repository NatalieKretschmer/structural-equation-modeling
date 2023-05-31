library(lavaan)
library(readxl)

## import data 
setwd("~/Desktop/SP23/path, factor analysis, etc/hw4")
data <- read_excel("RuminationData.xlsx")
#data <- data[, 2:14] 

cor <- cor(data)
head(cor)

## specify model 1: interconnected
model_1 <- '
rumination =~ rumi1 + rumi2 + rumi3 + rumi4
sadness =~ sad1 + sad2 + sad3 + sad4
sleep =~ sleep1 + sleep2 + sleep3
immunity =~ leukocyt + lymphocy
'

## fit
fit_1 <- cfa(model_1, sample.cov = cor, sample.nobs = 300)

## parameters
parTable(fit_1)
parameterestimates(fit_1)

## specify model 2: rumination as root cause, mediated model
model_2 <- '
rumination =~ rumi1 + rumi2 + rumi3 + rumi4
sadness =~ sad1 + sad2 + sad3 + sad4 
sleep =~ sleep1 + sleep2 + sleep3 
immunity =~ leukocyt + lymphocy
sleep ~ a*rumination
sadness ~ c*rumination
immunity ~ b*sleep + d*sadness
immunity_ind1 := a*b
immunity_ind2 := c*d
'  

## fit
fit_2 <- sem(model_2, data = data)

## parameters
parTable(fit_2)
parameterestimates(fit_2)


## summaries
summary(fit_1, fit.measures = T, standardized = T, ci = T, rsquare = T)
summary(fit_2, fit.measures = T, standardized = T, ci = T, rsquare = T)

