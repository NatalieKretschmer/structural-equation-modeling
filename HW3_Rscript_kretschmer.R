library(lavaan)

## raw data route 
#setwd("~")
#data <- read_csv(#INSERT PATH HERE)

## corrtable route
cor_raw <- '
1																	
0.68	1																
0.64	0.67	1															
0.59	0.69	0.62	1														
0.58	0.69	0.57	0.82	1													
0.54	0.52	0.72	0.45	0.45	1												
0.6	0.58	0.58	0.55	0.5	0.45	1											
0.57	0.53	0.51	0.45	0.43	0.49	0.76	1										
0.45	0.42	0.44	0.45	0.42	0.35	0.72	0.6	1									
0.48	0.42	0.45	0.47	0.46	0.33	0.66	0.56	0.73	1								
0.47	0.46	0.42	0.46	0.43	0.29	0.76	0.66	0.74	0.77	1							
0.52	0.48	0.48	0.48	0.43	0.37	0.79	0.72	0.75	0.75	0.85	1						
0.62	0.64	0.61	0.56	0.55	0.47	0.54	0.5	0.45	0.43	0.47	0.49	1					
0.63	0.63	0.65	0.61	0.61	0.49	0.53	0.48	0.45	0.44	0.46	0.47	0.77	1				
0.65	0.63	0.66	0.6	0.6	0.52	0.56	0.51	0.46	0.48	0.48	0.51	0.77	0.9	1			
0.58	0.57	0.55	0.51	0.51	0.5	0.52	0.49	0.48	0.46	0.48	0.53	0.69	0.69	0.72	1		
0.61	0.64	0.56	0.56	0.58	0.46	0.51	0.48	0.42	0.4	0.45	0.48	0.73	0.76	0.74	0.68	1	
0.65	0.64	0.64	0.59	0.6	0.51	0.57	0.54	0.47	0.49	0.53	0.52	0.71	0.8	0.83	0.7	0.8	1
'

cor <- getCov(cor_raw, names=c("JE1_p1", "JE2_p2", "JE3_p3", "JE4_p4", "JE5_p5", "JE6_p6", "JE7_e1", "JE8_e2", "JE9_e3", "JE10_e4", "JE11_e5", "JE12_e6", "JE13_c1", "JE14_c2", "JE15_c3", "JE16_c4", "JE17_c5", "JE18_c6"
))
cor

## specify model 1: 1 factor "JE" on all items
model_1 <- '
JE =~ JE1_p1+JE2_p2+JE3_p3+JE4_p4+JE5_p5+JE6_p6+JE7_e1+JE8_e2+JE9_e3+JE10_e4+JE11_e5+JE12_e6+JE13_c1+JE14_c2+JE15_c3+JE16_c4+JE17_c5+JE18_c6
'

## fit
fit_1 <- cfa(model_1, sample.cov = cor, sample.nobs = 512)

## parameters
parTable(fit_1)
parameterestimates(fit_1)

## specify model 2: [three correlated factors will be examined, where the factor
#  “Physical” will load on items JE1_p1, JE2_p2, JE3_p3, JE4_p4, JE5_p5, and 
#  JE6_p6, and the factor “Emotional” will load on items JE7_e1, JE8_e2, JE9_e3, 
#  JE10_e4, JE11_e5, and JE12_e6, and factor “Cognitive” will load on items 
#  JE13_c1, JE14_c2, JE15_c3, JE16_c4, JE17_c5, and JE18_c6]
model_2 <- '
Physical =~ JE1_p1 + JE2_p2 + JE3_p3+ JE4_p4 + JE5_p5 + JE6_p6
Emotional =~ JE7_e1 + JE8_e2 + JE9_e3+ JE10_e4 + JE11_e5 + JE12_e6
Cognitive =~ JE13_c1 + JE14_c2 + JE15_c3 + JE16_c4 + JE17_c5 + JE18_c6
'  

## fit
fit_2 <- cfa(model_2, sample.cov = cor, sample.nobs = 512)

## parameters
parTable(fit_2)
parameterestimates(fit_2)


## summaries
summary(fit_1, fit.measures = T, standardized = T, ci = T)
summary(fit_2, fit.measures = T, standardized = T, ci = T)

