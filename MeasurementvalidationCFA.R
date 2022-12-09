# Measurement model validation #
## Performed separately on the four relationship types (P-P, C-C, P-C, C-P)  ##

library(lavaan)
library(semTools)
library(semPlot)
library(psych)

## P-P relationship type

afin<-read.table("../Data/Preprocessed/finsol_datrecodea_cfa.txt")
aprak<-read.table("../Data/Preprocessed/praksol_datrecodea_cfa.txt")
aem<-read.table("../Data/Preprocessed/emsol_datrecodea_cfa.txt")
A_full<-cbind(afin,aprak,aem)

psych::alpha(afin[,c(2,3,5,6)])
psych::alpha(aprak[,c(5,7,8,10)])
psych::alpha(aem[,c(5,8,9,10)])

model1A_h<-'
fin =~ Q21 + Q22 + Q24 + Q25
prak =~ Q34 + Q36 + Q37 + Q39
em =~ Q43 + Q46 + Q47 + Q48
sol =~ fin + prak + em
'
fit2<- cfa(model1A_h, data = A_full, missing = "fiml", bounds = T, estimator = "MLR")
summary(fit2, fit.measures = T, standardize = T)

semTools::reliability(fit2)
compRelSEM(fit2, higher = "sol")

## C-C relationship type

bfin<-read.table("../Data/Preprocessed/finsol_datrecodeb_cfa.txt")
bprak<-read.table("../Data/Preprocessed/praksol_datrecodeb_cfa.txt")
bem<-read.table("../Data/Preprocessed/emsol_datrecodeb_cfa.txt")
B_full<-cbind(bfin,bprak,bem)

psych::alpha(bfin[,c(2,3,5,6)])
psych::alpha(bprak[,c(5,7,8,10)])
psych::alpha(bem[,c(5,8,9,10)])


model1B_h<-'
fin =~ Q21 + Q22 + Q24 + Q25
prak =~ Q34 + Q36 + Q37 + Q39
em =~ Q43 + Q46 + Q47 + Q48
sol =~ fin + prak + em
'
fit2<- cfa(model1B_h, data = B_full, missing = "fiml", bounds = T, estimator = "MLR")
summary(fit2, fit.measures = T, standardize = T)


reliability(fit2)
compRelSEM(fit2, higher = "sol")

## P-C relationship type

cfin<-read.table("../Data/Preprocessed/finsol_datrecodec_cfa.txt")
cprak<-read.table("../Data/Preprocessed/praksol_datrecodec_cfa.txt")
cem<-read.table("../Data/Preprocessed/emsol_datrecodec_cfa.txt")
C_full<-cbind(cfin,cprak,cem)

psych::alpha(cfin[,c(2,3,5,6)])
psych::alpha(cprak[,c(5,7,8,10)])
psych::alpha(cem[,c(5,8,9,10)])

model1C_h<-'
fin =~ Q21 + Q22 + Q24 + Q25
prak =~ Q34 + Q36 + Q37 + Q39
em =~ Q43 + Q46 + Q47 + Q48
sol =~ fin + prak + em
'
fit2<- cfa(model1C_h, data = C_full, missing = "fiml", bounds = T, estimator = "MLR")
summary(fit2, fit.measures = T, standardize = T)


reliability(fit2)
compRelSEM(fit2, higher = "sol")

## C-P relationship type

dfin<-read.table("../Data/Preprocessed/finsol_datrecoded_cfa.txt")
dprak<-read.table("../Data/Preprocessed/praksol_datrecoded_cfa.txt")
dem<-read.table("../Data/Preprocessed/emsol_datrecoded_cfa.txt")
# add 1 row because of missing observation
dprak[nrow(dprak)+1,] <- NA
D_full<-cbind(dfin,dprak,dem)


psych::alpha(dfin[,c(2,3,5,6)])
psych::alpha(dprak[,c(5,7,8,10)])
psych::alpha(dem[,c(5,8,9,10)])

model1D_h<-'
fin =~ Q21 + Q22 + Q24 + Q25
prak =~ Q34 + Q36 + Q37 + Q39
em =~ Q43 + Q46 + Q47 + Q48
sol =~ fin + prak + em
'
fit2<- cfa(model1D_h, data = D_full, missing = "fiml", bounds = T, estimator = "MLR")
summary(fit2, fit.measures = T, standardize = T)


