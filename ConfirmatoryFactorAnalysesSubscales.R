# Confirmatory Factor Analyses subscales #
## Performed on 4 relationship types (P-P, P-C, C-P, C-C) ##
## Performed separately on the candidate items of the financial, practical
## and emotional subscale ##

library(lavaan)
library(semTools)
library(semPlot)
library(psych)


finsol_data_efa<- read.table("../Data/Preprocessed/finsol_item_asampling_efa.txt", quote="\"", comment.char="")
#Reverse score items
finsol_data_efa$Q24<-(100-finsol_data_efa$Q24)
finsol_data_efa$Q27<-(100-finsol_data_efa$Q27)
finsol_data_efa$Q28<-(100-finsol_data_efa$Q28)

finsol_data_efa[,2:11] <- sapply(finsol_data_efa[,2:11],as.numeric)

#Transform to 1-10 Liket scale
finsol_datrecodea_efa<-Recode(as.matrix(finsol_data_efa[,-1]), "0:10=1; 11:20=2; 21:30=3; 31:40=4; 41:50=5; 51:60=6; 
              61:70=7; 71:80=8; 81:90=9; 91:100=10")
finsol_datrecodea_efa<-as.data.frame(finsol_datrecodea_efa)
finsol_datrecodea_efa<-cbind(finsol_data_efa[,1],finsol_datrecodea_efa)
# Perform one-factor CFA for relationship type P-P
finsol_A<- ' f =~ Q21 + Q22 + Q24 + Q25 '
fit <- cfa(finsol_A, data=finsol_datrecodea_efa,estimator="MLR",missing="FIML")
summary(fit,fit.measures=TRUE,standardized = TRUE)
# Calculate reliability indices
semTools::reliability(fit)
psych::alpha(finsol_datrecodea_efa[,c(2,3,5,6)]) 

## Repeat process 12 times (4 relationship types on 3 dimensions
## functional solidarity)