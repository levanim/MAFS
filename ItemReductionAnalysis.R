# Item Reduction Analysis #
## Performed on 4 relationship types (P-P, P-C, C-P, C-C) ##

library(ggplot2)
library(psych)
library(lavaan)
library(egg)
library(psych)
library(corrplot)
library(car)
library(factoextra)
library(writexl)
library(semPlot)


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

# Plot distributions
for (i in c(2:11)){
  hist(finsol_datrecodea_efa[,i],xlim = c(1,10),breaks=1*(1:10), main = colnames(finsol_datrecodea_efa)[i])  
}

#Calculate descriptives, including Cronbach's alpha on all candidate items
psych::alpha(finsol_datrecodea_efa[,c(2:11)]) 
psych::describe(finsol_datrecodea_efa[,c(2:11)]) 


## Repeat script for B, C , D samples