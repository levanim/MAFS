# Exploratory Factor Analyses #
## Performed on 4 relationship types (P-P, P-C, C-P, C-C) ##
## Performed separately on the candidate items of the financial, practical
## and emotional subscale ##
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
library(paran)


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
a_cors <- cor(finsol_datrecodea_efa[,c(2,3,5:11)],use="complete.obs")
# visualisize correlations within items of same subscale
corrplot(a_cors, method="number")


# paran to check how many factor
paran(finsol_datrecodea_efa[,c(2,3,5:11)], iterations = 5000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)
fa.parallel(a_cors)
# eigenvalues

# Then use that correlation matrix to calculate eigenvalues
eigenvals <- eigen(a_cors)

# Look at the eigenvalues returned
eigenvals$values


# scree plot
scree(a_cors, factors = F)

# 1 to 2 factor solution
# perform 1-factor solution
EFA_model <- fa(a_cors, nfactors = 1, rotate = "oblimin",fm="pa")
print(EFA_model)
summary(EFA_model)
EFA_model
EFA_model$loadings
a_fin<-cor(finsol_datrecodea_efa[,c(2,3,5,6)],use="complete.obs")



KMO(
  a_cors)

## Repeat process 12 times (4 relationship types on 3 dimensions
## functional solidarity)