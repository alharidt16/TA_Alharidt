
library(readxl)
library(factoextra)
library(cluster)
library(klaR)
DATA_TA <- read.csv("Dataclean.csv")
DATA_TA
#Data tanpa duplikat
DATA_TA<- unique(DATA_TA)

MYDATA=DATA_TA

DATA_TA=DATA_TA[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]
str(DATA_TA)

#Kmodes

k_modes=kmodes(DATA_TA, 5, iter.max = 100, weighted = FALSE, fast = TRUE)
k_modes$cluster

MYDATA$cluster_kmodes<-k_modes$cluster
View(MYDATA)

#Kmedoids
#k_medoids=pam(DATA_TA, 5, metric = "manhattan", stand = FALSE)

#k_medoids$clustering
#k_medoids$medoids
#MYDATA$cluster_kmedoids<-k_medoids$clustering

View(MYDATA)




#eksplor ke excel
library(writexl)
write_xlsx(MYDATA, path="MYDATA.xlsx")