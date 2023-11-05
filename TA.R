
library(readxl)
library(factoextra)
library(cluster)
DATA_TA <- read.csv("Data_gabung _filter 2.csv")
#Data tanpa duplikat
DATA_TA<- unique(DATA_TA)

MYDATA=DATA_TA
DATA_TA=DATA_TA[,c(-1,-2,-38)]
str(DATA_TA)

k_medoids=pam(DATA_TA, 5, metric = "manhattan", stand = FALSE)

k_medoids$clustering
k_medoids$medoids

"""
#SAMPLE

sampel2 <- read_excel("sampel2.xlsx", sheet = "Sheet2")
k_medoids_sample=pam(sampel2, 5, metric = "manhattan", stand = FALSE)
k_medoids_sample


MYDATA$cluster_kmedian<-k_medoids$clustering
View(MYDATA)


#eksplor ke excel
library(writexl)
write_xlsx(MYDATA, path="DATA_kmedoids.xlsx")

""""
