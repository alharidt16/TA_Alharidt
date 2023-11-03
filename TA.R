
library(readxl)
library(factoextra)
library(cluster)
DATA_TA <- read_excel("Data_gabung _filter 2.xlsx")
#Data tanpa duplikat
DATA_TA<- unique(DATA_TA)

MYDATA=DATA_TA
DATA_TA=DATA_TA[,c(-1,-2,-38)]
str(DATA_TA)

k_medoids=pam(DATA_TA, 5, metric = "manhattan", stand = FALSE)
pam(DATA_TA, 5, metric = "manhattan", stand = FALSE)






"""

MYDATA$cluster_kmedian<-k_median$bestresult$cluster
View(MYDATA)


#eksplor ke excel
library(writexl)
write_xlsx(MYDATA, path="DATA_kmedian.xlsx")

""""
