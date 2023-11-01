library(Kmedians)
library(readxl)

DATA <- read_excel("Data_gabung _filter 2.xlsx")
#Data tanpa duplikat
DATA<- unique(DATA)

MYDATA=DATA

DATA=DATA[,c(-1,-2,-38)]
str(DATA)


k_median=Kmedians(DATA,nclust=5,ninit=0,niter=100,method = 'Online', init=TRUE,par=TRUE)

View(k_median$bestresult$cluster)
View(k_median$allresults$centers)

MYDATA$cluster_kmedian<-k_median$bestresult$cluster
View(MYDATA)


#eksplor ke excel
library(writexl)
write_xlsx(MYDATA, path="DATA_kmedian.xlsx")
