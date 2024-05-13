#Load library
library(readxl) # membaca data excel
library(factoextra) # untuk kmedoids
library(cluster) # untuk kmedoids
library(klaR) # untuk kmodes
library(writexl) # untuk mengunduh data ke local
library(dplyr) # memudahkan function
library(caret) # # untuk confusion matrix
library(rpart) #untuk desicion tree
library(caTools) # untuk membagi data train dan test
library(rpart.plot) #visualisai
#Load Data
DATA_TA <- read.csv("Dataclean.csv")
DATA_TA
str(DATA_TA)
#Menghilangkan Data duplikat
DATA_TA<- unique(DATA_TA)

#Memilih Variabel yang akan digunakan
DATA_TA=DATA_TA[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26","Y")]
DATA_TA


#KMODES
#Data Tanpa Variabel Y untuk kmodes
DATA_TA_1=DATA_TA[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]
DATA_TA_1

#clustering menggunakan Kmodes
k_modes=kmodes(DATA_TA_1, 5, iter.max = 100, weighted = FALSE, fast = TRUE)
k_modes$cluster#Melihat label dari hasil cluster kmodes 
k_modes$modes #Pusat klaster
pusat_kmodes=k_modes$modes

#Menambahkan kolom label hasil kmodes
DATA_TA$cluster_kmodes<-k_modes$cluster
View(DATA_TA)

#KMEDOIDS
#Data Tanpa Variabel Y untuk kmedoids
DATA_TA_2=DATA_TA[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]
DATA_TA_2
#k_medoids=pam(DATA_TA_2, 5, metric = "manhattan", stand = FALSE)
k_medoids$clustering
k_medoids$medoids
pusat_kmedoids=k_medoids$medoids
pusat_kmedoids

DATA_TA$cluster_kmedoids<-k_medoids$clustering
DATA_TA


#eksplor ke excel
write_xlsx(DATA_TA, path="DATA_label.xlsx")


#Menentukan Pusat Klaster Awal dengan Modus
DATA_TA_A=DATA_TA[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26","Y")]
DATA_TA_A

DATA_TA_A_5=filter(DATA_TA_A, Y==5)
DATA_TA_A_5=DATA_TA_A_5[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]

DATA_TA_A_4=filter(DATA_TA_A, Y==4)
DATA_TA_A_4=DATA_TA_A_4[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]

DATA_TA_A_3=filter(DATA_TA_A, Y==3)
DATA_TA_A_3=DATA_TA_A_3[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]

DATA_TA_A_2=filter(DATA_TA_A, Y==2)
DATA_TA_A_2=DATA_TA_A_2[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]

DATA_TA_A_1=filter(DATA_TA_A, Y==1)
DATA_TA_A_1=DATA_TA_A_1[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]


#fungsi untuk mencari modus
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
} 
#Menentukan Pusat Klaster Awal dengan Modus
P_K1_A1=apply(DATA_TA_A_1, 2, find_mode) #pusat klaster 1 kategori awal untuk modus
P_K2_A1=apply(DATA_TA_A_2, 2, find_mode)
P_K3_A1=apply(DATA_TA_A_3, 2, find_mode)
P_K4_A1=apply(DATA_TA_A_4, 2, find_mode)
P_K5_A1=apply(DATA_TA_A_5, 2, find_mode)

#Menentukan Pusat Klaster Awal dengan Median
P_K1_A2=apply(DATA_TA_A_1, 2, median) #pusat klaster 1 kategori awal untuk median
P_K2_A2=apply(DATA_TA_A_2, 2, median)
P_K3_A2=apply(DATA_TA_A_3, 2, median)
P_K4_A2=apply(DATA_TA_A_4, 2, median)
P_K5_A2=apply(DATA_TA_A_5, 2, median)


# pelabelan klaster Kmodes
# M=Y, 1=1, 5=2, 4=3, 2=4, 3=5
#pusat klaster kmodes
P_K1_M1=pusat_kmodes[1,]
P_K2_M1=pusat_kmodes[5,]
P_K3_M1=pusat_kmodes[4,]
P_K4_M1=pusat_kmodes[2,]
P_K5_M1=pusat_kmodes[3,]

# pelabelan klaster Kmedoids
# M=Y, 5=1, 4=2, 3=3, 2=4, 1=5

#pusat klaster kmedoids
P_K1_M2=pusat_kmedoids[5,]
P_K2_M2=pusat_kmedoids[4,]
P_K3_M2=pusat_kmedoids[3,]
P_K4_M2=pusat_kmedoids[2,]
P_K5_M2=pusat_kmedoids[1,]

# mengubah nilai kolom Y, cluster_kmodes dan cluster_kmedoids
# Data Y
DATA_TA <- DATA_TA %>% 
  mutate(Y = recode(Y,
                                 "1" = "Sangat Miskin",
                                 "2" = "Miskin",
                                 "3" = "Hampir Miskin",
                                 "4" = "Cukup Miskin",
                                 "5" = "Tidak Miskin"))
# Data Kmodes
DATA_TA <- DATA_TA %>% 
  mutate(cluster_kmodes = recode(cluster_kmodes,
                                 "1" = "Sangat Miskin",
                                 "5" = "Miskin",
                                 "4" = "Hampir Miskin",
                                 "2" = "Cukup Miskin",
                                 "3" = "Tidak Miskin"))

# Data Kmedoids
DATA_TA <- DATA_TA %>% 
  mutate(cluster_kmedoids = recode(cluster_kmedoids,
                                 "5" = "Sangat Miskin",
                                 "4" = "Miskin",
                                 "3" = "Hampir Miskin",
                                 "2" = "Cukup Miskin",
                                 "1" = "Tidak Miskin"))

#merubah kembali menjadi angka

DATA_TA <- DATA_TA %>% 
  mutate(Y = recode(Y,
                    "Sangat Miskin"=1,
                    "Miskin"=2,
                    "Hampir Miskin"=3,
                    "Cukup Miskin"=4,
                    "Tidak Miskin"=5))
DATA_TA <- DATA_TA %>% 
  mutate(cluster_kmodes = recode(cluster_kmodes,
                                 "Sangat Miskin"=1,
                                 "Miskin"=2,
                                 "Hampir Miskin"=3,
                                 "Cukup Miskin"=4,
                                 "Tidak Miskin"=5))
DATA_TA <- DATA_TA %>% 
  mutate(cluster_kmedoids = recode(cluster_kmedoids,
                                   "Sangat Miskin"=1,
                                   "Miskin"=2,
                                   "Hampir Miskin"=3,
                                   "Cukup Miskin"=4,
                                   "Tidak Miskin"=5))


#Menjadikan faktor
DATA_TA$Y <- as.factor(DATA_TA$Y)
DATA_TA$cluster_kmodes <- as.factor(DATA_TA$cluster_kmodes)
DATA_TA$cluster_kmedoids <- as.factor(DATA_TA$cluster_kmedoids)

#Data Kmodes yang klasternya berbeda
DATA_TA_KMODES=DATA_TA[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26","Y","cluster_kmodes")]
OUTLIER_KMODES=DATA_TA_KMODES[DATA_TA_KMODES$Y != DATA_TA_KMODES$cluster_kmodes,]
str(OUTLIER_KMODES)
write.csv(OUTLIER_KMODES,"DATA_labelbeda_kmodes.csv", row.names = TRUE)
 
#Data Kmedoids yang klasternya berbeda
DATA_TA_KMEDOIDS=DATA_TA[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26","Y","cluster_kmedoids")]
OUTLIER_KMEDOIDS=DATA_TA_KMEDOIDS[DATA_TA_KMEDOIDS$Y != DATA_TA_KMEDOIDS$cluster_kmedoids,]
str(OUTLIER_KMEDOIDS)
write.csv(OUTLIER_KMEDOIDS,"DATA_labelbeda_kmedoids.csv", row.names = TRUE)


#jarak Hamming
hamming_dist <- function(a, b) {sum(a != b)}
#contoh
hamming_dist(P_K2_A1, OUTLIER_KMODES[1,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")])

#jarak manhattan
manhattan_dist <- function(a, b){
  dist <- abs(a-b)
  dist <- sum(dist)
  return(dist)
}

#contoh
manhattan_dist(P_K2_M1,OUTLIER_KMEDOIDS[10,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")] )
DATA_TA
OUTLIER_KMEDOIDS#[2,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]

#DATA SRT Sangat Miskin
DATA_TA_SM=filter(DATA_TA, Y=="Sangat Miskin")
DATA_TA_M=filter(DATA_TA, Y=="Miskin")
DATA_TA_HM=filter(DATA_TA, Y=="Hampir Miskin")
DATA_TA_CM=filter(DATA_TA, Y=="Cukup Miskin")
DATA_TA_TM=filter(DATA_TA, Y=="Tidak Miskin")



#CONFUSION MATRIX
# Membuat confusion matrix dengan fungsi confusionMatrix
CM_kmodes <- confusionMatrix(data = DATA_TA$cluster_kmodes, reference = DATA_TA$Y)
CM_kmedoids <- confusionMatrix(data = DATA_TA$cluster_kmedoids, reference = DATA_TA$Y)
CM_3 <- confusionMatrix(data = DATA_TA$cluster_kmedoids, reference = DATA_TA$cluster_kmodes)
# Menampilkan hasil confusion matrix
CM_kmodes
CM_kmedoids
CM_3

table(DATA_TA$cluster_kmodes,DATA_TA$Y)
table(DATA_TA$cluster_kmedoids, DATA_TA$Y)
table(DATA_TA$cluster_kmedoids, DATA_TA$cluster_kmodes)

#Evaluasi perbedaan kelas Y dan hasil klaster
#Melakukan evaluasi
#Y dan kmodes
akurasi_bedakmodes=confusionMatrix(data=DATA_TA$cluster_kmodes, reference = DATA_TA$Y)$overall[1]#akurasi
presisi_bedakmodes=confusionMatrix(data=DATA_TA$cluster_kmodes, reference = DATA_TA$Y)$byClass[1]#presisi
recal_bedakmodes=confusionMatrix(data=DATA_TA$cluster_kmodes, reference = DATA_TA$Y)$byClass[2]#recal
F1_bedakmodes = 2 * (presisi_bedakmodes * recal_bedakmodes) / (presisi_bedakmodes+ recal_bedakmodes) #F1
akurasi_bedakmodes
presisi_bedakmodes
recal_bedakmodes
F1_bedakmodes


#Y dan kmedoids
akurasi_bedakmedoids=confusionMatrix(data=DATA_TA$cluster_kmedoids, reference = DATA_TA$Y)$overall[1]#akurasi
presisi_bedakmedoids=confusionMatrix(data=DATA_TA$cluster_kmedoids, reference = DATA_TA$Y)$byClass[1]#presisi
recal_bedakmedoids=confusionMatrix(data=DATA_TA$cluster_kmedoids, reference = DATA_TA$Y)$byClass[2]#recal
F1_bedakmedoids = 2 * (presisi_bedakmedoids * recal_bedakmedoids) / (presisi_bedakmedoids+ recal_bedakmedoids) #F1
akurasi_bedakmedoids
presisi_bedakmedoids
recal_bedakmedoids
F1_bedakmedoids

#kmodes dan kmedoids
akurasi_beda3=confusionMatrix(data=DATA_TA$cluster_kmedoids, reference = DATA_TA$cluster_kmodes)$overall[1]#akurasi
presisi_beda3=confusionMatrix(data=DATA_TA$cluster_kmedoids, reference = DATA_TA$cluster_kmodes)$byClass[1]#presisi
recal_beda3=confusionMatrix(data=DATA_TA$cluster_kmedoids, reference = DATA_TA$cluster_kmodes)$byClass[2]#recal
F1_beda3 = 2 * (presisi_beda3 * recal_beda3) / (presisi_beda3+ recal_beda3) #F1
akurasi_beda3
presisi_beda3
recal_beda3
F1_beda3


# DESICION TREE
#membagi data training dan testing
set.seed(101)
splitdata <- sample.split(DATA_TA$Y, SplitRatio = 0.7) # training 70%
data.training <- subset(DATA_TA, splitdata==T)
data.testing <- subset(DATA_TA, splitdata==F)

#melihat dimensi data
dim(data.training)
dim(data.testing)


#membuat model desicion tree
#variabel Y
DT_Y=rpart(Y~X1+X15+X10+X13+X4+X14+X19+X11+X16+X5+X26, method = "class", data = data.training)
printcp(DT_Y)
#variabel kmodes
DT_kmodes=rpart(cluster_kmodes~X1+X15+X10+X13+X4+X14+X19+X11+X16+X5+X26, method = "class", data = data.training)
printcp(DT_kmodes)
#variabel kmedoids
DT_kmedoids=rpart(cluster_kmedoids~X1+X15+X10+X13+X4+X14+X19+X11+X16+X5+X26, method = "class", data = data.training)
printcp(DT_kmedoids)

#visualisasi
prp(DT_Y)
prp(DT_kmodes)
prp(DT_kmedoids)

#Prdiksi
prediksi_Y <- predict(DT_Y, newdata = data.testing, type = "class")
prediksi_kmodes <- predict(DT_kmodes, newdata = data.testing, type = "class")
prediksi_kmedoids <- predict(DT_kmedoids, newdata = data.testing, type = "class")

#confusion matrix
table(prediksi_Y,data.testing$Y)
table(prediksi_kmodes,data.testing$cluster_kmodes)
table(prediksi_kmedoids,data.testing$cluster_kmedoids)


#Melakukan evaluasi
#Y
akurasi_Y=confusionMatrix(data=prediksi_Y, reference = data.testing$Y)$overall[1]#akurasi
presisi_Y=confusionMatrix(data=prediksi_Y, reference = data.testing$Y)$byClass[1]#presisi
recal_Y=confusionMatrix(data=prediksi_Y, reference = data.testing$Y)$byClass[2]#recal
F1_Y = 2 * (presisi_Y * recal_Y) / (presisi_Y+ recal_Y) #F1
akurasi_Y
presisi_Y
recal_Y
F1_Y

#Kmodes
akurasi_kmodes=confusionMatrix(data=prediksi_kmodes, reference = data.testing$cluster_kmodes)$overall[1]#akurasi
presisi_kmodes=confusionMatrix(data=prediksi_kmodes, reference = data.testing$cluster_kmodes)$byClass[1]#presisi
recal_kmodes=confusionMatrix(data=prediksi_kmodes, reference = data.testing$cluster_kmodes)$byClass[2]#recal
F1_kmodes = 2 * (presisi_kmodes * recal_kmodes) / (presisi_kmodes+ recal_kmodes) #F1
akurasi_kmodes
presisi_kmodes
recal_kmodes
F1_kmodes


#Kmedoids
akurasi_kmedoids=confusionMatrix(data=prediksi_kmedoids, reference = data.testing$cluster_kmedoids)$overall[1]#akurasi
presisi_kmedoids=confusionMatrix(data=prediksi_kmedoids, reference = data.testing$cluster_kmedoids)$byClass[1]#presisi
recal_kmedoids=confusionMatrix(data=prediksi_kmedoids, reference = data.testing$cluster_kmedoids)$byClass[2]#recal
F1_kmedoids = 2 * (presisi_kmedoids * recal_kmedoids) / (presisi_kmedoids+ recal_kmedoids) #F1
akurasi_kmedoids
presisi_kmedoids
recal_kmedoids
F1_kmedoids


DT_Y$variable.importance
DT_kmodes$variable.importance
DT_kmedoids$variable.importance


#Membagi data yang beda
#Kmodes
OUTLIER_KMODES
# Y = 1
DATA_beda_MODES_Y1=filter(OUTLIER_KMODES, Y==1)
# Y=1 dan K1=2
DATA_beda_MODES_Y1_M2=filter(DATA_beda_MODES_Y1,cluster_kmodes==2)
# Y=1 dan K1=3
DATA_beda_MODES_Y1_M3=filter(DATA_beda_MODES_Y1,cluster_kmodes==3)
# Y=1 dan K1=4
DATA_beda_MODES_Y1_M4=filter(DATA_beda_MODES_Y1,cluster_kmodes==4)
# Y=1 dan K1=5
DATA_beda_MODES_Y1_M5=filter(DATA_beda_MODES_Y1,cluster_kmodes==5)

# Y = 2
DATA_beda_MODES_Y2=filter(OUTLIER_KMODES, Y==2)
# Y=2 dan K1=1
DATA_beda_MODES_Y2_M1=filter(DATA_beda_MODES_Y2,cluster_kmodes==1)
# Y=2 dan K1=3
DATA_beda_MODES_Y2_M3=filter(DATA_beda_MODES_Y2,cluster_kmodes==3)
# Y=2 dan K1=4
DATA_beda_MODES_Y2_M4=filter(DATA_beda_MODES_Y2,cluster_kmodes==4)
# Y=2 dan K1=5
DATA_beda_MODES_Y2_M5=filter(DATA_beda_MODES_Y2,cluster_kmodes==5)

# Y = 3
DATA_beda_MODES_Y3=filter(OUTLIER_KMODES, Y==3)
# Y=3 dan K1=1
DATA_beda_MODES_Y3_M1=filter(DATA_beda_MODES_Y3,cluster_kmodes==1)
# Y=3 dan K1=2
DATA_beda_MODES_Y3_M2=filter(DATA_beda_MODES_Y3,cluster_kmodes==2)
# Y=3 dan K1=4
DATA_beda_MODES_Y3_M4=filter(DATA_beda_MODES_Y3,cluster_kmodes==4)
# Y=3 dan K1=5
DATA_beda_MODES_Y3_M5=filter(DATA_beda_MODES_Y3,cluster_kmodes==5)

# Y = 4
DATA_beda_MODES_Y4=filter(OUTLIER_KMODES, Y==4)
# Y=4 dan K1=1
DATA_beda_MODES_Y4_M1=filter(DATA_beda_MODES_Y4,cluster_kmodes==1)
# Y=4 dan K1=2
DATA_beda_MODES_Y4_M2=filter(DATA_beda_MODES_Y4,cluster_kmodes==2)
# Y=4 dan K1=3
DATA_beda_MODES_Y4_M3=filter(DATA_beda_MODES_Y4,cluster_kmodes==3)
# Y=4 dan K1=5
DATA_beda_MODES_Y4_M5=filter(DATA_beda_MODES_Y4,cluster_kmodes==5)

# Y = 5
DATA_beda_MODES_Y5=filter(OUTLIER_KMODES, Y==5)
# Y=5 dan K1=1
DATA_beda_MODES_Y5_M1=filter(DATA_beda_MODES_Y5,cluster_kmodes==1)
# Y=5 dan K1=2
DATA_beda_MODES_Y5_M2=filter(DATA_beda_MODES_Y5,cluster_kmodes==2)
# Y=5 dan K1=3
DATA_beda_MODES_Y5_M3=filter(DATA_beda_MODES_Y5,cluster_kmodes==3)
# Y=5 dan K1=4
DATA_beda_MODES_Y5_M4=filter(DATA_beda_MODES_Y5,cluster_kmodes==4)

#Kmedoids
OUTLIER_KMEDOIDS
# Y = 1
DATA_beda_MEDOIDS_Y1=filter(OUTLIER_KMEDOIDS, Y==1)
# Y=1 dan K2=2
DATA_beda_MEDOIDS_Y1_M2=filter(DATA_beda_MEDOIDS_Y1,cluster_kmedoids==2)
# Y=1 dan K2=3
DATA_beda_MEDOIDS_Y1_M3=filter(DATA_beda_MEDOIDS_Y1,cluster_kmedoids==3)
# Y=1 dan K2=4
DATA_beda_MEDOIDS_Y1_M4=filter(DATA_beda_MEDOIDS_Y1,cluster_kmedoids==4)
# Y=1 dan K2=5
DATA_beda_MEDOIDS_Y1_M5=filter(DATA_beda_MEDOIDS_Y1,cluster_kmedoids==5)

# Y = 2
DATA_beda_MEDOIDS_Y2=filter(OUTLIER_KMEDOIDS, Y==2)
# Y=2 dan K2=1
DATA_beda_MEDOIDS_Y2_M1=filter(DATA_beda_MEDOIDS_Y2,cluster_kmedoids==1)
# Y=2 dan K2=3
DATA_beda_MEDOIDS_Y2_M3=filter(DATA_beda_MEDOIDS_Y2,cluster_kmedoids==3)
# Y=2 dan K2=4
DATA_beda_MEDOIDS_Y2_M4=filter(DATA_beda_MEDOIDS_Y2,cluster_kmedoids==4)
# Y=2 dan K2=5
DATA_beda_MEDOIDS_Y2_M5=filter(DATA_beda_MEDOIDS_Y2,cluster_kmedoids==5)

# Y = 3
DATA_beda_MEDOIDS_Y3=filter(OUTLIER_KMEDOIDS, Y==3)
# Y=3 dan K2=1
DATA_beda_MEDOIDS_Y3_M1=filter(DATA_beda_MEDOIDS_Y3,cluster_kmedoids==1)
# Y=3 dan K2=2
DATA_beda_MEDOIDS_Y3_M2=filter(DATA_beda_MEDOIDS_Y3,cluster_kmedoids==2)
# Y=3 dan K2=4
DATA_beda_MEDOIDS_Y3_M4=filter(DATA_beda_MEDOIDS_Y3,cluster_kmedoids==4)
# Y=3 dan K2=5
DATA_beda_MEDOIDS_Y3_M5=filter(DATA_beda_MEDOIDS_Y3,cluster_kmedoids==5)

# Y = 4
DATA_beda_MEDOIDS_Y4=filter(OUTLIER_KMEDOIDS, Y==4)
# Y=4 dan K2=1
DATA_beda_MEDOIDS_Y4_M1=filter(DATA_beda_MEDOIDS_Y4,cluster_kmedoids==1)
# Y=4 dan K2=2
DATA_beda_MEDOIDS_Y4_M2=filter(DATA_beda_MEDOIDS_Y4,cluster_kmedoids==2)
# Y=4 dan K2=3
DATA_beda_MEDOIDS_Y4_M3=filter(DATA_beda_MEDOIDS_Y4,cluster_kmedoids==3)
# Y=4 dan K2=5
DATA_beda_MEDOIDS_Y4_M5=filter(DATA_beda_MEDOIDS_Y4,cluster_kmedoids==5)

# Y = 5
DATA_beda_MEDOIDS_Y5=filter(OUTLIER_KMEDOIDS, Y==5)
# Y=5 dan K2=1
DATA_beda_MEDOIDS_Y5_M1=filter(DATA_beda_MEDOIDS_Y5,cluster_kmedoids==1)
# Y=5 dan K2=2
DATA_beda_MEDOIDS_Y5_M2=filter(DATA_beda_MEDOIDS_Y5,cluster_kmedoids==2)
# Y=5 dan K2=3
DATA_beda_MEDOIDS_Y5_M3=filter(DATA_beda_MEDOIDS_Y5,cluster_kmedoids==3)
# Y=5 dan K2=4
DATA_beda_MEDOIDS_Y5_M4=filter(DATA_beda_MEDOIDS_Y5,cluster_kmedoids==4)




# Fungsi untuk menghitung jarak Hamming
hamming_dist_to_center <- function(observation, center) {
  dist <- sum(observation != center)
  return(dist)
}

# Hitung jarak Hamming dari setiap observasi ke pusat klaster
jarak_hamming <- function(data,pusat) {
  distances <- sapply(1:nrow(data), function(i) {
    obs <- data[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]
    obs <- obs[i, ]
    hamming_dist_to_center(obs, pusat)
  })
  return(distances)
}
#contoh A = 1 dan M = 2
result <- jarak_hamming(DATA_beda_MODES_Y1_M2,P_K2_M1)
DATA_beda_MODES_Y1_M2$jarak_pusat_modes <- result

result <- jarak_hamming(DATA_beda_MODES_Y1_M2,P_K1_A2)
DATA_beda_MODES_Y1_M2$jarak_pusat_awal <- result

DATA_beda_MODES_Y1_M2



# Fungsi untuk menghitung jarak Manhattan
manhattan_dist_to_center <- function(observation, center) {
  dist <- abs(observation - center)
  dist <- sum(dist)
  return(dist)
}

# Hitung jarak Manhattan dari setiap observasi ke pusat klaster
jarak_manhattan <- function(data,pusat) {
  distances <- sapply(1:nrow(data), function(i) {
    obs <- data[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]
    obs <- obs[i, ]
    
    manhattan_dist_to_center(obs, pusat)
  })
  return(distances)
}
#contoh A = 1 dan M = 2

result <- jarak_manhattan(DATA_beda_MEDOIDS_Y1_M2,P_K1_A2)
DATA_beda_MEDOIDS_Y1_M2$jarak_pusat_awal <- result

result <- jarak_manhattan(DATA_beda_MEDOIDS_Y1_M2,P_K2_M2)
DATA_beda_MEDOIDS_Y1_M2$jarak_pusat_medoids <- result



DATA_beda_MEDOIDS_Y1_M2

DATA_TA_HASIL_MODES=DATA_TA
DATA_TA_HASIL_MEDOIDS=DATA_TA

##
# Hitung jarak Hamming dari setiap observasi ke pusat klaster
jarak_hamming <- function(data,pusat1,pusat2,pusat3,pusat4,pusat5) {
  distances <- sapply(1:nrow(data), function(i) {
    obs <- data[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]
    obs <- obs[i, ]
    if (data[i, "Y"]==1){pusat=pusat1
    } else if(data[i, "Y"]==2){pusat=pusat2
    } else if(data[i, "Y"]==3){pusat=pusat3
    } else if(data[i, "Y"]==4){pusat=pusat4
    } else {pusat=pusat5}
    hamming_dist_to_center(obs, pusat)
  })
  return(distances)
}

result<-jarak_hamming(DATA_TA_HASIL_MODES,P_K1_A1,P_K2_A1,P_K3_A1,P_K4_A1,P_K5_A1)
DATA_TA_HASIL_MODES$jarak_pusat_awal <- result

result<-jarak_hamming(DATA_TA_HASIL_MODES,P_K1_M1,P_K2_M1,P_K3_M1,P_K4_M1,P_K5_M1)
DATA_TA_HASIL_MODES$jarak_pusat_modes <- result


##
# Hitung jarak Manhattan dari setiap observasi ke pusat klaster
jarak_manhattan <- function(data,pusat1,pusat2,pusat3,pusat4,pusat5) {
  distances <- sapply(1:nrow(data), function(i) {
    obs <- data[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")]
    obs <- obs[i, ]
    if (data[i, "Y"]==1){pusat=pusat1
    } else if(data[i, "Y"]==2){pusat=pusat2
    } else if(data[i, "Y"]==3){pusat=pusat3
    } else if(data[i, "Y"]==4){pusat=pusat4
    } else {pusat=pusat5}
    manhattan_dist_to_center(obs, pusat)
  })
  return(distances)
}

result<-jarak_manhattan(DATA_TA_HASIL_MEDOIDS,P_K1_A2,P_K2_A2,P_K3_A2,P_K4_A2,P_K5_A2)
DATA_TA_HASIL_MEDOIDS$jarak_pusat_awal <- result

result<-jarak_manhattan(DATA_TA_HASIL_MEDOIDS,P_K1_M2,P_K2_M2,P_K3_M2,P_K4_M2,P_K5_M2)
DATA_TA_HASIL_MEDOIDS$jarak_pusat_medoids <- result


### Mendapatkan klaster akhir dengan jarak terdekat
MODES_AKHIR <- ifelse(DATA_TA_HASIL_MODES[, "jarak_pusat_modes"] >= DATA_TA_HASIL_MODES[, "jarak_pusat_awal"], DATA_TA_HASIL_MODES[, "Y"], DATA_TA_HASIL_MODES[, "cluster_kmodes"])
MEDOIDS_AKHIR <-ifelse(DATA_TA_HASIL_MEDOIDS[, "jarak_pusat_medoids"] >= DATA_TA_HASIL_MEDOIDS[, "jarak_pusat_awal"], DATA_TA_HASIL_MEDOIDS[, "Y"], DATA_TA_HASIL_MEDOIDS[, "cluster_kmedoids"])
DATA_TA_HASIL_MODES$KLASTER_AKHIR <- MODES_AKHIR
DATA_TA_HASIL_MEDOIDS$KLASTER_AKHIR <- MEDOIDS_AKHIR

DATA_TA_HASIL_MODES
DATA_TA_HASIL_MEDOIDS



#eksplor ke excel
write_xlsx(DATA_TA_HASIL_MODES, path="DATA_TA_HASIL_MODES.xlsx")
write_xlsx(DATA_TA_HASIL_MEDOIDS, path="DATA_TA_HASIL_MEDOIDS.xlsx")
