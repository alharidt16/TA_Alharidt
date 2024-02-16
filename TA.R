#Load library
library(readxl)
library(factoextra)
library(cluster)
library(klaR)
library(writexl)
library(dplyr)
library(caret)


#Load Data
DATA_TA <- read.csv("Dataclean.csv")
DATA_TA

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
write_xlsx(DATA_TA, path="MYDATA.xlsx")


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

#pusat klaster kmodes
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
OUTLIER_KMODES

#Data Kmedoids yang klasternya berbeda
DATA_TA_KMEDOIDS=DATA_TA[,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26","Y","cluster_kmedoids")]
OUTLIER_KMEDOIDS=DATA_TA_KMEDOIDS[DATA_TA_KMEDOIDS$Y != DATA_TA_KMEDOIDS$cluster_kmedoids,]
OUTLIER_KMEDOIDS

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
manhattan_dist(P_K2_M1,OUTLIER_KMODES[1,c("X1","X15","X10","X13","X4","X14","X19","X11","X16","X5","X26")] )
DATA_TA


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
# Menampilkan hasil confusion matrix

CM_kmodes
CM_kmedoids

# Memuat paket yang dibutuhkan, misalnya 'party' atau 'rpart'
library (party)

# Membuat model decision tree dengan fungsi ctree
# formula adalah rumus yang mendeskripsikan variabel prediktor dan respon
# data adalah nama kumpulan data yang digunakan
model_DT_kmodes <- ctree (cluster_kmodes~X1+X15+X10+X13+X4+X14+X19+X11+X16+X5+X26, DATA_TA)
model_DT_kmedoids <- ctree (cluster_kmedoids~X1+X15+X10+X13+X4+X14+X19+X11+X16+X5+X26, DATA_TA)




# Memuat paket yang dibutuhkan, misalnya 'RWeka' atau 'partykit'
library (RWeka)

# Membuat model decision tree dengan fungsi J48
# formula adalah rumus yang mendeskripsikan variabel prediktor dan respon
# data adalah nama kumpulan data yang digunakan
# control adalah parameter tambahan untuk mengatur algoritma C4.5
model <- J48 (cluster_kmedoids~X1+X15+X10+X13+X4+X14+X19+X11+X16+X5+X26, DATA_TA, control = Weka_control ())
# Melakukan visualisasi decision tree dengan fungsi plot
model


