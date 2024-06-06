# DESICION TREE
#membagi data training dan testing
set.seed(101)
splitdata <- sample.split(DATA_TA_HASIL_MEDOIDS$KLASTER_AKHIR, SplitRatio = 0.75) # training 70%
data.training <- subset(DATA_TA_HASIL_MEDOIDS, splitdata==T)
data.testing <- subset(DATA_TA_HASIL_MEDOIDS, splitdata==F)

#melihat dimensi data
dim(data.training)
dim(data.testing)
data.training
#membuat model desicion tree
#variabel Y
DT_AKHIR_MODES=rpart(KLASTER_AKHIR~X1+X15+X10+X13+X4+X14+X19+X11+X16+X5+X26, method = "class", data = data.training)


DT_AKHIR_MODES
#visualisasi
prp(DT_AKHIR_MODES)

#Prdiksi
prediksi_KMODES_KLASTER_AKHIR <- predict(DT_AKHIR_MODES, newdata = data.testing, type = "class")
summary(prediksi_KMODES_KLASTER_AKHIR)
#confusion matrix
table(prediksi_KMODES_KLASTER_AKHIR,data.testing$KLASTER_AKHIR)


akurasi_KLASTER_AKHIR=confusionMatrix(data=prediksi_KMODES_KLASTER_AKHIR, reference = data.testing$KLASTER_AKHIR)$overall[1]#akurasi
presisi_KLASTER_AKHIR=confusionMatrix(data=prediksi_KMODES_KLASTER_AKHIR, reference = data.testing$KLASTER_AKHIR)$byClass[1]#presisi
recal_KLASTER_AKHIR=confusionMatrix(data=prediksi_KMODES_KLASTER_AKHIR, reference = data.testing$KLASTER_AKHIR)$byClass[2]#recal
F1_KLASTER_AKHIR = 2 * (presisi_KLASTER_AKHIR * recal_KLASTER_AKHIR) / (presisi_KLASTER_AKHIR+ recal_KLASTER_AKHIR) #F1
akurasi_KLASTER_AKHIR
presisi_KLASTER_AKHIR
recal_KLASTER_AKHIR
F1_KLASTER_AKHIR
