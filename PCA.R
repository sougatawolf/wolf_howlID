#set directory
setwd ("D:/Wolf_Project/Howl_recognise_fresh20200821_1133/Analysis/R/PCA.133H")
library(readxl)

#readfile
howl133 <- read_excel("133H.xlsx")
str(howl133)
howl133_variable <- howl133 [,4:16]
howl133_variable
pca1 <- prcomp(howl133_variable, scale=TRUE)

#ploting PCA pca1$x[,1] for component 1 & pca1$x [,2] for comp 2 
plot(pca1$x[,1], pca1$x [,2])

#exporting PC values
PC1 <-pca1$x[,1]
PC2 <-pca1$x[,2]
PC3 <-pca1$x[,3]
PC4 <-pca1$x[,4]
PC5 <-pca1$x[,5]
PC6 <-pca1$x[,6]
PC7 <-pca1$x[,3]

PC_score <- data.frame(howl133$Filename, PC1, PC2, PC3, PC4, PC5, PC6, PC7)
write.csv(PC_score, "PCA.score.133H.csv")

pca1.var <-pca1$sdev^2
pca1.var.per <-round(pca1.var/sum(pca1.var)*100,1)
write.csv(pca1.var.per, "PCA_components_imp.csv")

barplot(pca1.var.per, main="Scree Plot", xlab="Principal Component", ylab = "percent variation", names.arg=TRUE)

#loading_scores for PC1
loading_scores <-pca1$rotation[,2]
variable_score <- abs(loading_scores)
variable_score_ranked <- sort(variable_score, decreasing = TRUE)
variable_score_ranked

#loading_scores for PC2
loading_scores_pc2 <-pca1$rotation[,2]
variable_score_pc2 <- abs(loading_scores_pc2)
variable_score_pc2_ranked <- sort(variable_score_pc2, decreasing = TRUE)
variable_score_pc2_ranked

#export PC-rotation value
colnames(howl133_variable)

PC_load<-data.frame(colnames(howl133_variable),pca1$rotation[,1],pca1$rotation[,2],pca1$rotation[,3],
                    pca1$rotation[,4],pca1$rotation[,5],pca1$rotation[,6],pca1$rotation[,7])
write.csv(PC_load, "variable_score_PCA.csv")
