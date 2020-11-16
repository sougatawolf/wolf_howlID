setwd ("D:/Wolf_Project/Howl_recognise_fresh20200821_1133/Analysis/R/Dendogram_test")

#Required Packages
#install.packages("dendextend")
#install.packages("circlize")

# load package cluster
library(cluster, quietly = TRUE)
library(readxl)
library(dendextend)
library(colorspace) # get nice colors
library(circlize)

#Reading excel file
wolfH_test <- read_excel("howlpredict_test.xlsx")
str(wolfH_test)

wolfH_test.rn <- data.frame (wolfH_test$Combo, wolfH_test$x.LD1, wolfH_test$x.LD2, row.names = TRUE )
wolfH_test.rn
str(wolfH_test.rn)
agn2 <- agnes(wolfH_test.rn, metric = "manhattan", stand = TRUE)

dend <- as.dendrogram(agn2)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:0)

# Color the branches based on the hight:
dend <- color_branches(dend, h=2.2) #, groupLabels=different Howling Individual)


# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend2 <- set(dend, "labels_cex", .4)
# And plot:
par(mar=c(1,1,1,1),  mgp=c(1, 1, 1))
circlize_dendrogram(dend2, lebels=TRUE, row.names= TRUE, labels_track_height = 0.4)

#writetable
clustnumber<- cutree(dend, h=2.2)
dendogram_test_howl <-data.frame(wolfH_test$Individaul.Name, clustnumber)
write.csv(dendogram_test_howl, "testhowl_dend_results2.csv")


#####################################################

#highlighting the wrong rows (optional)
highlight<- row.names(wolfH_test.rn)[13:14]
highlight2 <- row.names(wolfH_test.rn)[16:17]
highlight3 <- row.names(wolfH_test.rn)[20]

dend <- color_labels(dend, labels = highlight , col = 2)
dend <- color_labels(dend, labels = highlight2 , col = 2)
dend <- color_labels(dend, labels = highlight3 , col = 2)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend2 <- set(dend, "labels_cex", .4)
# And plot:
par(mar=c(1,1,1,1),  mgp=c(1, 1, 1))
circlize_dendrogram(dend2, lebels=TRUE, row.names= TRUE, labels_track_height = 0.4)
