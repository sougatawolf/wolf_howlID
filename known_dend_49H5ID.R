setwd ("D:/Wolf_Project/Howl_recognise_fresh20200821_1133/Analysis/R/Dendogram_known")

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
wolf49H <- read_excel("49H5ID_LDScroe.xlsx")
str(wolf49H)
wolf49Hx <- wolf49H [,10:13]
wolf49Hx
wolf49H.rn <- data.frame (wolf49H$howl_3ID.Filename, wolf49Hx$x.LD1, wolf49Hx$x.LD2, row.names = TRUE )
wolf49H.rn
str(wolf49H.rn)
agn2 <- agnes(wolf49H.rn, metric = "manhattan", stand = TRUE)
dend <- as.dendrogram(agn2)
 
dend <- as.dendrogram(agn2) 

# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:0)

# Color the branches based on the hight:
dend <- color_branches(dend, h= 2.2) #, groupLabels=different Howling Individual)

# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", .5)
# And plot:
par(mar=c(1,1,1,1),  mgp=c(1, 1, 1))
circlize_dendrogram(dend, lebels=TRUE, row.names= TRUE, labels_track_height = 0.4)
dend
#writetable
clustnumber<- cutree(dend, k=5)
dendogram_50known_howl <-data.frame(wolf49H$howl_3ID.Individual, clustnumber)
write.csv(dendogram_50known_howl, "49h5ID_dend_results.csv")

#########################
agn3 <- agnes(wolf49H.rn, metric = "manhattan", stand = TRUE)

dend2 <- as.dendrogram(agn3)
# order it the closest we can to the order of the observations:
dend2 <- rotate(dend2, 1:0)

# Color the branches based on the hight:
dend2 <- color_branches(dend2, h= 2.2) #, groupLabels=different Howling Individual)

# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend2 <- set(dend2, "labels_cex", .5)
#colour highlighted row
highlight<- row.names(wolf49H.rn)[20]
highlight
dend2 <- color_labels(dend2, labels = highlight , col = 2)
# And plot:
par(mar=c(1,1,1,1),  mgp=c(1, 1, 1))

circlize_dendrogram(dend2, lebels=TRUE, row.names= TRUE, labels_track_height = 0.4)
