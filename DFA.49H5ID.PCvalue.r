#install.packages("psych")

# load package
library(readxl)
library(psych)

#working directory from Options | General
setwd ("D:/Wolf_Project/Howl_recognise_fresh20200821_1133/Analysis/R/DFA_50H")

#Reading excel file
howl_3ID <- read_excel("50H5ID.xlsx")
#howl_3ID

#Assign Factors
howl_3ID$Individual=as.factor(howl_3ID$Individual)
#levels(howl_3ID$Individual)
#str(howl_3ID)

#LDA
library(MASS)
LDAhowl <- lda(formula = Individual ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7,
               data=howl_3ID)
LDAhowl
#histogram 
p <- predict(LDAhowl, howl_3ID)


px<-data.frame(howl_3ID$Filename, howl_3ID$Individual, p)
write.csv(px, "howlpredict_train.result.csv")
write
par(mar=c(1,1,1,1))
ldahist (data =p$x[,1:1], g = howl_3ID$Individual)
ldahist (data =p$x[,2:2], g = howl_3ID$Individual)

#Bi-plot
#install.packages("devtools")
#install.packages("ps", dependencies = T)
#install_github("fawda123/ggord")

library(devtools)
library(ggord)
ggord(LDAhowl, howl_3ID$Individual,
      xlim = c(-12, 5),
      ylim = c (-4.5, 4))

# Confusion matrix and accuracy - training data
p1 <- predict(LDAhowl, howl_3ID)$class
tab <- table(Predicted = p1, Actual = howl_3ID$Individual)
tab
sum(diag(tab))/sum(tab)

# Confusion matrix and accuracy - testing data
howl_test <- read_excel("Testing.xlsx")

#calculate value LD value for all howl
p3 <- predict(LDAhowl, howl_test)
p3x<-data.frame(howl_test$Filename, howl_test$Individual, p3)
write.csv(p3x, "howlpredict_test_LDscore .results.csv")
