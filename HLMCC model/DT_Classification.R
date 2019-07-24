library(caret) 
library(rpart) 
library(e1071)
library(DMwR)
library(PRROC)

rm(list=ls())

setwd("../Desktop/HLMCC/Datasets/clustered_Dataset")
clusteredData = read.csv("satellite-unsupervised-lablled.csv", header=T)

table(clusteredData$ClusterLabel) ## "o" = "anomaly" class And "n" = "normal"

#remove the id
clusteredData <- clusteredData[,-1]

#convert the label to factor
clusteredData$ClusterLabel <- factor(clusteredData$ClusterLabel)

#--- SMOTE the data -----
Over = ( (0.1 * 5069) - 31 ) / 31 
Under = (0.9 * 5069) / (31 * Over) 
Over_Perc = round(Over, 1) * 100
Under_Perc = round(Under, 1) * 100

set.seed(10)
smoteData <- SMOTE(ClusterLabel ~., clusteredData, perc.over=Over_Perc, perc.under=Under_Perc)
table(smoteData$ClusterLabel)

#---- evaluation metrics + seeds ----#
fpr <- rep(NA, len=5)
fm <- rep(NA, len=5)
pri <- rep(NA, len=5)
pr_c <- rep(NA, len=5)
Sensitivity <- rep(NA, len=5)
seed <- c(10,100,1000,2000,3500)

#--- train the model 5 times ----#
for (i in 1:5) {
  set.seed(seed[i])
  n <- nrow(smoteData)
  # Shuffle the dataset, call the result shuffled
  shuffled <- smoteData[sample(n),]
  validation_index <- createDataPartition(shuffled$ClusterLabel, p = 0.7, list = FALSE)
  train <- shuffled[validation_index ,]
  test<- shuffled[-validation_index ,]

  # train the DT.
  fit <- rpart(ClusterLabel ~ . ,train,  method="class")
  print(fit)
  # Predict fot hte test data.
  predicted<- predict(fit,test[,1:36],type="class") # give me predict calss label
  predicted
  conf.mat<- table(Predicted_class = predicted,
                   Actual_class = test$ClusterLabel) [2:1,2:1]
  conf.mat
  results <- confusionMatrix(conf.mat)
  print(conf.mat)
  fpr[i] <- 1-specificity(conf.mat)
  fm[i] <- results$byClass['F1']
  pri[i]<- precision(conf.mat)
  Sensitivity[i] <- sensitivity(conf.mat)

  # compute the area under the precision-recall curve (AUCPR)
  ypred<- predict(fit,test[,1:36]) # give me prediction
  ypred
  m0<-ypred[,1]  #normal class
  m1<-ypred[,2]  # anomaly class
  
  # Create a dataframe of scores
  scores <- data.frame(m1,test$ClusterLabel)
  # Class 0 is data points of +ve class (in this case, digit o) and -ve class (digit n)
  #Compute Precision Recall
  pr_auc <- pr.curve(scores.class0=scores[scores$test.ClusterLabel=="o",]$m1,
                 scores.class1=scores[scores$test.ClusterLabel=="n",]$m1,
                 curve=T)
  pr_c[i] <-pr_auc$auc.integral
}

#--------The mean of results --------

cat("False Positive Rate : Mean",mean(fpr)) 
cat("False Positive Rate : SD",sqrt((5-1)/5) * sd(fpr)) 

cat("F-measure :",mean (fm)) 
cat("F-measure  : SD",sqrt((5-1)/5) * sd(fm)) 

cat("Sensitivity :",mean (Sensitivity)) 
cat("Sensitivity : SD",sqrt((5-1)/5) * sd(Sensitivity)) 


cat("Precision :",mean(pri)) 
cat("Precision  : SD",sqrt((5-1)/5) * sd(pri)) 

cat("The area under the precision-recall curve (AUCPR) :",mean(pr_c)) 
cat("The area under the precision-recall curve (AUCPR) : SD",sqrt((5-1)/5) * sd(pr_c))
