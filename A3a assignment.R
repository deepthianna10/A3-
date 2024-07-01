
read.csv("C:\\Users\\HP\\OneDrive\\Desktop\\deepthi  asignments scma\\Airline_customer_satisfaction.csv\\Airline_customer_satisfaction.csv")
file_path <-("C:\\Users\\HP\\OneDrive\\Desktop\\deepthi  asignments scma\\Airline_customer_satisfaction.csv\\Airline_customer_satisfaction.csv")
df <- read.csv(file_path)
library(dplyr)
  summary(df)
  plot_missing(df)
  sum(is.na(df))
  head(df)
  tail(df)
  names(df)
  str(df)
  
  # Replace missing values with the mean for numeric columns
  df <- df %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  #Checking missing values after filling it with mean values of the column 
  missing_info <- colSums(is.na(df))
  cat("Missing Values Information:\n")
  print(missing_info)
  cor_matrix<-cor(df)
  print(cor_matrix)
  heatmap(cor_matrix)
  boxplot(flight disrance+seat comfort+departure+foodanddrink+onlinesupport+inflightentertainment+baggagehandling+,data="C:\\Users\\HP\\OneDrive\\Desktop\\Airline_customer_satisfaction.csv")
  install.packages("caTools")
  install.packages("MLmetrics")
  
  library(dplyr)
  
  library(pROC)
  library(rpart)
  library(rpart.plot)
  library(MLmetrics)
  
  # Logistic Regression
  set.seed(123)
  split<-sample.split(Outcome,SplitRatio = 0.7)
  train<-subset(df,split==TRUE)
  test<-subset(df,split==FALSE)
  model<-glm(Outcome~.,data=train,family=binomial)
  predicted_probs<-predict(model,newdata=test,type="response")
  predicted_class<-ifelse(predicted_probs>=0.5,1,0)
  
  # Confusion Matrix 
  CM<-ConfusionMatrix(factor(predicted_class),factor(test$Outcome))
  print(CM)
  roc_obj<-roc(test$Outcome,predicted_probs)
  auc<-auc(roc_obj)
  print(paste("AUC-ROC:",auc))
  plot(roc_obj,main="ROC Curve",print.auc=TRUE)
  
  #decision tree analysis for the data in part A and compare the results of the 
  #Logistic regression and Decision tree
  library(stats)
  install.packages("rpart")
  library(rpart)
  install.packages("caTools")
  library(caTools)
  
  
  
  set.seed(123)
  split<-sample.split(dia$Outcome,SplitRatio = 0.7)
  train<-subset(df,split == TRUE)
  test<-subset(df,split == FALSE)
  model<-rpart(Outcome~.,data=train,method="class")
  predicted_probs<-predict(model,newdata=test,type="prob")
  predicted_class<-ifelse(predicted_probs[,2]>=0.5,1,0)
  
  ConfM<-ConfusionMatrix(factor(predicted_class),factor(test$Outcome))
  print(ConfM)-
    roc_obj<-roc(test$Outcome,predicted_probs[,2])
  auc<-auc(roc_obj)
  print(paste("AUC-ROC:",auc))
  plot(roc_obj,main="ROC Curve",print.auc=TRUE)
  