library(mice)
library(ggplot2)
library(caret)
library(ROCR)
library(class)
library(C50)
library(rpart)
library(randomForest)
library(shiny)
library(tree)
library(kernlab)

rm(list = ls())
#download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", "processed.cleveland.data")
#download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.hungarian.data","processed.hungarian.data")
#download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.switzerland.data","processed.switzerland.data")
#download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.va.data","processed.va.data")

#Data Preprocessing
processed.cleveland <- read.csv("processed.cleveland.data",header=FALSE)
processed.hungarian <- read.csv("processed.hungarian.data", header=FALSE)
processed.switzerland <- read.csv("processed.switzerland.data", header=FALSE)
processed.va <- read.csv("processed.va.data", header=FALSE)
#Combining all into one
data<-rbind(processed.cleveland,processed.hungarian,processed.switzerland,processed.va)

idx <- data == "?"
is.na(data) <- idx

colnames(data)<-c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")

#Converting factors/character into appropriate datatype 
data$age <- as.numeric(data$age)
data$sex<-as.factor(data$sex)
data$cp<-as.factor(data$cp)
data$trestbps<-as.numeric(data$trestbps)
data$chol<-as.numeric(data$chol)
data$fbs<-as.factor(data$fbs)
data$restecg<-as.factor(data$restecg)
data$thalach<-as.numeric(data$thalach)
data$exang<-as.factor(data$exang)
data$oldpeak<-as.numeric(data$oldpeak)
data$slope<-as.factor(data$slope)
data$ca <- as.factor(data$ca)
data$thal <- as.factor(data$thal)
data$num<-as.factor(data$num)

#To convert each column of data to numeric 
data_set <- as.data.frame(apply(data, 2, as.numeric))

#num gives the presence of heart disease, Present (>1), Not Present(0)
data_set$num[data_set$num > 0] <- 1
data_set$num <- factor(data_set$num, levels = c(0,1), labels = c("negative","positive"))
clean_data <- data_set
clean_data <- mice(clean_data)
Final_data <- complete(clean_data)


#Divide into training and testing
set.seed(3033)
inTrain<-createDataPartition(y=Final_data$num,p=0.7,list=F)
training<-Final_data[inTrain,]
testing<-Final_data[-inTrain,]
dim(training);dim(testing)
summary(training)

featurePlot(x=training[,c("age","cp","trestbps","chol","thalach","oldpeak")],
            y=training$num,plot = "pairs",scales=list(x=list(relation="free"), y=list(relation="free")), auto.key=list(columns=2))
featurePlot(x=training[,c("age","cp","trestbps","chol","thalach","oldpeak")],
            y=training$num,plot = "density",scales=list(x=list(relation="free"), y=list(relation="free")), auto.key=list(columns=2))

mahal=mahalanobis(training[,-c(14)],colMeans(training[,-c(14)]),cov(training[,-c(14)],use = "pairwise.complete.obs"))
cutoff=qchisq(.999,ncol(training[,-c(14)]))
training=training[mahal<cutoff,]
dim(training)

#training model using ML algorithms
model_rf<-train(num~.,data=training,method="rf",trControl=trainControl(method="cv"),number=10)
model_glm<-train(num~.,data=training,method="glm",preProcess="pca")
model_gbm<-train(num~.,data=training,method="gbm")
model_knn <- train(num ~., method = "knn", data = training,tuneLength = 10,  tuneGrid=data.frame(k=1:5),
                   trControl = trainControl(
                     method = "cv"))

# accuracy improved after increase the k and preProcess "pca" (signal extraction (11), centered (11), scaled (11) )
model_knn <- train(num ~., method = "knn", preProcess="pca",data = training,tuneLength = 10,  tuneGrid=data.frame(k=5:25),
                   trControl = trainControl(
                     method = "cv"))

# sigDist <- sigest(num ~., data = training, frac = 1)
# svmTuneGrid <- data.frame(.sigma = sigDist[1], .C = 2^(5:25))
# model_svm <- train(num ~.,
#                 data = training,
#                 method = "svmRadial",
#                 preProc = c("center", "scale"),
#                 tuneGrid = svmTuneGrid,
#                 trControl = trainControl(method = "repeatedcv", repeats = 5, 
#                                          classProbs =  TRUE))



pred_rf_p<-predict(model_rf,newdata=testing,method="class",type="prob")
pred_rf_r<-predict(model_rf,newdata = testing,method="class",type="raw")
pred_gbm_p<-predict(model_gbm,newdata=testing,method="class",type="prob")
pred_gbm_r<-predict(model_gbm,newdata = testing,method="class",type="raw")
pred_glm_p<-predict(model_glm,newdata=testing,type="prob")
pred_glm_r<-predict(model_glm,newdata=testing,type="raw")
pred_knn_p<-predict(model_knn,newdata=testing,type="prob")
pred_knn_r<-predict(model_knn,newdata=testing,type="raw")
# pred_svm_p<-predict(model_svm,newdata=testing,method="class",type="prob")
# pred_svm_r<-predict(model_svm,newdata = testing,method="class",type="raw")

pred_roc_rf<-prediction(pred_rf_p$positive,testing$num)
perf_roc_rf<-performance(pred_roc_rf,"tpr", "fpr")
plot(perf_roc_rf,col=1,main="ROC curves comparing classification performance of 4 machine learning models")
auc_rf<- performance(pred_roc_rf,"auc")
auc_rf

pred_roc_glm<-prediction(pred_glm_p$positive,testing$num)
perf_roc_glm<-performance(pred_roc_glm,"tpr", "fpr")
plot(perf_roc_glm,add=T,col=2)
auc_glm<- performance(pred_roc_glm,"auc")
auc_glm

pred_roc_knn<-prediction(pred_knn_p$positive,testing$num)
perf_roc_knn<-performance(pred_roc_knn,"tpr", "fpr")
plot(perf_roc_knn,col=3,add=T)
auc_knn<- performance(pred_roc_knn,"auc")
auc_knn

pred_roc_gbm<-prediction(pred_gbm_p$positive,testing$num)
perf_roc_gbm<-performance(pred_roc_gbm,"tpr", "fpr")
plot(perf_roc_gbm,col=4,add=T)
auc_gbm<- performance(pred_roc_gbm,"auc")
auc_gbm

# pred_roc_svm<-prediction(pred_svm_p$positive,testing$num)
# perf_roc_svm<-performance(pred_roc_svm,"tpr", "fpr")
# plot(perf_roc_svm,col=5,add=T)
# auc_svm<- performance(pred_roc_svm,"auc")
# auc_svm


legend(0.1, 0.3, c('rf','glm','knn','gbm'), 1:4 ,seg.len = 0.01,text.width=0.05,horiz = T)

Evaluation_output<- function(x,y){
  TP  <- sum(x == "positive" & y== "positive")
  FP   <- sum(x == "negative" & y== "positive")
  TN  <- sum(x == "negative" & y == "negative")
  FN   <- sum(x == "positive" & y == "negative")
  Accuracy<-(TP+TN)/(TP+FP+TN+FN)
  Precision<-TP/(TP+FP)
  Recall_Sensitivity<-TP/(TP+FN)
  #F1<-2*Precision*Recall_Sensitivity/(Precision+Recall_Sensitivity)
  row.names <- c("Prediction_T", "Prediction_F" )
  col.names <- c("Test_T", "Test_F")
  Outcome_Matrix<-cbind(Outcome = row.names, as.data.frame(matrix( 
    c(TP, FN, FP, TP) ,
    nrow = 2, ncol = 2,dimnames = list(row.names, col.names))))
  cat("Accuracy:",Accuracy)
  cat("\nPrecision:",Precision)
  cat("\nRecall: ",Recall_Sensitivity,"\n")
  # cat("F:",F1)
  print(Outcome_Matrix)
}
Evaluation_output(pred_rf_r,testing$num)
Evaluation_output(pred_glm_r,testing$num)
Evaluation_output(pred_knn_r,testing$num)
Evaluation_output(pred_gbm_r,testing$num)
#Evaluation_output(pred_svm_r,testing$num)
