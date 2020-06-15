#rscript compare_svm_nullmodel.R
library(ROCR)
library(e1071)
library(gbm)
library(caret)
library(class)


wbcd <- read.csv("data.csv", header=T, stringsAsFactors=F)
wbcd$X <- NULL
wbcd <- wbcd[,-1]
wbcd$diagnosis <- factor(ifelse(wbcd$diagnosis=="B","Benign","Malignant"))
#View(wbcd)



# split data k fold 
set.seed(1111)
folds<-createFolds(y=wbcd$diagnosis,k=5)


# best validation score of SVM i=4, all features
i<-4
train<-wbcd[-c(folds[[i]],folds[[(i%%5)+1]]),]
test<-wbcd[folds[[(i%%5)+1]],]

# null_model
pre_null <- test$diagnosis
for(i in 1:length(pre_null)){
  pre_null[[i]] = "Benign"
}
eval_null <- prediction(as.numeric(pre_null),as.numeric(test$diagnosis))
# null_model's confusionMatrix
cm_null<-confusionMatrix(pre_null, test$diagnosis)
print(cm_null)
# null model’s ROC
plot(performance(eval_null,"tpr","fpr"))
#  null model’s AUC
AUC_null<-attributes(performance(eval_null,'auc'))$y.values[[1]]
print(paste("AUC:",AUC_null))


# SVM 
learn_svm <- svm(diagnosis~., data=train)
pre_svm <- predict(learn_svm, test[,-1])
eval_svm <- prediction(as.numeric(pre_svm),as.numeric(test$diagnosis))
# SVM's confusionMatrix
cm_svm<-confusionMatrix(pre_svm, test$diagnosis)
print(cm_svm)
# SVM's ROC
plot(performance(eval_svm, "tpr","fpr"), main="ROC curve")
#  SVM’s AUC
AUC_svm<-attributes(performance(eval_svm,'auc'))$y.values[[1]]
print(paste("AUC:",AUC_svm))

