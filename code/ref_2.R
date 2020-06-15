# ref:https://www.kaggle.com/mirichoi0218/classification-breast-cancer-or-not-with-15-ml 

library("argparse")

# Rscript yuyu.R --data data.csv --output result.csv
parser <- ArgumentParser()
parser$add_argument('--data', help='data')
parser$add_argument('--output', help='performance')
args = parser$parse_args()

wbcd <- read.csv(args$data, header = T, stringsAsFactors=F)
# wbcd <- read.csv("data.csv", header=T, stringsAsFactors=F)
wbcd$X <- NULL
wbcd <- wbcd[,-1]
wbcd$diagnosis <- factor(ifelse(wbcd$diagnosis=="B","Benign","Malignant"))
# str(wbcd)

# split data
nrows <- NROW(wbcd)
set.seed(218)                           ## fix random value
index <- sample(1:nrows, 0.7 * nrows)   ## shuffle and divide

#train <- wbcd                          ## 569 test data (100%)
train <- wbcd[index,]                   ## 398 test data (70%)
test <- wbcd[-index,]                   ## 171 test data (30%)

prop.table(table(train$diagnosis))
prop.table(table(test$diagnosis))

library(caret)

opt_predict = c()

# null_model
my_ans = test$diagnosis
for(i in 1:length(my_ans)){
  my_ans[[i]] = "Benign"
}

TP = 0
FP = 0
for(i in 1:length(my_ans)){
  if(my_ans[[i]] == test$diagnosis[[i]]){
    TP = TP + 1
  }
  else{
    FP = FP + 1    
  }
}
accuracy = TP/(TP+FP)
sprintf("accuracy= %s", accuracy)
opt_predict = c(opt_predict, 0.5)


library(rpart)
learn_rp <- rpart(diagnosis~.,data=train,control=rpart.control(minsplit=2))
pre_rp <- predict(learn_rp, test[,-1], type="class")
cm_rp  <- confusionMatrix(pre_rp, test$diagnosis)   
cm_rp

# loading the package
library(ROCR)
Prediction <- prediction(as.numeric(pre_rp),as.numeric(test$diagnosis))
performance <- performance(Prediction, "tpr","fpr")
# plotting ROC curve
# par(mar = rep(2, 4))
plot(performance, main="ROC curve of decision tree")

# calculate auc
auc.test <- performance(Prediction, measure = "auc")
auc.test <- auc.test@y.values
sprintf("AUC= %s", round(auc.test[[1]],3))
opt_predict = c(opt_predict, round(auc.test[[1]],3))


# naive bayes
library(e1071)

 # naiveBayes without laplace
learn_nb <- naiveBayes(train[,-1], train$diagnosis)
pre_nb <- predict(learn_nb, test[,-1])
cm_nb <- confusionMatrix(pre_nb, test$diagnosis)        
cm_nb

Prediction <- prediction(as.numeric(pre_nb),as.numeric(test$diagnosis))
performance <- performance(Prediction, "tpr","fpr")
# plotting ROC curve
# par(mar = rep(2, 4))
plot(performance, main="ROC curve of naive bayes")

# calculate auc
auc.test <- performance(Prediction, measure = "auc")
auc.test <- auc.test@y.values
sprintf("AUC= %s", round(auc.test[[1]],3))
opt_predict = c(opt_predict, round(auc.test[[1]],3))

library(randomForest)
learn_rf <- randomForest(diagnosis~., data=train, ntree=500, proximity=T, importance=T)
pre_rf   <- predict(learn_rf, test[,-1])
cm_rf    <- confusionMatrix(pre_rf, test$diagnosis)
cm_rf 

Prediction <- prediction(as.numeric(pre_rf),as.numeric(test$diagnosis))
performance <- performance(Prediction, "tpr","fpr")
# plotting ROC curve
# par(mar = rep(2, 4))
plot(performance, main="ROC curve of random forest")

# calculate auc
auc.test <- performance(Prediction, measure = "auc")
auc.test <- auc.test@y.values
sprintf("AUC= %s", round(auc.test[[1]],3))
opt_predict = c(opt_predict, round(auc.test[[1]],3))

# knn
# Choose ‘k’ which shows best predict performance in KNN
library(class)

acc_test <- numeric() 

for(i in 1:30){
  predict <- knn(train=train[,-1], test=test[,-1], cl=train[,1], k=i, prob=T)
  acc_test <- c(acc_test,mean(predict==test[,1]))
}

acc <- data.frame(k= seq(1,30), cnt = acc_test)

opt_k <- subset(acc, cnt==max(cnt))[1,]
sub <- paste("Optimal number of k is", opt_k$k, "(accuracy :", opt_k$cnt,") in KNN")

library(highcharter)
hchart(acc, 'line', hcaes(k, cnt)) %>%
  hc_title(text = "Accuracy With Varying K (KNN)") %>%
  hc_subtitle(text = sub) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_xAxis(title = list(text = "Number of Neighbors(k)")) %>%
  hc_yAxis(title = list(text = "Accuracy"))

# Apply optimal K to show best predict performance in KNN
pre_knn <- knn(train = train[,-1], test = test[,-1], cl = train[,1], k=opt_k$k, prob=T)
cm_knn  <- confusionMatrix(pre_knn, test$diagnosis)
cm_knn

Prediction <- prediction(as.numeric(pre_knn),as.numeric(test$diagnosis))
performance <- performance(Prediction, "tpr","fpr")
# plotting ROC curve
# par(mar = rep(2, 4))
plot(performance, main="ROC curve of KNN")

# calculate auc
auc.test <- performance(Prediction, measure = "auc")
auc.test <- auc.test@y.values
sprintf("AUC= %s", round(auc.test[[1]],3))
opt_predict = c(opt_predict, round(auc.test[[1]],3))

# svm
learn_svm <- svm(diagnosis~., data=train)
pre_svm <- predict(learn_svm, test[,-1])
cm_svm <- confusionMatrix(pre_svm, test$diagnosis)
cm_svm


Prediction <- prediction(as.numeric(pre_svm),as.numeric(test$diagnosis))
performance <- performance(Prediction, "tpr","fpr")
# plotting ROC curve
# par(mar = rep(2, 4))
plot(performance, main="ROC curve of SVM")

# calculate auc
auc.test <- performance(Prediction, measure = "auc")
auc.test <- auc.test@y.values
sprintf("AUC= %s", round(auc.test[[1]],3))
opt_predict = c(opt_predict, round(auc.test[[1]],3))

# gbm
library(gbm)
test_gbm <- gbm(diagnosis~., data=train, distribution="gaussian",n.trees = 10000,
                shrinkage = 0.01, interaction.depth = 4, bag.fraction=0.5, train.fraction=0.5,n.minobsinnode=10,cv.folds=3,keep.data=TRUE,verbose=FALSE,n.cores=1)
best.iter <- gbm.perf(test_gbm, method="cv",plot.it=FALSE)
fitControl = trainControl(method="cv", number=5, returnResamp="all")
learn_gbm = train(diagnosis~., data=train, method="gbm", distribution="bernoulli", trControl=fitControl, verbose=F, tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))
pre_gbm <- predict(learn_gbm, test[,-1])
cm_gbm <- confusionMatrix(pre_gbm, test$diagnosis)
cm_gbm

Prediction <- prediction(as.numeric(pre_gbm),as.numeric(test$diagnosis))
performance <- performance(Prediction, "tpr","fpr")
# plotting ROC curve
# par(mar = rep(2, 4))
plot(performance, main="ROC curve of gradient boosting")

# calculate auc
auc.test <- performance(Prediction, measure = "auc")
auc.test <- auc.test@y.values
sprintf("AUC= %s", round(auc.test[[1]],3))
opt_predict = c(opt_predict, round(auc.test[[1]],3))

# opt_predict2 <- c(accuracy, cm_rp$overall[1], cm_nb$overall[1], cm_knn$overall[1], cm_rf$overall[1], cm_gbm$overall[1], cm_svm$overall[1])
names(opt_predict) <- c("null", "rpart","nb","knn","rf","gbm","svm")
best_predict_model <- subset(opt_predict, opt_predict==max(opt_predict))
sprintf("best_model_is: %s", best_predict_model)

output = args$output

df <- data.frame(Model_name = names(opt_predict),
                 AUC_score = opt_predict)

write.csv(df, output, row.names = FALSE, quote=FALSE)
