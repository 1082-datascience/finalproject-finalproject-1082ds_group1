#rscript six_models.R --data data.csv --output result7.csv

library("argparse")
library("corrplot")
library(rpart)
library(ROCR)
library(e1071)
library(randomForest)
library('Formula')
library(gbm)
library(caret)
library(class)

parser <- ArgumentParser()
parser$add_argument('--data', help='data')
parser$add_argument('--output', help='performance')
args = parser$parse_args()

wbcd <- read.csv(args$data, header = T, stringsAsFactors=F)
#wbcd <- read.csv("data.csv", header=T, stringsAsFactors=F)

# remove null column and Id column
wbcd$X <- NULL
wbcd <- wbcd[,-1]
wbcd$diagnosis <- factor(ifelse(wbcd$diagnosis=="B","Benign","Malignant"))

# split data k fold 
set.seed(1111)
folds<-createFolds(y=wbcd$diagnosis,k=5)


# reduce features get columnsSelectedByCorr
getfmla<-function(train){
  # compare and remove one of two features that have a correlation higher than 0.75
  cor1<-cor(train[,-1])  # get correlation of train data
  eliminatecolumns <- c()
  for(i in c(1:29)){
    for(j in c((i+1):30)){
      if(abs(cor1[i,j])>=0.75){
        if (!(j%in%eliminatecolumns)){
          eliminatecolumns<-c(eliminatecolumns,j)
        }
      }
    }
  }
  
  columnsSelectedByCorr<- colnames(cor1)[-eliminatecolumns]
  return(columnsSelectedByCorr)
}

# get AUC function
getauc<-function(Prediction){
  auc.test <- performance(Prediction, measure = "auc")
  auc.test <- auc.test@y.values
  a<-round(auc.test[[1]],3)
  return(a)
}


# decision tree
get_DT<-function(fmla, train, test){
  learn_rp <- rpart(fmla,data=train,control=rpart.control(minsplit=2))# selected features
  pre_rp <- predict(learn_rp, test[,-1], type="class")
  Prediction <- prediction(as.numeric(pre_rp),as.numeric(test$diagnosis))
  a<-getauc(Prediction)
  return(a)
}

# naive bayes
get_NB<-function(columns, train, test){
  learn_nb <- naiveBayes(train[,columns], train$diagnosis) # selected features by columns
  pre_nb <- predict(learn_nb, test[,-1])
  Prediction <- prediction(as.numeric(pre_nb),as.numeric(test$diagnosis))
  a<-getauc(Prediction)
  return(a)
}


# radom forest
get_RF<-function(fmla, train, test){
  set.seed(1111)
  learn_rf <- randomForest(as.formula(fmla), data=train, ntree=500, proximity=T, importance=T) # selected features
  pre_rf <- predict(learn_rf, test[,-1])
  Prediction <- prediction(as.numeric(pre_rf),as.numeric(test$diagnosis))
  a<-getauc(Prediction)
  return(a)
}

# knn
get_knn<-function(columns, train, test){
  acc_test <- numeric() 
  for(i in 1:30){
    predict <- knn(train=train[,columns], test=test[,columns], cl=train[,1], k=i, prob=T)
    acc_test <- c(acc_test,mean(predict==test[,1]))
  }
  acc <- data.frame(k= seq(1,30), cnt = acc_test)
  opt_k <- subset(acc, cnt==max(cnt))[1,]
  pre_knn <- knn(train = train[,-1], test = test[,-1], cl = train[,1], k=opt_k$k, prob=T)
  Prediction <- prediction(as.numeric(pre_knn),as.numeric(test$diagnosis))
  acc_test <- c(acc_test,mean(predict==test[,1]))
  a<-getauc(Prediction)
  return(a)
}
# svm
get_svm<-function(fmla, train, test){
  learn_svm <- svm(as.formula(fmla), data=train)
  pre_svm <- predict(learn_svm, test[,-1])
  Prediction <- prediction(as.numeric(pre_svm),as.numeric(test$diagnosis))
  a<-getauc(Prediction)
  return(a)
}

# gbm
get_gbm<-function(fmla, train, test){
  test_gbm <- gbm(as.formula(fmla), data=train, distribution="gaussian",n.trees = 10000,
                  shrinkage = 0.01, interaction.depth = 4, bag.fraction=0.5, train.fraction=0.5,n.minobsinnode=10,cv.folds=3,keep.data=TRUE,verbose=FALSE,n.cores=1) 
  best.iter <- gbm.perf(test_gbm, method="cv",plot.it=FALSE)
  fitControl = trainControl(method="cv", number=5, returnResamp="all")
  learn_gbm = train(as.formula(fmla), data=train, method="gbm", distribution="bernoulli", trControl=fitControl, verbose=F, tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))
  pre_gbm <- predict(learn_gbm, test[,-1])
  Prediction <- prediction(as.numeric(pre_gbm),as.numeric(test$diagnosis))
  a<-getauc(Prediction)
  return(a)
}

# kfold validation for reduce features
ReduceFTable<-data.frame(models = c("rpart","nb","knn","rf","gbm","svm"),bestval=0,valauc=0,testauc=0)
for(i in 1:5){
  train<-wbcd[-c(folds[[i]],folds[[(i%%5)+1]]),]
  val<-wbcd[folds[[i]],]
  test<-wbcd[folds[[(i%%5)+1]],]
  # get columnsSelectedByCorr
  columns<-getfmla(train)
  # get formula of selected features
  y<-"diagnosis"
  x<-columns
  fmla <- paste(y,"~",paste(x,collapse=' + '),sep='')
  DT_AUC<-get_DT(fmla,train,val)
  NB_AUC<-get_NB(columns,train,val)
  KNN_AUC<-get_knn(columns,train,val)
  RF_AUC <-get_RF(fmla,train,val)
  GBM_AUC <-get_gbm(fmla,train,val)
  SVM_AUC <-get_svm(fmla,train,val)
  
  if(ReduceFTable[1,3]<DT_AUC){
    ReduceFTable[1,3]<-DT_AUC
    ReduceFTable[1,2]<-i
    ReduceFTable[1,4]<-get_DT(fmla,train,test)
  }
  if(ReduceFTable[2,3]<NB_AUC){
    ReduceFTable[2,3]<-NB_AUC
    ReduceFTable[2,2]<-i
    ReduceFTable[2,4]<-get_NB(columns,train,test)
  }
  if(ReduceFTable[3,3]<KNN_AUC){
    ReduceFTable[3,3]<-KNN_AUC
    ReduceFTable[3,2]<-i
    ReduceFTable[3,4]<-get_knn(columns,train,test)
  }
  if(ReduceFTable[4,3]<RF_AUC){
    ReduceFTable[4,3]<-RF_AUC
    ReduceFTable[4,2]<-i
    ReduceFTable[4,4]<-get_RF(fmla,train,test)
  }
  if(ReduceFTable[5,3]<GBM_AUC){
    ReduceFTable[5,3]<-GBM_AUC
    ReduceFTable[5,2]<-i
    ReduceFTable[5,4]<-get_gbm(fmla,train,test)
  }
  if(ReduceFTable[6,3]<SVM_AUC){
    ReduceFTable[6,3]<-SVM_AUC
    ReduceFTable[6,2]<-i
    ReduceFTable[6,4]<-get_svm(fmla,train,test)
  }
}

# kfold validation for all features
AllFTable<-data.frame(models = c("rpart","nb","knn","rf","gbm","svm"),bestval=0,valauc=0,testauc=0)
fmla<-"diagnosis~." # formula of all features
columns<-colnames(train[,-1]) # columns of all features
for(i in 1:5){
  train<-wbcd[-c(folds[[i]],folds[[(i%%5)+1]]),]
  val<-wbcd[folds[[i]],]
  test<-wbcd[folds[[(i%%5)+1]],]
  
  DT_AUC<-get_DT(fmla,train,val)
  NB_AUC<-get_NB(columns,train,val)
  KNN_AUC<-get_knn(columns,train,val)
  RF_AUC <-get_RF(fmla,train,val)
  GBM_AUC <-get_gbm(fmla,train,val)
  SVM_AUC <-get_svm(fmla,train,val)
  
  if(AllFTable[1,3]<DT_AUC){
    AllFTable[1,3]<-DT_AUC
    AllFTable[1,2]<-i
    AllFTable[1,4]<-get_DT(fmla,train,test)
  }
  if(AllFTable[2,3]<NB_AUC){
    AllFTable[2,3]<-NB_AUC
    AllFTable[2,2]<-i
    AllFTable[2,4]<-get_NB(columns,train,test)
  }
  if(AllFTable[3,3]<KNN_AUC){
    AllFTable[3,3]<-KNN_AUC
    AllFTable[3,2]<-i
    AllFTable[3,4]<-get_knn(columns,train,test)
  }
  if(AllFTable[4,3]<RF_AUC){
    AllFTable[4,3]<-RF_AUC
    AllFTable[4,2]<-i
    AllFTable[4,4]<-get_RF(fmla,train,test)
  }
  if(AllFTable[5,3]<GBM_AUC){
    AllFTable[5,3]<-GBM_AUC
    AllFTable[5,2]<-i
    AllFTable[5,4]<-get_gbm(fmla,train,test)
  }
  if(AllFTable[6,3]<SVM_AUC){
    AllFTable[6,3]<-SVM_AUC
    AllFTable[6,2]<-i
    AllFTable[6,4]<-get_svm(fmla,train,test)
  }
}



output = args$output
result<-cbind(AllFTable,ReduceFTable)

write.csv(result, output, row.names = FALSE, quote=FALSE)
