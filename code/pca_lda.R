#rscript pca_lda.R --data data.csv --output result_lda_1.csv --pcaimportance pcaimportance.csv

library(ggbiplot)
library(MASS)
library(ggplot2)
library("argparse")
library(caret)
library(ROCR)

parser <- ArgumentParser()
parser$add_argument('--data', help='data')
parser$add_argument('--output', help='performance')
parser$add_argument('--pcaimportance', help='pcaimportance')
args = parser$parse_args()
pdf("plot.pdf")

# read data
wdata <- read.csv(args$data, header = T, stringsAsFactors=F)
#wdata <- read.csv("data.csv", stringsAsFactors = F) 

# remove null column and Id column
wdata$X <- NULL
wdata <- wdata[,-1]
# View(wdata)

# spilt data and diagnosis
wdata.data<-wdata[,-1]
diagnosis<-ifelse(wdata[,1]=="B",1,0)

# run pca
wdata.pca <- prcomp(wdata.data, scale = TRUE, center = TRUE)

# get pca plot
ggbiplot(wdata.pca,choices=c(1,2), obs.scale = 1, var.scale = 1,groups = wdata$diagnosis)+
  scale_color_discrete(name = 'diganosis')


# get importance information of PCA
a<-data.frame(unclass(summary(wdata.pca))$importance)
b<-data.frame(PC=c(1:30),t(a[2,]))

# plot variances
ggplot(data=b, aes(x=PC,y=Proportion.of.Variance)) +
  geom_line()+
  geom_point()

# get PCA data
wdata.pca.all<-as.data.frame(cbind(diagnosis,wdata.pca$x))

# calculate AUC 
calcAUC<-function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==1),'auc')
  as.numeric(perf@y.values)
}

# get lda predict
getLdaPred<- function(test,train){
  ldamodel<-lda(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = train) # train model
  pred<- predict(ldamodel, newdata = test) # get prediction
  as.data.frame(pred$posterior)[,2]
}

# split data k fold 
kfold<-6
set.seed(1111)
folds<-createFolds(y=wdata.pca.all$diagnosis,k=kfold)

# store the best fold and auc score
best<-0
bestauc<-0
trainscore<-c()
valscore<-c()
testscore<-c()
# run k fold
for(i in 1:kfold){
  #split data to train, val and test
  train<-wdata.pca.all[-c(folds[[i]],folds[[(i%%kfold)+1]]),]
  val<-wdata.pca.all[folds[[i]],]
  test<-wdata.pca.all[folds[[(i%%kfold)+1]],]
  #print(paste("train:",nrow(train),"val:",nrow(val),"test:",nrow(test)))
  
  # get train auc
  lda.predict <- getLdaPred(train,train)
  lda.train.auc <- calcAUC(lda.predict,train$diagnosis)
  trainscore<-c(trainscore,lda.train.auc)
  
  # get val auc
  lda.predict <- getLdaPred(val,train)
  lda.val.auc <- calcAUC(lda.predict,val$diagnosis)
  valscore<-c(valscore,lda.val.auc)
  
  # get test auc
  lda.predict <- getLdaPred(test,train)
  lda.test.auc <- calcAUC(lda.predict,test$diagnosis)
  testscore<-c(testscore,lda.test.auc)
  
}

trainscore<-c(trainscore,mean(trainscore))
valscore<-c(valscore,mean(valscore))
testscore<-c(testscore,mean(testscore))

perfTable<-data.frame(train=trainscore,val=valscore,test=testscore)
perfTable<-data.frame(set=c(1:kfold,"avg."),round(perfTable,4))


#get the best val score fold
best <- match(max(valscore),valscore)
  
#best fold
i<-best[1]
train<-wdata.pca.all[-c(folds[[i]],folds[[(i%%kfold)+1]]),]
test<-wdata.pca.all[folds[[(i%%kfold)+1]],]


lda.predict <- getLdaPred(test,train)
auc <- calcAUC(lda.predict,test$diagnosis)
print(paste("best fold:",i,"test auc:",auc))

# confusion matrix
lda.pred<-ifelse(lda.predict>0.8,"benign","malignant")
real.data<-ifelse(test$diagnosis==1,"benign","malignant")
cm<- table(real.data,lda.pred) 
confusionMatrix(cm)

# density plot
lda.predict<-data.frame(lda.predict)
Diagnosis_<-ifelse(test$diagnosis==1,"benign","malignant")
ggplot(lda.predict, aes(x=lda.predict, fill=Diagnosis_)) + geom_density(alpha=0.25)


# plot ROC of test
roc.perf <- performance(prediction(lda.predict, test$diagnosis), measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc[[1]],5), sep = ""))

# output performance table
output1<-args$output
output2<-args$pcaimportance

write.csv(perfTable, output1, row.names = FALSE, quote=FALSE)
write.csv(round(a,2), output2, row.names = TRUE, quote=FALSE)

