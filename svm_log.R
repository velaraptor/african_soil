##===================================================================================================================
##===================================================================================================================
## velaraptor twitter.com/christophvel
## christophvel@gmail.com
##===================================================================================================================
## Using Support Vector Machines With Log Transformations for SOC and P 
## Throughout the competition I tuned GBM, Random Forests, and Support Vector Machines, but due to overfitting in 
## GBM models, and trying to use ensemble learning methods, I decided to submit this model since it seemed to be the
## most effective model. 
##===================================================================================================================
##===================================================================================================================
library(e1071)
set.seed(57)
train <- read.csv("training.csv",header=TRUE,stringsAsFactors=FALSE)
test <- read.csv("sorted_test.csv",header=TRUE,stringsAsFactors=FALSE)
submission <- test[,1] 
labels <- train[,c("Ca","P","pH","SOC","Sand")]
train <- train[,2:3579]
test <- test[,2:3579]
##===================================================================================================================
svms <- lapply(1:ncol(labels),
               function(i)
               {
                 svm(train,labels[,i],cost=10000,scale=FALSE)
               })
predictions <- sapply(svms,predict,newdata=test)     
colnames(predictions) <- c("Ca","P","pH","SOC","Sand")  
##===================================================================================================================
##I decided to transform the predictors P and SOC based on Exploratory Analysis and came up with these numbers 
##to make the predictor values all positive and used a log transformation since it seemed what the plots showed
## The RMSE using this approach benefited and lowered it. 
##===================================================================================================================
p.log.svm<-svm(train,log(labels$P+1.5),cost=100000,scale=FALSE) 
svm.log.p<-predict(p.log.svm,newdata=test)              
svm.fix.p<-exp(svm.log.p)-1.5
predictions<-as.data.frame(predictions)
predictions$P<-svm.fix.p
##===================================================================================================================
soc.log.svm<-svm(train,log(labels$SOC+2),cost=100000,scale=FALSE) 
svm.log.soc<-predict(soc.log.svm,newdata=test)              
svm.fix.soc<-exp(svm.log.soc)-2
predictions$SOC<-svm.fix.soc
##===================================================================================================================
submission <- cbind(PIDN=submission,predictions)
write.csv(submission,"logbasedsvm.csv",row.names=FALSE,quote=FALSE)
