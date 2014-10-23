##===================================================================================================================
##===================================================================================================================
## velaraptor twitter.com/christophvel
##TUNING SUPPORT VECTOR MACHINES
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

tuneca<-tune.svm(train,labels[,1],scale=FALSE,gamma = 10^seq(-3,-1,by=1), cost = 10^seq(2,5,by=1))
tunep<-tune.svm(train,log(labels[,2]+1.5),scale=FALSE,gamma = 10^seq(-3,-1,by=1), cost = 10^seq(2,5,by=1))
tuneph<-tune.svm(train,labels[,3],scale=FALSE,gamma = 10^seq(-3,-1,by=1), cost = 10^seq(2,5,by=1))
tunesoc<-tune.svm(train,log(labels[,4]+2),scale=FALSE,gamma = 10^seq(-3,-1,by=1), cost = 10^seq(2,5,by=1))
tunesand<-tune.svm(train,labels[,5],scale=FALSE,gamma = 10^seq(-3,-1,by=1), cost = 10^seq(2,5,by=1))

##===================================================================================================================

bestca<-tuneca$best.model
bestp<-tunep$best.model
bestph<-tuneph$best.model
bestsoc<-tunesoc$best.model
bestsand<-tunesand$best.model

##===================================================================================================================
##===================================================================================================================
##Best Parameters for Ca: gamma:    cost:
##Best Parameters for P: gamma:    cost:
##Best Parameters for pH: gamma:    cost:
##Best Parameters for SOC: gamma:    cost:
##Best Parameters for Sand: gamma:    cost:
##===================================================================================================================
##===================================================================================================================

##===================================================================================================================
##TRAINING PREDICTIONS WITH TRAINING SET AND RMSE
##===================================================================================================================

predict.best.ca<-predict(bestca,data=train)
predict.best.p<-predict(bestp,data=train)
predict.best.p<-exp(predict.best.p)-1.5
predict.best.ph<-predict(bestph,data=train)
predict.best.soc<-predict(bestsoc,data=train)
predict.best.soc<-exp(predict.best.soc)-2
predict.best.sand<-predict(bestsand,data=train)

predictions.train<-list(predict.best.ca,predict.best.p,predict.best.ph,predict.best.soc,predict.best.sand)
rmse.train.tune<-lapply(1:5,
          function(i){
            sqrt(sum((predictions.train-labels[,i])^2)/(nrow(train)))
            }
          )
rmse.train.tune

##===================================================================================================================
##Root Mean Square Error FOR CA: P: pH: SOC: SAND:
##===================================================================================================================

##===================================================================================================================
##TEST PREDICTIONS AND PRINT
##===================================================================================================================

predict.best.ca<-predict(bestca,newdata=test)
predict.best.p<-predict(bestp,newdata=test)
predict.best.p<-exp(predict.best.p)-1.5
predict.best.ph<-predict(bestph,newdata=test)
predict.best.soc<-predict(bestsoc,newdata=test)
predict.best.soc<-exp(predict.best.soc)-2
predict.best.sand<-predict(bestsand,newdata=test)

predictions<-cbind(predict.best.ca,predict.best.p,predict.best.ph,predict.best.soc,predict.best.sand)
submission<-cbind(PIDN=submission,predictions)
write.csv(submission,"tunedsvm.csv",row.names=FALSE,quote=FALSE)
##===================================================================================================================


