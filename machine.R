library(caret)
pmltrain<-read.csv("pml-training.csv")
str(pmltrain)
names(pmltrain)
pmltrainok<-pmltrain[,!apply(is.na(pmltrain), 2, any)]
str(pmltrainok)
nsv <- nearZeroVar(pmltrainok,saveMetrics=TRUE)
good<-rownames(nsv)[nsv$nzv==FALSE]
pmltrainred<-pmltrainok[,names(pmltrainok)%in%good]

pmltrainokk<-pmltrainred[,-c(1:6)]
 str(pmltrainokk)

set.seed(3455)
inTrain <- createDataPartition(y=pmltrainokk$classe,
                               p=0.4, list=FALSE)
training <- pmltrainokk[inTrain,]
testing <- pmltrainokk[-inTrain,]

#nsv <- nearZeroVar(training,saveMetrics=TRUE)
#M <- abs(cor(training[,-53]))
#diag(M) <- 0
#which(M > 0.8,arr.ind=T)

#modelFit <- train(training$classe ~ .,method="glm",preProcess="pca",data=training)
#confusionMatrix(testing$classe,predict(modelFit,testing))
#modelFit <- train(training$classe ~ .,method="rpart",data=training)

modFit <- train(classe~.,data=training,method="rf",
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)
modFit
#pred <- predict(modFit,testing); testing$predRight <- pred==testing$classe
confusionMatrix(testing$classe,predict(modFit,testing))

pmltest<-read.csv("pml-testing.csv")
pred <- predict(modFit,pmltest)