# My first Xgboost script
av = read.csv("train_shelter.csv")
av_test=read.csv("test_shelter.csv")

library(caTools)
require(xgboost)
require(Matrix)
require(data.table)

set.seed(1998)

av$AnimalType=as.numeric(av$AnimalType)
av_test$AnimalType=as.numeric(av_test$AnimalType)
train=av
test=av_test

cat ("feature engineering");

train$OutcomeType=NULL
train$AnimalID=NULL
id=test$ID
test$ID=NULL
train$Name=NULL
test$Name=NULL
tmp=as.Date.factor(train$DateTime)
year=as.factor(format(tmp,"%Y"))
train$year=year
train$DateTime=NULL
tmp=as.Date.factor(test$DateTime)
year=as.factor(format(tmp,"%Y"))
test$year=year
test$DateTime=NULL
train$OutcomeType=NULL
# convert character levels to numeric
train.y=as.numeric(train$OutcomeSubtype)-1
train$OutcomeSubType=NULL
pred <- rep(0,nrow(test));
cat("Creating data.matrix...\n");
trainM<-data.matrix(train, rownames.force = NA);
dtrain <- xgb.DMatrix(data=trainM, label=train.y, missing = NaN);
watchlist <- list(trainM=dtrain);
param <- list(  objective           = "multi:softprob",
                num_class           = 17,
                eval_metric         = "mlogloss",
                eta                 = 0.4,
                max_depth           = 6,
                subsample           = 0.40,
                colsample_bytree    = 0.40
)

clf <- xgb.cv(  params              = param, 
                data                = dtrain, 
                nrounds             = 1500, 
                verbose             = 1,
                watchlist           = watchlist,
                maximize            = FALSE,
                nfold               = 3,
                early.stop.round    = 10,
                print.every.n       = 1
);
bestRound <- which.min( as.matrix(clf)[,3] );
cat("Best round:", bestRound,"\n");
cat("Best result:",min(as.matrix(clf)[,3]),"\n");

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = bestRound, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)
testM <-data.matrix(test, rownames.force = NA);
preds <- predict(clf, testM,missing=NA)
output=preds+1
test$sot=output
train$sot=train.y+1
cat("actual prediction task")
train.y=as.numeric(av$OutcomeType)-1
pred <- rep(0,nrow(test));
cat("Creating data.matrix...\n");
trainM<-data.matrix(train, rownames.force = NA);
dtrain <- xgb.DMatrix(data=trainM, label=train.y, missing = NaN);
watchlist <- list(trainM=dtrain);
param <- list(  objective           = "multi:softprob",
                num_class           = 5,
                eval_metric         = "mlogloss",
                eta                 = 0.1,
                max_depth           = 6,
                subsample           = 0.40,
                colsample_bytree    = 0.40
)

clf <- xgb.cv(  params              = param, 
                data                = dtrain, 
                nrounds             = 1500, 
                verbose             = 1,
                watchlist           = watchlist,
                maximize            = FALSE,
                nfold               = 3,
                early.stop.round    = 10,
                print.every.n       = 1
);
bestRound <- which.min( as.matrix(clf)[,3] );
cat("Best round:", bestRound,"\n");
cat("Best result:",min(as.matrix(clf)[,3]),"\n");

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = bestRound, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)
testM <-data.matrix(test, rownames.force = NA);
preds <- predict(clf, testM,missing=NA)
result=matrix(preds,nrow=5,ncol=length(preds)/5)+1
result=t(result)
colnames(result)= c("Adoption","Died","Euthanasia","Return_to_owner","Transfer")
submission=data.frame(ID=test.id,result)
write.csv(submission, "shelter.csv", row.names = F);