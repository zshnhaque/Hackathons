library(readr)
train <- read_csv("C:/Users/Zeeshan/Downloads/train_u6lujuX_CVtuZ9i (1).csv")
test<- read_csv("C:/Users/Zeeshan/Downloads/test_Y3wMUE5_7gLdaTN.csv")
h<-dim(train)[2]
l<-dim(train)[1]
h_test<-dim(test)[2]
l_test<-dim(test)[1]

all_feature<-rbind(train[,1:h_test],test[,1:h_test])

for(i in 2:h_test){
  if(i!=11){
  if(class(all_feature[,i])=="integer" || class(all_feature[,i])=="numeric" ){
      temp<-as.numeric(table(is.na(all_feature[,i])))
        if(temp[1]>0){
        temp1<-mean(all_feature[,i],na.rm=TRUE)
        for(j in 1:(l+l_test)){
            if(is.na(all_feature[j,i]))
              {
               all_feature[j,i]<-as.integer(temp1)
              }
            }
        }
  }
  else if(class(all_feature[,i])=="character")
 { 
    n<-names(table(all_feature[,i]))
    t<-as.numeric(table(all_feature[,i]))
    temp1<-n[match(max(t),t)]
    for(j in 1:(l+l_test))
      {
      if(is.na(all_feature[j,i]))
        {
        all_feature[j,i]<-temp1
      }
    }
  }
  }
}

for(j in 1:(l+l_test)){
  if(is.na(all_feature[j,11]))
     {
        all_feature[j,11]<-1
      }
  }

for(i in 2:h_test)
{
  if(class(all_feature[,i])=="character"){
    all_feature[,i]<-as.factor(all_feature[,i])
    all_feature[,i]<-as.numeric(all_feature[,i])
  }
}


all_feature$ApplicantIncome<-log(all_feature$ApplicantIncome+1)
all_feature$CoapplicantIncome<-log(all_feature$CoapplicantIncome+1)
all_feature$LoanAmount<-log(all_feature$LoanAmount+1)

X_train<-all_feature[1:l,c(-1)]
X_test<-all_feature[(l+1):(l+l_test),c(-1)]

target<-train$Loan_Status
target<-as.factor(target)
target<-as.numeric(target)
target<-target-1

require(xgboost)
dtrain <- xgb.DMatrix(data = data.matrix(X_train[,]),label = target)
evalerror <- function(preds, dtrain)
{
  labels <- getinfo(dtrain, "label")
  err <- as.numeric(sum(abs(labels - preds)))/length(labels)
  return(list(metric = "mae error", value = err))
}

watchlist <- list(train = dtrain)

param <- list(  objective = "binary:logistic",booster = "gbtree",eval_metric = evalerror,eta = 0.01,max_depth = 5,subsample = 0.8,colsample_bytree = 0.8,min_child_weight = 1)

xgbmodel <- xgb.train(params = param,data = dtrain,nrounds = 500,verbose = 2,watchlist = watchlist,maximize = FALSE)

pred<-predict(xgbmodel, data.matrix(X_test[,]))

Loan_Status<-rep(" ",l_test)
for(i in 1:l_test){
if(pred[i]>=0.5)
{
Loan_Status[i]<-"Y"
}
else
{
Loan_Status[i]<-"N"
}
}

submission<-data.frame(test$Loan_ID,Loan_Status)

names(submission)<-c("Loan_ID","Loan_Status")
write_csv(submission,"C:/Users/Zeeshan/Downloads/Loan_Submission_2.csv")




