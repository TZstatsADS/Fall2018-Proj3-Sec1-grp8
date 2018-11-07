#########################################################
### Train a classification model with training features ###
#########################################################

### Author:Zhengyang Xu
### Project 3


xgb.train.begin = Sys.time()
train.xgb <- function(feat_train,label_train, par = NULL){
  
  ### Train a Xgboost classifier using features of training images
  
  ### Input:
  ### - training data including features of training images 
  ###                       and class labels of training images
  ### Output:
  ### - training Xgboost model specification
  
  ### load libraries
  library("xgboost")
  
  ### train with Xgboost
  modelList = list()
  
  

  
  if(is.null(par)){
    max_depth = 4
    eta = 0.15
    gamma = 0.1
    min_child_weight = 6.5
    colsample_bytree=1
    subsample = 0.6
    nrounds = 100
    lambda=0.5
    alpha=0.5
    
  } 
  else {
    eta <- par$eta
    max_depth <- par$max_depth
    gamma <- par$gamma
    min_child_weight = par$min_child_weight
    subsample <- par$subsample
    nrounds = par$nrounds
    colsample_bytree=par$colsample_bytree
    lambda= par$lambda
    alpha = par$alpha
  }
  
  for (i in 1:12){
    # convert trainning data to data frame
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- feat_train[, , c2]
    labMat <- label_train[, c1, c2]
    fit_xgb <- xgboost(data = data.matrix(featMat),
                       label = as.numeric(labMat) ,
                       booster = "gblinear",
                       nrounds = 50,
                       max_depth = max_depth,
                       lambda = 0.5,
                       eta = eta,
                       gamma = gamma,
                       colsample_bytree = colsample_bytree,
                       early_stopping_rounds = 30,
                       min_child_weight = min_child_weight,
                       subsample = subsample,
                       eval_metric = "rmse",
                       alpha = 0.5,
                       objective = "reg:linear")
    modelList[i] <- list(fit_xgb)
    print(i)
  }
  return(modelList)
}

# modelList = train.xgb(feat_train,lab_train)

xgb.train.end = Sys.time()
xgb.train.time = xgb.train.end- xgb.train.begin

xgb.train.time

