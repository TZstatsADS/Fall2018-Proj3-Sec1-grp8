#########################################################
### Train a classification model with training features ###
#########################################################

### Author:Zhengyagn Xu
### Project 3

# 
# train <- function(feat_train, label_train, par=NULL){
#   
#   ### Train a Gradient Boosting Model (GBM) using processed features from training images
#   
#   ### Input: 
#   ###  -  features from LR images 
#   ###  -  responses from HR images
#   ### Output: a list for trained models
#   
#   ### load libraries
#   library("gbm")
#   
#   ### creat model list
#   modelList <- list()
#   
#   ### Train with gradient boosting model
#   if(is.null(par)){
#     depth <- 3
#     trees <- 10
#     bags <- 0.5
#   } else {
#     depth <- par$depth
#     trees <- par$trees
#     bags <- par$bags
#   }
#   
#   ## changing the number of trees from 200 to 50 brought a negligeable increase of rmse from
#   ## 2.589731e-08 to 2.850652e-08 
#   
#   
#   
#   ### the dimension of response arrat is * x 4 x 3, which requires 12 classifiers
#   ### this part can be parallelized
#   # profvis({
#   for (i in 1:12){
#   # for (i in 1:2){
#   # foreach(i = 1:2) %dopar% {
#   # f <- function(i){
#     ## calculate column and channel
#     c1 <- (i-1) %% 4 + 1
#     c2 <- (i-c1) %/% 4 + 1
#     featMat <- feat_train[, , c2]
#     labMat <- label_train[, c1, c2]
#     fit_gbm <- gbm.fit(x=featMat, y=labMat,
#                          n.trees=trees,
#                          distribution="gaussian",
#                          interaction.depth=depth, 
#                          bag.fraction = bags,
#                          verbose=FALSE)
#     best_iter <- gbm.perf(fit_gbm, method="OOB", plot.it = FALSE)
#     modelList[[i]] <- list(fit=fit_gbm, iter=best_iter)
#   }
#   # })
#   
#   # library(parallel)
#   # system.time(modelList <- lapply(1:2, f))
#   # system.time(modelList <- mclapply(1:2, f))
#   
#   return(modelList)
#   
# }

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
    max_depth = 7
    eta = 0.17
    gamma = 0.5
    min_child_weight = 6.5
    colsample_bytree=1
    subsample = 1
    nrounds = 100
  } 
  else {
    eta <- par$eta
    max_depth <- par$max_depth
    gamma <- par$gamma
    min_child_weight = par$min_child_weight
    subsample <- par$subsample
    nrounds = par$nrounds
    colsample_bytree=par$colsample_bytree
    
  }
  
  for (i in 1:12){
    # convert trainning data to data frame
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- feat_train[, , c2]
    labMat <- label_train[, c1, c2]
    fit_xgb <- xgboost(data = data.matrix(featMat),
                       label = as.numeric(labMat) ,
                       nrounds = nrounds,
                       max_depth = max_depth,
                       eta = eta,
                       gamma = gamma,
                       colsample_bytree = colsample_bytree, 
                       early_stopping_rounds = 80,
                       min_child_weight = min_child_weight,
                       subsample = subsample,
                       eval_metric = "rmse",
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

# 
# train_xgboost <- function(traindata){
#   # traindata has to be a matrix
#   timestart <- Sys.time()
#   # Data Preparation
#   xgb.train.data <- xgb.DMatrix(data = traindata[,-1],label = traindata[,1] - 1)
#   # Default Parameter 
#   numberOfClasses <- length(unique(traindata[,1]))
#   xgb_params <- list("objective" = "multi:softmax",
#                      "eval_metric" = "mlogloss",
#                      "num_class" = numberOfClasses,
#                      "silent"="0")
#   cv_model <- xgb.cv(params = xgb_params,
#                      data = xgb.train.data, 
#                      nrounds = 200,
#                      nfold = 10,
#                      verbose = FALSE,
#                      prediction = TRUE)
#   min_logloss = min(cv_model[["evaluation_log"]][, 4])     
#   min_logloss_index = (1:200)[cv_model[["evaluation_log"]][, 4]==min_logloss]   
#   xgb_fit <- xgb.train(data = xgb.train.data,    
#                        nround = min_logloss_index, 
#                        params = xgb_params)
#   timeend <- Sys.time()
#   runningtime <- timeend - timestart
#   return(list(fit = xgb_fit, time = runningtime))
# }
