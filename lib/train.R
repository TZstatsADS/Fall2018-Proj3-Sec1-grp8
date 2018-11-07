#########################################################
### Train a classification model with training features ###
#########################################################

### Author: Chengliang Tang
### Project 3


train <- function(feat_train, label_train, par=NULL){
  
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  features from LR images 
  ###  -  responses from HR images
  ### Output: a list for trained models
  
  ### load libraries
  library("gbm")
  
  ### creat model list
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 3
    trees <- 10
    bags <- 0.5
    shrink <- 0.1
  } else {
    depth <- par$depth
    trees <- par$trees
    bags <- par$bags
    shrink <- 0.1
  }
  
  ## changing the number of trees from 200 to 50 brought a negligeable increase of rmse from
  ## 2.589731e-08 to 2.850652e-08 
  
  
  
  ### the dimension of response arrat is * x 4 x 3, which requires 12 classifiers
  ### this part can be parallelized
  # profvis({
  for (i in 1:12){
  # for (i in 1:2){
  # foreach(i = 1:2) %dopar% {
  # f <- function(i){
    ## calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- feat_train[, , c2]
    labMat <- label_train[, c1, c2]
    fit_gbm <- gbm.fit(x=featMat, y=labMat,
                         n.trees=trees,
                         distribution="gaussian",
                         interaction.depth=depth, 
                         bag.fraction = bags,
                         verbose=FALSE,
                         shrinkage = shrink)
    best_iter <- gbm.perf(fit_gbm, method="OOB", plot.it = FALSE)
    modelList[[i]] <- list(fit=fit_gbm, iter=best_iter)
  }
  # })
  
  # library(parallel)
  # system.time(modelList <- lapply(1:2, f))
  # system.time(modelList <- mclapply(1:2, f))
  
  return(modelList)
  
}
