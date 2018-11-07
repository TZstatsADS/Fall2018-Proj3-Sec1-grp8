#' ---
#' title: "Project 3 - Example Main Script"
#' author: "Chengliang Tang, Tian Zheng"
#' output:
#'   html_document:
#'     df_print: paged
#' ---
#' 
#' 
#' This file runs the evaluation experiments of image analysis (or any predictive modeling).
#' 
## ------------------------------------------------------------------------
if(!require("EBImage")){
  source("https://bioconductor.org/biocLite.R")
  biocLite("EBImage")
}

if(!require("gbm")){
  install.packages("gbm")
}

library("EBImage")
#install.packages("gbm")
library("gbm")

#' 
#' 
#' ### Step 0: specify directories.
#' 
#' Setting the working directory to the image folder. Specify the training and the testing set. 
#' 
## ----wkdir, eval=FALSE---------------------------------------------------
## set.seed(2018)
## # setwd("Fall2018-Proj3-Sec1-grp8/")
## # here replace it with your own path or manually set it in RStudio to where this rmd file is located.
## # use relative path for reproducibility

#' 
#' Provide directories for training images. Low-resolution (LR) image set and High-resolution (HR) image set will be in different subfolders. 
## ------------------------------------------------------------------------

train_dir <- "../data/train_set/" # This will be modified for different data sets.
list.dirs(path = train_dir, full.names = TRUE, recursive = TRUE)
train_LR_dir <- paste(train_dir, "LR/", sep="")
train_HR_dir <- paste(train_dir, "HR/", sep="")
train_label_path <- paste(train_dir, "label.csv", sep="") 

#' 
#' ### Step 1: set up controls for evaluation experiments.
#' 
#' In this chunk, we have a set of controls for the evaluation experiments. 
#' 
#' + (T/F) cross-validation on the training set
#' + (number) K, the number of CV folds
#' + (T/F) process features for training set
#' + (T/F) run evaluation on an independent test set
#' + (T/F) process features for test set
#' + (T/F) run the final training
#' 
## ----exp_setup-----------------------------------------------------------
run.cv=FALSE # run cross-validation on the training set
K <- 5  # number of CV folds
run.feature.train=FALSE # process features for training set
run.test=TRUE # run evaluation on an independent test set
run.feature.test=FALSE # process features for test set
run.final.train = FALSE # run the final training

#' 
#' Using cross-validation or independent test set evaluation, we compare the performance of models with different specifications. In this example, we use GBM with different `depth`. In the following chunk, we list, in a vector, setups (in this case, `depth`) corresponding to models that we will compare. Note that with higher parameters and a lower shrinkage, it was possible to improve the model, but the reonstruction of higher resolution images would have taken over 30 min. The parameters used below are the ones that allow for running in just under 30 min.
#' 
## ----model_setup---------------------------------------------------------
depths <- 2:3
trees <- c(10,20)
bags <- seq(0.3, 0.7, 0.1)


# higher parametrization attempts (too slow)
# depths<- c(3,4)
# trees<- c(150, 200, 300)
# bags <- 0.9

# lower shrinkage attempts (too slow)
# depths<- c(3,4)
# trees<- c(150, 200, 300)
# shrink <- 0.01
# model_values <- expand.grid(depths = depths, trees = trees, shrink = shrink)



# all combinations
model_values <- expand.grid(depths = depths, trees = trees, bags = bags)


# model_labels = paste("GBM with depth =", model_values)


#' 
#' 
#' ### Step 3: construct features and responses
#' 
#' `feature.R` has two alternatives for the sampling of images: random sampling or keypoint detection (via Laplace transform):
#' + Input: a path for low-resolution images.
#' + Input: a path for high-resolution images.
#' + Output: an RData file that contains extracted features and corresponding responses
#' 
## ----feature-------------------------------------------------------------
source("../lib/feature.R")

tm_feature_train <- NA
if(run.feature.train){
  tm_feature_train <- system.time(dat_train <- feature(train_LR_dir, train_HR_dir))
  feat_train <- dat_train$feature
  label_train <- dat_train$label
  save(dat_train, file="../output/feature_train.RData")
}


load(file="../output/feature_train.RData")
feat_train <- dat_train$feature
label_train <- dat_train$label


#' 
#' 
#' ### Step 4: Train a classification model with training images
#' Call the train model and test model from library. 
#' 
#' `train.R` and `test.R` should be wrappers for all your model training steps and your classification/prediction steps. 
#' + `train.R`
#'   + Input: a path that points to the training set features and responses.
#'   + Output: an RData file that contains trained classifiers in the forms of R objects: models/settings/links to external trained configurations.
#' + `test.R`
#'   + Input: a path that points to the test set features.
#'   + Input: an R object that contains a trained classifier.
#'   + Output: an R object of response predictions on the test set. If there are multiple classifiers under evaluation, there should be multiple sets of label predictions. 
## ----loadlib-------------------------------------------------------------
source("../lib/train.R")
source("../lib/test.R")

#' 
#' 
#' #### Model selection with cross-validation
#' * Do model selection by choosing among different values of training model parameters. 
## ----runcv, message=FALSE, warning=FALSE---------------------------------
source("../lib/cross_validation.R")

if(run.cv){
  err_cv <- array(dim=c(nrow(model_values), 2))
  for(k in 1:nrow(model_values)){
    cat("k =", k, "\n")
    err_cv[k,] <- cv.function(dat_train$feature, dat_train$label, model_values[k,], K)
  }
  save(err_cv, file="../output/err_cv.RData")
}

load(file="../output/err_cv.RData")


#' 
#' Visualize cross-validation results. 
## ----cv_vis--------------------------------------------------------------
if(run.cv){
  load(file="../output/err_cv.RData")
  library(scatterplot3d)
  scatterplot3d(model_values[,3],model_values[,2],err_cv[,1],color = as.factor(model_values[,1]),
                angle = 45,xlab = "bags",ylab="trees",zlab="err_cv", lab.z = 2)
  #dev.off()
  # load("../output/err_cv.RData")
  # plot(model_values, err_cv[,1], xlab="Interaction Depth", ylab="CV Error",
  #      main="Cross Validation Error", type="n", ylim=c(0, 0.25))
  # points(model_values, err_cv[,1], col="blue", pch=16)
  # lines(model_values, err_cv[,1], col="blue")
  # arrows(model_values, err_cv[,1] - err_cv[,2], model_values, err_cv[,1] + err_cv[,2], 
  #       length=0.1, angle=90, code=3)
}

#' 
#' 
#' * Choose the "best"" parameter value
## ----best_model----------------------------------------------------------
model_best=model_values[1,]
if(run.cv){
 model_best <- model_values[which.min(err_cv[,1]),]
}


#' 
#' * Train the model with the entire training set using the selected model (model parameter) via cross-validation.
## ----final_train---------------------------------------------------------
#tm_train=NA
#tm_train <- system.time(fit_train <- train(feat_train, label_train, model_best))
#save(fit_train, file="../output/fit_train.RData")

# final model for the presentation
model_best[1,] <- c(3, 20, 0.7)

if(run.feature.train){
  tm_train=NA
  tm_train <- system.time(fit_train <- train(feat_train, label_train, model_best))
  save(fit_train, file="../output/fit_train.RData")
}

load(file="../output/fit_train.RData")


#' 
#' ### Step 5: Super-resolution for test images
#' Feed the final training model with the completely holdout testing data. 
#' + `superResolution.R`
#'   + Input: a path that points to the folder of low-resolution test images.
#'   + Input: a path that points to the folder (empty) of high-resolution test images.
#'   + Input: an R object that contains tuned predictors.
#'   + Output: construct high-resolution versions for each low-resolution test image.
## ----superresolution-----------------------------------------------------
source("../lib/superResolution.R")
test_dir <- "../data/test_set/" # This will be modified for different data sets.
test_LR_dir <- paste(test_dir, "LR/", sep="")
test_HR_dir <- paste(test_dir, "HR/", sep="")

# library(profvis)
tm_test=NA
# profvis({
if(run.test){
  #load(file="../output/fit_train.RData")
  tm_test <- system.time(superResolution(test_LR_dir, test_HR_dir, fit_train))
}
# })

#' 
#' ### Peak Signal-to-noise Ratio
#' 
## ------------------------------------------------------------------------

test_dir <- "../data/test_set/" # This will be modified for different data sets.
test_reconst_dir <- paste(test_dir, "HR/", sep="")
test_original_dir <- paste(test_dir, "originals/", sep="")

source("../lib/PSNR.R")
sn <- psnr(test_reconst_dir, test_original_dir)
mean(sn)
plot(density(sn))
# save(sn, file = "../output/psnrGBM.RData")


#' 
#' 
#' 
#' ### Summarize Running Time
#' Prediction performance matters, so does the running times for constructing features and for training the model, especially when the computation resource is limited. 
## ----running_time--------------------------------------------------------
cat("Time for constructing training features=", tm_feature_train[1], "s \n")
cat("Time for constructing testing features=", tm_feature_test[1], "s \n")
cat("Time for training model=", tm_train[1], "s \n")
cat("Time for super-resolution=", tm_test[1], "s \n")

#' 
