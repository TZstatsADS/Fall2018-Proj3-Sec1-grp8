#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3

feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images

# 
#   LR_dir <- "/Users/gabrielbenedict/Google_Drive/docs/UNIS/KU Leuven/Exchange/columbia/Courses/Applied Data Science/Projects/Fall2018-Proj3-Sec1-grp8/data/train_set/LR/"
#   HR_dir <- "/Users/gabrielbenedict/Google_Drive/docs/UNIS/KU Leuven/Exchange/columbia/Courses/Applied Data Science/Projects/Fall2018-Proj3-Sec1-grp8/data/train_set/HR/"

  # 
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    ### step 1. sample n_points from imgLR
    width <- dim(imgLR@.Data)[1]
    height <- dim(imgLR@.Data)[2]
    x <- sample(1:width, n_points, replace = T)
    y <- sample(1:height, n_points, replace = T)
    
    
    ### step 2. for each sampled point in imgLR,
    
    for(p in 1:n_points){
    
        ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
        ###           tips: padding zeros for boundary points

        # define square (don't allow for out of bound subscripts)
        square <- imgLR@.Data[
          (x[p] - 1):as.integer(((x[p] + 1) > width) * width 
                                + ((x[p] + 1) <= width) * (x[p] + 1)), 
          (y[p] - 1):as.integer(((y[p] + 1) > height) * height 
                                + ((y[p] + 1) <= height) * (y[p] + 1)), ]
        
        
        # padding zeros for boundary points
        if(dim(square)[1] < 3){
          square <- apply(square, c(2, 3), c, 0)
        }
        if(dim(square)[2] < 3){
          # https://stackoverflow.com/questions/27637393/adding-column-or-row-in-3d-array
          square <- aperm(apply(square, c(1, 3), c, 0), c(2, 1, 3))          
        }
        
        featMat[p,,] <- c(square)[c(-5, -14, -23)] # no central pixel
    

        ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
    
        square <- imgHR@.Data[(x[p] * 2 - 1):(x[p] * 2), 
                              (y[p] * 2 - 1):(y[p] * 2), ]
        labMat[p,,] <- c(square)
    }
  }
  return(list(feature = featMat, label = labMat))
}
