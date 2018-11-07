#############################################################
### Construct features and responses for training images  ###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3

feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images


  # LR_dir <- "/Users/gabrielbenedict/Google_Drive/docs/UNIS/KU Leuven/Exchange/columbia/Courses/Applied Data Science/Projects/Fall2018-Proj3-Sec1-grp8/data/train_set/LR/"
  # HR_dir <- "/Users/gabrielbenedict/Google_Drive/docs/UNIS/KU Leuven/Exchange/columbia/Courses/Applied Data Science/Projects/Fall2018-Proj3-Sec1-grp8/data/train_set/HR/"

  # pad zeros first <- padding doesn't take time
  # imgLR.data matrix ? CHECK
  # important points (gradiant filter / variance) CHECK
  # 200 pixels per image CHECK
  # 1200 pics CHECK
  
  # 
  ### load libraries
  library("EBImage")
  
  # Lagrange transform
  fhi = matrix(1, nrow = 3, ncol = 3)
  fhi[2, 2] = -8
  
  
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  # library(doMC)
  # registerDoMC(cores=2)
  # foreach(i = 1:n_files) %dopar% {
  ### read LR/HR image pairs
  # foreach(p = 1:n_points) %dopar% {
  # profvis({
  # for(i in  (2 * n_files/3 + 1) : (2 * n_files/3 + 4)){
  # for(i in  (2 * n_files/3 + 1) : n_files){
  for(i in 1:n_files){  
  
    imgLRObj <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHRObj <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
      
    imgLR <- as.array(imgLRObj@.Data)
    imgHR <- as.array(imgHRObj@.Data)
  
    width <- dim(imgLR)[1]
    height <- dim(imgLR)[2]
    
    # Lagrange transform
    img_fhi = filter2(imgLR, fhi)
    img_fhi <- normalize(img_fhi)
  
    a <- (img_fhi[,,1] + img_fhi[,,2] + img_fhi[,,3])/3
    x <- order(a, decreasing = T)[1:n_points] %% width
    x[x==0] <- width # points on the right edge
    y <- order(a, decreasing = T)[1:n_points] %/% width
    y[y==0] <- 1 # points on the top edge
    
    # ## step 1. sample n_points from imgLR
    # set.seed(100)
    # x <- sample(1:width, n_points, replace = T)
    # y <- sample(1:height, n_points, replace = T)
    
    padded <- array(0, c(width+2, height+2, 3))
    padded[2:(width+1),2:(height+1),] <- imgLR
    
    ### step 2. for each sampled point in imgLR,
    
    for(p in 1:n_points){
        ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
        ###           tips: padding zeros for boundary points
  
        square <- padded[x[p]:(x[p] + 2), y[p]:(y[p] + 2), ]
        
        vectorized <- c(square)
        centPixel <- vectorized[c(5, 14, 23)]
        vectorized <- vectorized - (!vectorized == 0) * c(rep(vectorized[5], 9),
                                                          rep(vectorized[14], 9),
                                                          rep(vectorized[23], 9))
        vectorized <- vectorized[c(-5, -14, -23)] # no central pixel
        
        featMat[(i-1) * n_points + p,,] <- vectorized
        # featMat[(i-1) * n_points + p,,] <- array(vectorized, c(8,3))
    
        ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
    
        square <- imgHR[(x[p] * 2 - 1):(x[p] * 2), 
                              (y[p] * 2 - 1):(y[p] * 2), ]
        labMat[(i-1) * n_points + p,,] <- c(square) - c(rep(centPixel[1], 4), 
                                                        rep(centPixel[2], 4),
                                                        rep(centPixel[3], 4))
        # labMat[(i-1) * n_points + p,,] <- array(c(square), c(4,3))
    }
    imgLRObj <- NULL
    imgHRObj <- NULL
    print(i)
  }
  return(list(feature = featMat, label = labMat))
}
