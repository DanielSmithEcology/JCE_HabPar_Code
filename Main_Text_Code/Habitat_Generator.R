#################################################################################
############################ Habitat Generator ##################################
#################################################################################

# This code will generate the spatially explicit environment of habitats on the patch level. 
# That is, it will generate a M x M matrix of different values [0,1] such that denote the patch type 
# NOTE: the names of pathces here are slightly different from what is reference in the main text 
       # The main text uses "MedHigh" for high and "MedLow" for low. This was for space reasons. 
       # This does not change and qualitative results. 


# Dowload / load required packages 
library(NLMR)
library(sp)
library(raster)
library(plyr)
library(remotes)

# required functions 
uniformize <- function(x) {
  ans_x <- x
  for (idx in seq(1,length(x))){
    max_idx <- max(which(CDF.Hist$mids < x[idx]))
    ans_x[idx] <- empirical_cumulative_distribution[max_idx]
  }
  return(ans_x)
}

# Size of community (baseline -- algorithm will reduce it to 499 x 499)
dm <- 500

########################################################
### sequence of inputs that will generate landscapes  ##
########################################################

### High autocorrelation  ###

# set seed
set.seed(100) 
# make landscape of patch types with  a midpoint displacement neutral landscape model
r <- nlm_mpd(dm,dm, resolution = 1, roughness = 0.25, rand_dev = 1, torus = T, rescale = TRUE, verbose = TRUE)
# Turn into matrix (instead of raster)
Mat.S.H <- t(matrix(as.vector(r),nrow=nrow(r),ncol=ncol(r)))
# Turn into vector 
values <- (as.vector(Mat.S.H))
# Histogram of patch types (making CDF)
CDF.Hist <- hist(Mat.S.H,breaks=length(Mat.S.H))
# Cumulative distribution of the patch types 
empirical_cumulative_distribution  <- cumsum(CDF.Hist$counts)/length(Mat.S.H)
# apply function "uniformize" which will make all patches uniformly distributed in frequency
uniform2 <- uniformize(values)
uniform2 <- uniform2

# remove NAs (there will often be several NAs) and replace them with random patches. This unimportant quantitatively, but they will generate errors in the simulations, so they need to be replaced. 
NAs <- which(is.na(uniform2))
for(nn in 1:length(NAs)){
  
  uniform2[NAs[nn]] <- runif(0,1,n=1)
} 

# Turn the modified landscape into raster data
r[] <- uniform2
# transform the raster data into a matrix 
Mat.S.H <- t(matrix(as.vector(r),nrow=nrow(r),ncol=ncol(r)))
# Plot (to inspect)
plot(r,col=rainbow(100))

# Save the landscape as a CSV 
write.csv(Mat.S.H,"Hab_High.csv",quote=F,row.names=F)


#### MedHigh autocorrelation ####
# NOTE: I don't comment the code. 
# This is because: except minor differences in the implementation of the "nlm_mpd" function below (where roughness is modified between) the code is identical
# each block of code below will simply produce landscapes using the same methods as above, but with different levels of autocorrelation 

set.seed(100)
r <- nlm_mpd(dm,dm, resolution = 1, roughness = 0.5, rand_dev = 1, torus = T, rescale = TRUE, verbose = TRUE)

Mat.S.H <- t(matrix(as.vector(r),nrow=nrow(r),ncol=ncol(r)))
values <- (as.vector(Mat.S.H))
CDF.Hist <- hist(Mat.S.H,breaks=length(Mat.S.H))
empirical_cumulative_distribution  <- cumsum(CDF.Hist$counts)/length(Mat.S.H)

uniform2 <- uniformize(values)
uniform2 <- uniform2

NAs <- which(is.na(uniform2))
for(nn in 1:length(NAs)){
  
  uniform2[NAs[nn]] <- runif(0,1,n=1)
} 
r[] <- uniform2
Mat.S.H <- t(matrix(as.vector(r),nrow=nrow(r),ncol=ncol(r)))
plot(r,col=rainbow(10))


write.csv(Mat.S.H,"Hab_MedHigh.csv",quote=F,row.names=F)



# Med autocorrelation 
set.seed(100)

r <- nlm_mpd(dm,dm, resolution = 1, roughness = 0.75, rand_dev = 1, torus = T, rescale = TRUE, verbose = TRUE)

Mat.S.H <- t(matrix(as.vector(r),nrow=nrow(r),ncol=ncol(r)))
values <- (as.vector(Mat.S.H))
CDF.Hist <- hist(Mat.S.H,breaks=length(Mat.S.H))
empirical_cumulative_distribution  <- cumsum(CDF.Hist$counts)/length(Mat.S.H)

uniform2 <- uniformize(values)
uniform2 <- uniform2
#mean(uniform2)
#hist(uniform2, breaks=10)
NAs <- which(is.na(uniform2))
for(nn in 1:length(NAs)){
  
  uniform2[NAs[nn]] <- runif(0,1,n=1)
} 
r[] <- uniform2
Mat.S.H <- t(matrix(as.vector(r),nrow=nrow(r),ncol=ncol(r)))
plot(r,col=rainbow(10))


write.csv(Mat.S.H,"Hab_Med.csv",quote=F,row.names=F)


# MedLow Autocorrelation 
set.seed(100)

r <- nlm_mpd(dm,dm, resolution = 1, roughness = 1, rand_dev = 1, torus = T, rescale = TRUE, verbose = TRUE)

Mat.S.H <- t(matrix(as.vector(r),nrow=nrow(r),ncol=ncol(r)))
values <- (as.vector(Mat.S.H))
CDF.Hist <- hist(Mat.S.H,breaks=length(Mat.S.H))
empirical_cumulative_distribution  <- cumsum(CDF.Hist$counts)/length(Mat.S.H)

uniform2 <- uniformize(values)
uniform2 <- uniform2
mean(uniform2)
NAs <- which(is.na(uniform2))
for(nn in 1:length(NAs)){
  
  uniform2[NAs[nn]] <- runif(0,1,n=1)
} 
r[] <- uniform2
Mat.S.H <- t(matrix(as.vector(r),nrow=nrow(r),ncol=ncol(r)))
plot(r,col=rainbow(10))


write.csv(Mat.S.H,"Hab_MedLow.csv",quote=F,row.names=F)


# Low Autocorrelation 

set.seed(100)

r <- nlm_mpd(dm,dm, resolution = 1, roughness = 1.25, rand_dev = 1, torus = T, rescale = TRUE, verbose = TRUE)
#plot(r,col=rainbow(80))

Mat.S.H <- t(matrix(as.vector(r),nrow=nrow(r),ncol=ncol(r)))
values <- (as.vector(Mat.S.H))
CDF.Hist <- hist(Mat.S.H,breaks=length(Mat.S.H))
empirical_cumulative_distribution  <- cumsum(CDF.Hist$counts)/length(Mat.S.H)
uniform2 <- uniformize(values)
uniform2 <- uniform2
#mean(uniform2)
#hist(uniform2, breaks=10)
NAs <- which(is.na(uniform2))
for(nn in 1:length(NAs)){
  
  uniform2[NAs[nn]] <- runif(0,1,n=1)
} 
r[] <- uniform2
Mat.S.H <- t(matrix(as.vector(r),nrow=nrow(r),ncol=ncol(r)))
plot(r,col=rainbow(10))


write.csv(Mat.S.H,"Hab_Low.csv",quote=F,row.names=F)


# Rand autocorrelation 
Mat.S.H2 <- sample(Mat.S.H,replace = F)
r[] <- Mat.S.H2
write.csv(Mat.S.H,"Hab_Rand.csv",quote=F,row.names=F)












