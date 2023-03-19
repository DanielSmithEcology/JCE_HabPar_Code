########################################################################
################# Appendix Invasion criteria validation ################
########################################################################

#  this code will compare the approximate invasion criteria to exact invasion criteria 
#  Set your working directory to the "CSV_Files" folder 

##### Required packages #####
library(scales)

##############################################
############### Functions ####################
##############################################

# Turn matrix into torus
mat.torus        <- function(Matrix,Rad,xcord,ycord){      # Torus of Space
  
  dm    <- nrow(Matrix)                               # dimension of matrix (n x n marix, so rows = col)
  
  Crown.Pot       <-  c(seq(1,Rad,by=2)^2)-1    # arbitrarily set to 10, because that many neighbors gets silly biologically, just used for computation 
  Crowns          <-  seq(0,length(Crown.Pot))
  Crown           <-  Crowns[min(which(Crown.Pot >= (Rad-1)^2))]  #returns crown exension total (even if not whole crown)
  
  rows  <- c(  (xcord-Crown):(xcord+Crown)       )    # figure out which rows of matrix crown falls in
  cols  <- c(  (ycord-Crown):(ycord+Crown)       )    # figure out which columns of matrix crown falls in
  
  rows[which(rows<1)]    <- rows[which(rows<1)] + dm  # if crown extends to a row value less than 1, go to opposite side of torus 
  rows[which(rows>dm)]   <- rows[which(rows>dm)] - dm # if crown extends to a row value greater than dm, go to opposite side of torus
  
  cols[which(cols<1)]    <- cols[which(cols<1)] + dm  # if crown extends to a column value less than 1, go to opposite side of torus 
  cols[which(cols>dm)]   <- cols[which(cols>dm)] - dm # if crown extends to a column value greater than dm, go to opposite side of torus
  
  JC_Matrix              <- Matrix[rows,cols ]        # returns subset of matrix / trees that are in JCE zone + extras 
  
  return(JC_Matrix)
  
}

# Gaussian Function of Fitness + habtitat Type 
Habitat_Survival <- function(x,y){ 
  Hab.Pr <- exp(  -(x-y)^2/(2*Sigma^2)    )
  return(Hab.Pr)
}

# counting function: returns counts of similar habitat types within M x M Moore neighborhood 
Count.Func       <- function(M,O,S){
  
  O.min <- O - S*sqrt(2*pi)*.5
  O.max <- O + S*sqrt(2*pi)*.5
  
  IN    <- length(M[M>O.min & M<O.max]) 
  
  return(IN)
  
}

# HP Term (that is, R(sigma_h, N) ) from main text (Eq. 2)
R.Func           <- function(Sig,N){
  
  R <- (Sig*sqrt(2*pi))*N
  
  if(N >=   (1/(Sig*sqrt(2*pi))) )  return(1)
  
  return(R)
  
}


R.Func <- function(Sig,N){
  
  Coef     <- Sig*sqrt(2*pi)*N 
  
  Coef2    <- exp(-Coef)
  
  R <- 1 - Coef2
  
  return(R)
  
}

# Numerical calculations of approximate invasion criteria 
Inv.Exact        <- function(H_Opt_i,H_Opt,Sh,S,A,Y,Rad,M.H,M.S){
  
  P          <- c(table(factor(M.S, levels = 1:S)))/(dm*dm)   # P goes to proportion of each species in environment
  Subset     <- which(P>0)
  
  Y <- Y[Subset]
  P <- P[Subset]
  
  Cords      <- data.frame(which(is.na(M.S)==F, arr.ind=TRUE))
  
  x.val      <- Cords[,1]   # x coordinates of disturbance
  y.val      <- Cords[,2]   # y coordinates of distriubance
  
  Replaceb   <- nrow(Cords)   
  Replaceb   <- seq(1,Replaceb,by=5)
  
  Total <- sapply(Replaceb, function(x){  
    
    Habitat_Type  <- M.H[x.val[x],y.val[x]]  # Returns Habitat Type 
    
    JC.Victims   <- mat.torus(M.S,Rad,x.val[x],y.val[x])
    JC.Pred       <- c(table(factor(JC.Victims, levels = 1:S))) 
    Predation     <- exp(-A*JC.Pred)[Subset]
    H_Effect      <- sapply(Subset, function(z) Habitat_Survival(H_Opt[z],  Habitat_Type)  )
    
    Seeds         <-  Y*H_Effect*Predation*P
    Total.Seeds   <- as.numeric(sum(Seeds)) # total scaled number of seeds in local patch
    
    H_Effect_i      <-  Habitat_Survival(H_Opt_i,  Habitat_Type)  
    Seeds_i         <-  H_Effect_i
    
    
    return(Seeds_i/Total.Seeds) # return winner  
  })
  
  
  Ratio <-   (length(Replaceb)/sum(Total))
  
  return(Ratio)
}    

# the same as Inv.Exact, but ignores habitat partitioning  
Inv.Exact_JCE        <- function(H_Opt_i,H_Opt,Sh,S,A,Y,Rad,M.H,M.S){
  
  P          <- c(table(factor(M.S, levels = 1:S)))/(dm*dm)   # P goes to proportion of each species in environment
  Subset     <- which(P>0)
  
  Y <- Y[Subset]
  P <- P[Subset]
  
  Cords      <- data.frame(which(is.na(M.S)==F, arr.ind=TRUE))
  
  x.val      <- Cords[,1]   # x coordinates of disturbance
  y.val      <- Cords[,2]   # y coordinates of distriubance
  
  Replaceb   <- nrow(Cords)   
  Replaceb   <- seq(1,Replaceb,by=1)
  
  Total <- sapply(Replaceb, function(x){  
    
    Habitat_Type  <- M.H[x.val[x],y.val[x]]  # Returns Habitat Type 
    
    JC.Victims   <- mat.torus(M.S,Rad,x.val[x],y.val[x])
    JC.Pred       <- c(table(factor(JC.Victims, levels = 1:S))) 
    Predation     <- exp(-A*JC.Pred)[Subset]
    H_Effect      <- rep(1, length(Subset))
    
    Seeds         <-  Y*H_Effect*Predation*P
    Total.Seeds   <- as.numeric(sum(Seeds)) # total scaled number of seeds in local patch
    
    H_Effect_i      <- 1
    Seeds_i         <-  H_Effect_i
    
    
    return(Seeds_i/Total.Seeds) # return winner  
  })
  
  
  Ratio <-   (length(Replaceb)/sum(Total))
  
  return(Ratio)
}    

# Numerical calculations of exact invasion criteria 
Inv.Approx       <- function(Sh,A,Y,Rad,M.H,M.S){
  
  # approximate range 
  Sig   <- Sh*sqrt(2*pi)
  
  # Subset matrix of species with positive abundance... 
  M.S2       <- table(factor(as.matrix(M.S,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
  M.S2       <- as.vector(M.S2)
  Subset     <- which(M.S2>0)
  P          <- M.S2[Subset]
  N          <- length(P)
  Y.vals     <- Y[Subset]
  
  # Calculation of k_M
  LEN <- dm^2 
  K   <- (1/(Sig*Rad^2))*mean(sapply(1:LEN, function(x){          
            xi      <- x - floor( (x-1)/dm)*dm       # index for column of matrix 
            yi      <- ceiling(x/dm)                 # index for row of matrix 
            H.x.y   <- M.H[xi,yi]                    # habitat type of focal patch 
            M.Neigh <- mat.torus(M.H,Rad,xi,yi)        # M x M Moore neighborhood around patch  
            C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh)  # number of similar patches therein 
          }))  

  Rcoef <- R.Func(Sh,N)  
  JCEe <- exp(-(1-exp(-A))*((Rad^2)/N)*K*Rcoef)
  Phi <- (N/(Rcoef*mean(Y.vals)))*(1 - (1-exp(-A))*((Rad^2))*K*(Rcoef/N)  )
  
  
  InvC <- Rcoef*mean(Y.vals) * JCEe*( 1 + Phi*cov(P,Y.vals)  )
  return(InvC)
}

# the same as Inv.Approx, but ignores habitat partitioning 
Inv.Approx_JCE       <- function(Sh,A,Y,Rad,M.H,M.S){
  
  # approximate range 
  Sig   <- Sh*sqrt(2*pi)
  
  # Subset matrix of species with positive abundance... 
  M.S2       <- table(factor(as.matrix(M.S,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
  M.S2       <- as.vector(M.S2)
  Subset     <- which(M.S2>0)
  P          <- M.S2[Subset]
  N          <- length(P)
  Y.vals     <- Y[Subset]
  
  K <- 1
  
  Rcoef <- 1
  JCEe <- exp(-(1-exp(-A))*((Rad^2)/N)*K)
  Phi <- (N/(mean(Y.vals)))*(1 - (1-exp(-A))*((Rad^2)/N)*K)
  
  
  InvC <- Rcoef*mean(Y.vals) * JCEe*( 1 + Phi*cov(P,Y.vals)  )
  return(InvC)
}



###########################################################################################
##################################### Load the Data #######################################
###########################################################################################

########## REMINDER: Your working directory must be set to the "CSV_Files" folder ######

############################################################
################ Load different Habitat Types ############## 
############################################################
dm <- 499 # dimension of community (needed to remake habitat matrices)

# High Autocorrelation 
Hab.High <- read.csv("Hab_MedHigh.csv")
Hab.High <- unname(unlist(Hab.High))
Hab.High <- matrix(Hab.High,nrow=dm,ncol=dm)

# High autocorrelation 2 (used for HP only)
Hab.High2 <- read.csv("Hab_High.csv")
Hab.High2 <- unname(unlist(Hab.High2))
Hab.High2 <- matrix(Hab.High2,nrow=dm,ncol=dm)

# Medium Autocorrelation 
Hab.Med  <- read.csv("Hab_Med.csv")
Hab.Med  <- unname(unlist(Hab.Med))
Hab.Med  <- matrix(Hab.Med,nrow=dm,ncol=dm)

# Low Autocorrelation 
Hab.Low  <- read.csv("Hab_MedLow.csv")
Hab.Low  <- unname(unlist(Hab.Low))
Hab.Low  <- matrix(Hab.Low,nrow=dm,ncol=dm)

# Zero Autocorrelation (random) 
Hab.Rand <- read.csv("Hab_Rand.csv")
Hab.Rand <- unname(unlist(Hab.Rand))
Hab.Rand <- matrix(Hab.Rand,nrow=dm,ncol=dm)


setwd("C:/Users/smith/Outputs2") 

################################################################################################
####################### Load the Spatially Explicit Model Outputs ##############################
################################################################################################

# code will load the data and produce matrices that give the spatial location of each tree for each simulation 
# trees are indexed by number (1:300)

#JCE + HP high autocorrelation
E1_S025_HIGH <- read.csv("DIST_E1_MEDHIGH_BOTH_S025.csv")
E1_S025_HIGH <- matrix(unname(unlist(E1_S025_HIGH)),nrow=dm,ncol=dm)
E9_S025_HIGH <- read.csv("DIST_E9_MEDHIGH_BOTH_S025.csv")
E9_S025_HIGH <- matrix(unname(unlist(E9_S025_HIGH)),nrow=dm,ncol=dm)
E25_S025_HIGH <- read.csv("DIST_E25_MEDHIGH_BOTH_S025.csv")
E25_S025_HIGH <- matrix(unname(unlist(E25_S025_HIGH)),nrow=dm,ncol=dm)
E49_S025_HIGH <- read.csv("DIST_E49_MEDHIGH_BOTH_S025.csv")
E49_S025_HIGH <- matrix(unname(unlist(E49_S025_HIGH)),nrow=dm,ncol=dm)
E81_S025_HIGH <- read.csv("DIST_E81_MEDHIGH_BOTH_S025.csv")
E81_S025_HIGH <- matrix(unname(unlist(E81_S025_HIGH)),nrow=dm,ncol=dm)

E1_S05_HIGH <- read.csv("DIST_E1_MEDHIGH_BOTH_S05.csv")
E1_S05_HIGH <- matrix(unname(unlist(E1_S05_HIGH)),nrow=dm,ncol=dm)
E9_S05_HIGH <- read.csv("DIST_E9_MEDHIGH_BOTH_S05.csv")
E9_S05_HIGH <- matrix(unname(unlist(E9_S05_HIGH)),nrow=dm,ncol=dm)
E25_S05_HIGH <- read.csv("DIST_E25_MEDHIGH_BOTH_S05.csv")
E25_S05_HIGH <- matrix(unname(unlist(E25_S05_HIGH)),nrow=dm,ncol=dm)
E49_S05_HIGH <- read.csv("DIST_E49_MEDHIGH_BOTH_S05.csv")
E49_S05_HIGH <- matrix(unname(unlist(E49_S05_HIGH)),nrow=dm,ncol=dm)
E81_S05_HIGH <- read.csv("DIST_E81_MEDHIGH_BOTH_S05.csv")
E81_S05_HIGH <- matrix(unname(unlist(E81_S05_HIGH)),nrow=dm,ncol=dm)

E1_S075_HIGH <- read.csv("DIST_E1_MEDHIGH_BOTH_S075.csv")
E1_S075_HIGH <- matrix(unname(unlist(E1_S075_HIGH)),nrow=dm,ncol=dm)
E9_S075_HIGH <- read.csv("DIST_E9_MEDHIGH_BOTH_S075.csv")
E9_S075_HIGH <- matrix(unname(unlist(E9_S075_HIGH)),nrow=dm,ncol=dm)
E25_S075_HIGH <- read.csv("DIST_E25_MEDHIGH_BOTH_S075.csv")
E25_S075_HIGH <- matrix(unname(unlist(E25_S075_HIGH)),nrow=dm,ncol=dm)
E49_S075_HIGH <- read.csv("DIST_E49_MEDHIGH_BOTH_S075.csv")
E49_S075_HIGH <- matrix(unname(unlist(E49_S075_HIGH)),nrow=dm,ncol=dm)
E81_S075_HIGH <- read.csv("DIST_E81_MEDHIGH_BOTH_S075.csv")
E81_S075_HIGH <- matrix(unname(unlist(E81_S075_HIGH)),nrow=dm,ncol=dm)

E1_S1_HIGH <- read.csv("DIST_E1_MEDHIGH_BOTH_S1.csv")
E1_S1_HIGH <- matrix(unname(unlist(E1_S1_HIGH)),nrow=dm,ncol=dm)
E9_S1_HIGH <- read.csv("DIST_E9_MEDHIGH_BOTH_S1.csv")
E9_S1_HIGH <- matrix(unname(unlist(E9_S1_HIGH)),nrow=dm,ncol=dm)
E25_S1_HIGH <- read.csv("DIST_E25_MEDHIGH_BOTH_S1.csv")
E25_S1_HIGH <- matrix(unname(unlist(E25_S1_HIGH)),nrow=dm,ncol=dm)
E49_S1_HIGH <- read.csv("DIST_E49_MEDHIGH_BOTH_S1.csv")
E49_S1_HIGH <- matrix(unname(unlist(E49_S1_HIGH)),nrow=dm,ncol=dm)
E81_S1_HIGH <- read.csv("DIST_E81_MEDHIGH_BOTH_S1.csv")
E81_S1_HIGH <- matrix(unname(unlist(E81_S1_HIGH)),nrow=dm,ncol=dm)

#JCE + HP medium autocorrelation
E1_S025_MED <- read.csv("DIST_E1_MED_BOTH_S025.csv")
E1_S025_MED <- matrix(unname(unlist(E1_S025_MED)),nrow=dm,ncol=dm)
E9_S025_MED <- read.csv("DIST_E9_MED_BOTH_S025.csv")
E9_S025_MED <- matrix(unname(unlist(E9_S025_MED)),nrow=dm,ncol=dm)
E25_S025_MED <- read.csv("DIST_E25_MED_BOTH_S025.csv")
E25_S025_MED <- matrix(unname(unlist(E25_S025_MED)),nrow=dm,ncol=dm)
E49_S025_MED <- read.csv("DIST_E49_MED_BOTH_S025.csv")
E49_S025_MED <- matrix(unname(unlist(E49_S025_MED)),nrow=dm,ncol=dm)
E81_S025_MED <- read.csv("DIST_E81_MED_BOTH_S025.csv")
E81_S025_MED <- matrix(unname(unlist(E81_S025_MED)),nrow=dm,ncol=dm)

E1_S05_MED <- read.csv("DIST_E1_MED_BOTH_S05.csv")
E1_S05_MED <- matrix(unname(unlist(E1_S05_MED)),nrow=dm,ncol=dm)
E9_S05_MED <- read.csv("DIST_E9_MED_BOTH_S05.csv")
E9_S05_MED <- matrix(unname(unlist(E9_S05_MED)),nrow=dm,ncol=dm)
E25_S05_MED <- read.csv("DIST_E25_MED_BOTH_S05.csv")
E25_S05_MED <- matrix(unname(unlist(E25_S05_MED)),nrow=dm,ncol=dm)
E49_S05_MED <- read.csv("DIST_E49_MED_BOTH_S05.csv")
E49_S05_MED <- matrix(unname(unlist(E49_S05_MED)),nrow=dm,ncol=dm)
E81_S05_MED <- read.csv("DIST_E81_MED_BOTH_S05.csv")
E81_S05_MED <- matrix(unname(unlist(E81_S05_MED)),nrow=dm,ncol=dm)

E1_S075_MED <- read.csv("DIST_E1_MED_BOTH_S075.csv")
E1_S075_MED <- matrix(unname(unlist(E1_S075_MED)),nrow=dm,ncol=dm)
E9_S075_MED <- read.csv("DIST_E9_MED_BOTH_S075.csv")
E9_S075_MED <- matrix(unname(unlist(E9_S075_MED)),nrow=dm,ncol=dm)
E25_S075_MED <- read.csv("DIST_E25_MED_BOTH_S075.csv")
E25_S075_MED <- matrix(unname(unlist(E25_S075_MED)),nrow=dm,ncol=dm)
E49_S075_MED <- read.csv("DIST_E49_MED_BOTH_S075.csv")
E49_S075_MED <- matrix(unname(unlist(E49_S075_MED)),nrow=dm,ncol=dm)
E81_S075_MED <- read.csv("DIST_E81_MED_BOTH_S075.csv")
E81_S075_MED <- matrix(unname(unlist(E81_S075_MED)),nrow=dm,ncol=dm)

E1_S1_MED <- read.csv("DIST_E1_MED_BOTH_S1.csv")
E1_S1_MED <- matrix(unname(unlist(E1_S1_MED)),nrow=dm,ncol=dm)
E9_S1_MED <- read.csv("DIST_E9_MED_BOTH_S1.csv")
E9_S1_MED <- matrix(unname(unlist(E9_S1_MED)),nrow=dm,ncol=dm)
E25_S1_MED <- read.csv("DIST_E25_MED_BOTH_S1.csv")
E25_S1_MED <- matrix(unname(unlist(E25_S1_MED)),nrow=dm,ncol=dm)
E49_S1_MED <- read.csv("DIST_E49_MED_BOTH_S1.csv")
E49_S1_MED <- matrix(unname(unlist(E49_S1_MED)),nrow=dm,ncol=dm)
E81_S1_MED <- read.csv("DIST_E81_MED_BOTH_S1.csv")
E81_S1_MED <- matrix(unname(unlist(E81_S1_MED)),nrow=dm,ncol=dm)

#JCE + HP low autocorrelation
E1_S025_LOW <- read.csv("DIST_E1_MEDLOW_BOTH_S025.csv")
E1_S025_LOW <- matrix(unname(unlist(E1_S025_LOW)),nrow=dm,ncol=dm)
E9_S025_LOW <- read.csv("DIST_E9_MEDLOW_BOTH_S025.csv")
E9_S025_LOW <- matrix(unname(unlist(E9_S025_LOW)),nrow=dm,ncol=dm)
E25_S025_LOW <- read.csv("DIST_E25_MEDLOW_BOTH_S025.csv")
E25_S025_LOW <- matrix(unname(unlist(E25_S025_LOW)),nrow=dm,ncol=dm)
E49_S025_LOW <- read.csv("DIST_E49_MEDLOW_BOTH_S025.csv")
E49_S025_LOW <- matrix(unname(unlist(E49_S025_LOW)),nrow=dm,ncol=dm)
E81_S025_LOW <- read.csv("DIST_E81_MEDLOW_BOTH_S025.csv")
E81_S025_LOW <- matrix(unname(unlist(E81_S025_LOW)),nrow=dm,ncol=dm)

E1_S05_LOW <- read.csv("DIST_E1_MEDLOW_BOTH_S05.csv")
E1_S05_LOW <- matrix(unname(unlist(E1_S05_LOW)),nrow=dm,ncol=dm)
E9_S05_LOW <- read.csv("DIST_E9_MEDLOW_BOTH_S05.csv")
E9_S05_LOW <- matrix(unname(unlist(E9_S05_LOW)),nrow=dm,ncol=dm)
E25_S05_LOW <- read.csv("DIST_E25_MEDLOW_BOTH_S05.csv")
E25_S05_LOW <- matrix(unname(unlist(E25_S05_LOW)),nrow=dm,ncol=dm)
E49_S05_LOW <- read.csv("DIST_E49_MEDLOW_BOTH_S05.csv")
E49_S05_LOW <- matrix(unname(unlist(E49_S05_LOW)),nrow=dm,ncol=dm)
E81_S05_LOW <- read.csv("DIST_E81_MEDLOW_BOTH_S05.csv")
E81_S05_LOW <- matrix(unname(unlist(E81_S05_LOW)),nrow=dm,ncol=dm)

E1_S075_LOW <- read.csv("DIST_E1_MEDLOW_BOTH_S075.csv")
E1_S075_LOW <- matrix(unname(unlist(E1_S075_LOW)),nrow=dm,ncol=dm)
E9_S075_LOW <- read.csv("DIST_E9_MEDLOW_BOTH_S075.csv")
E9_S075_LOW <- matrix(unname(unlist(E9_S075_LOW)),nrow=dm,ncol=dm)
E25_S075_LOW <- read.csv("DIST_E25_MEDLOW_BOTH_S075.csv")
E25_S075_LOW <- matrix(unname(unlist(E25_S075_LOW)),nrow=dm,ncol=dm)
E49_S075_LOW <- read.csv("DIST_E49_MEDLOW_BOTH_S075.csv")
E49_S075_LOW <- matrix(unname(unlist(E49_S075_LOW)),nrow=dm,ncol=dm)
E81_S075_LOW <- read.csv("DIST_E81_MEDLOW_BOTH_S075.csv")
E81_S075_LOW <- matrix(unname(unlist(E81_S075_LOW)),nrow=dm,ncol=dm)

E1_S1_LOW <- read.csv("DIST_E1_MEDLOW_BOTH_S1.csv")
E1_S1_LOW <- matrix(unname(unlist(E1_S1_LOW)),nrow=dm,ncol=dm)
E9_S1_LOW <- read.csv("DIST_E9_MEDLOW_BOTH_S1.csv")
E9_S1_LOW <- matrix(unname(unlist(E9_S1_LOW)),nrow=dm,ncol=dm)
E25_S1_LOW <- read.csv("DIST_E25_MEDLOW_BOTH_S1.csv")
E25_S1_LOW <- matrix(unname(unlist(E25_S1_LOW)),nrow=dm,ncol=dm)
E49_S1_LOW <- read.csv("DIST_E49_MEDLOW_BOTH_S1.csv")
E49_S1_LOW <- matrix(unname(unlist(E49_S1_LOW)),nrow=dm,ncol=dm)
E81_S1_LOW <- read.csv("DIST_E81_MEDLOW_BOTH_S1.csv")
E81_S1_LOW <- matrix(unname(unlist(E81_S1_LOW)),nrow=dm,ncol=dm)

#JCE + HP zero autocorrelation
E1_S025_RAND <- read.csv("DIST_E1_RAND_BOTH_S025.csv")
E1_S025_RAND <- matrix(unname(unlist(E1_S025_RAND)),nrow=dm,ncol=dm)
E9_S025_RAND <- read.csv("DIST_E9_RAND_BOTH_S025.csv")
E9_S025_RAND <- matrix(unname(unlist(E9_S025_RAND)),nrow=dm,ncol=dm)
E25_S025_RAND <- read.csv("DIST_E25_RAND_BOTH_S025.csv")
E25_S025_RAND <- matrix(unname(unlist(E25_S025_RAND)),nrow=dm,ncol=dm)
E49_S025_RAND <- read.csv("DIST_E49_RAND_BOTH_S025.csv")
E49_S025_RAND <- matrix(unname(unlist(E49_S025_RAND)),nrow=dm,ncol=dm)
E81_S025_RAND <- read.csv("DIST_E81_RAND_BOTH_S025.csv")
E81_S025_RAND <- matrix(unname(unlist(E81_S025_RAND)),nrow=dm,ncol=dm)

E1_S05_RAND <- read.csv("DIST_E1_RAND_BOTH_S05.csv")
E1_S05_RAND <- matrix(unname(unlist(E1_S05_RAND)),nrow=dm,ncol=dm)
E9_S05_RAND <- read.csv("DIST_E9_RAND_BOTH_S05.csv")
E9_S05_RAND <- matrix(unname(unlist(E9_S05_RAND)),nrow=dm,ncol=dm)
E25_S05_RAND <- read.csv("DIST_E25_RAND_BOTH_S05.csv")
E25_S05_RAND <- matrix(unname(unlist(E25_S05_RAND)),nrow=dm,ncol=dm)
E49_S05_RAND <- read.csv("DIST_E49_RAND_BOTH_S05.csv")
E49_S05_RAND <- matrix(unname(unlist(E49_S05_RAND)),nrow=dm,ncol=dm)
E81_S05_RAND <- read.csv("DIST_E81_RAND_BOTH_S05.csv")
E81_S05_RAND <- matrix(unname(unlist(E81_S05_RAND)),nrow=dm,ncol=dm)

E1_S075_RAND <- read.csv("DIST_E1_RAND_BOTH_S075.csv")
E1_S075_RAND <- matrix(unname(unlist(E1_S075_RAND)),nrow=dm,ncol=dm)
E9_S075_RAND <- read.csv("DIST_E9_RAND_BOTH_S075.csv")
E9_S075_RAND <- matrix(unname(unlist(E9_S075_RAND)),nrow=dm,ncol=dm)
E25_S075_RAND <- read.csv("DIST_E25_RAND_BOTH_S075.csv")
E25_S075_RAND <- matrix(unname(unlist(E25_S075_RAND)),nrow=dm,ncol=dm)
E49_S075_RAND <- read.csv("DIST_E49_RAND_BOTH_S075.csv")
E49_S075_RAND <- matrix(unname(unlist(E49_S075_RAND)),nrow=dm,ncol=dm)
E81_S075_RAND <- read.csv("DIST_E81_RAND_BOTH_S075.csv")
E81_S075_RAND <- matrix(unname(unlist(E81_S075_RAND)),nrow=dm,ncol=dm)

E1_S1_RAND <- read.csv("DIST_E1_RAND_BOTH_S1.csv")
E1_S1_RAND <- matrix(unname(unlist(E1_S1_RAND)),nrow=dm,ncol=dm)
E9_S1_RAND <- read.csv("DIST_E9_RAND_BOTH_S1.csv")
E9_S1_RAND <- matrix(unname(unlist(E9_S1_RAND)),nrow=dm,ncol=dm)
E25_S1_RAND <- read.csv("DIST_E25_RAND_BOTH_S1.csv")
E25_S1_RAND <- matrix(unname(unlist(E25_S1_RAND)),nrow=dm,ncol=dm)
E49_S1_RAND <- read.csv("DIST_E49_RAND_BOTH_S1.csv")
E49_S1_RAND <- matrix(unname(unlist(E49_S1_RAND)),nrow=dm,ncol=dm)
E81_S1_RAND <- read.csv("DIST_E81_RAND_BOTH_S1.csv")
E81_S1_RAND <- matrix(unname(unlist(E81_S1_RAND)),nrow=dm,ncol=dm)

# JCE only outputs
E1_JCE <- read.csv("DIST_E1_JCE.csv")
E1_JCE <- matrix(unname(unlist(E1_JCE)),nrow=dm,ncol=dm)
E9_JCE <- read.csv("DIST_E9_JCE.csv")
E9_JCE <- matrix(unname(unlist(E9_JCE)),nrow=dm,ncol=dm)
E25_JCE <- read.csv("DIST_E25_JCE.csv")
E25_JCE <- matrix(unname(unlist(E25_JCE)),nrow=dm,ncol=dm)
E49_JCE <- read.csv("DIST_E49_JCE.csv")
E49_JCE <- matrix(unname(unlist(E49_JCE)),nrow=dm,ncol=dm)
E81_JCE <- read.csv("DIST_E81_JCE.csv")
E81_JCE <- matrix(unname(unlist(E81_JCE)),nrow=dm,ncol=dm)

# HP only outputs 
S025 <- read.csv("DIST_HAB_S025.csv")
S025 <- matrix(unname(unlist(S025)),nrow=dm,ncol=dm)
S05 <- read.csv("DIST_HAB_S05.csv")
S05 <- matrix(unname(unlist(S05)),nrow=dm,ncol=dm)
S075 <- read.csv("DIST_HAB_S075.csv")
S075 <- matrix(unname(unlist(S075)),nrow=dm,ncol=dm)
S1 <- read.csv("DIST_HAB_S1.csv")
S1 <- matrix(unname(unlist(S025)),nrow=dm,ncol=dm)


##############################################################
################# Make the Approximations ####################
##############################################################

### parameters for all calculations ###
LEN <- 25
OPTS <- seq(0,1,length=LEN)
A <-.5
S <- 300
set.seed(100)
Y <- rlnorm(S,meanlog=0,sd=1)

##########################################################################
############### Run the Approximations and exact values ##################
##########################################################################

##################################
##### High autocorrelation #######
##################################

# sigma_h = 0.025
Sigma <- .025

Rad   <- 1
Approx_High_S025_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E1_S025_HIGH),LEN)
Exact_High_S025_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E1_S025_HIGH))
Exact_High_S025_M1.mean <- mean(Exact_High_S025_M1)

Rad   <- 3
Approx_High_S025_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E9_S025_HIGH),LEN)
Exact_High_S025_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E9_S025_HIGH))
Exact_High_S025_M3.mean <- mean(Exact_High_S025_M3)

Rad   <- 5
Approx_High_S025_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E25_S025_HIGH),LEN)
Exact_High_S025_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E25_S025_HIGH))
Exact_High_S025_M5.mean <- mean(Exact_High_S025_M5)

Rad   <- 7
Approx_High_S025_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E49_S025_HIGH),LEN)
Exact_High_S025_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E49_S025_HIGH))
Exact_High_S025_M7.mean <- mean(Exact_High_S025_M7)

Rad   <- 9
Approx_High_S025_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E81_S025_HIGH),LEN)
Exact_High_S025_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E81_S025_HIGH))
Exact_High_S025_M9.mean <- mean(Exact_High_S025_M9)

# sigma_h = 0.05
Sigma <- .05

Rad   <- 1
Approx_High_S05_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E1_S05_HIGH),LEN)
Exact_High_S05_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E1_S05_HIGH))
Exact_High_S05_M1.mean <- mean(Exact_High_S05_M1)

Rad   <- 3
Approx_High_S05_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E9_S05_HIGH),LEN)
Exact_High_S05_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E9_S05_HIGH))
Exact_High_S05_M3.mean <- mean(Exact_High_S05_M3)

Rad   <- 5
Approx_High_S05_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E25_S05_HIGH),LEN)
Exact_High_S05_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E25_S05_HIGH))
Exact_High_S05_M5.mean <- mean(Exact_High_S05_M5)

Rad   <- 7
Approx_High_S05_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E49_S05_HIGH),LEN)
Exact_High_S05_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E49_S05_HIGH))
Exact_High_S05_M7.mean <- mean(Exact_High_S05_M7)

Rad   <- 9
Approx_High_S05_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E81_S05_HIGH),LEN)
Exact_High_S05_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E81_S05_HIGH))
Exact_High_S05_M9.mean <- mean(Exact_High_S05_M9)

# sigma_h = 0.075
Sigma <- .075

Rad   <- 1
Approx_High_S075_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E1_S075_HIGH),LEN)
Exact_High_S075_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E1_S075_HIGH))
Exact_High_S075_M1.mean <- mean(Exact_High_S075_M1)

Rad   <- 3
Approx_High_S075_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E9_S075_HIGH),LEN)
Exact_High_S075_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E9_S075_HIGH))
Exact_High_S075_M3.mean <- mean(Exact_High_S075_M3)

Rad   <- 5
Approx_High_S075_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E25_S075_HIGH),LEN)
Exact_High_S075_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E25_S075_HIGH))
Exact_High_S075_M5.mean <- mean(Exact_High_S075_M5)

Rad   <- 7
Approx_High_S075_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E49_S075_HIGH),LEN)
Exact_High_S075_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E49_S075_HIGH))
Exact_High_S075_M7.mean <- mean(Exact_High_S075_M7)

Rad   <- 9
Approx_High_S075_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E81_S075_HIGH),LEN)
Exact_High_S075_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E81_S075_HIGH))
Exact_High_S075_M9.mean <- mean(Exact_High_S075_M9)


# sigma_h = 0.1
Sigma <- .1

Rad   <- 1
Approx_High_S1_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E1_S1_HIGH),LEN)
Exact_High_S1_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E1_S1_HIGH))
Exact_High_S1_M1.mean <- mean(Exact_High_S1_M1)

Rad   <- 3
Approx_High_S1_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E9_S1_HIGH),LEN)
Exact_High_S1_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E9_S1_HIGH))
Exact_High_S1_M3.mean <- mean(Exact_High_S1_M3)

Rad   <- 5
Approx_High_S1_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E25_S1_HIGH),LEN)
Exact_High_S1_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E25_S1_HIGH))
Exact_High_S1_M5.mean <- mean(Exact_High_S1_M5)

Rad   <- 7
Approx_High_S1_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E49_S1_HIGH),LEN)
Exact_High_S1_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E49_S1_HIGH))
Exact_High_S1_M7.mean <- mean(Exact_High_S1_M7)

Rad   <- 9
Approx_High_S1_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.High,E81_S1_HIGH),LEN)
Exact_High_S1_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.High,E81_S1_HIGH))
Exact_High_S1_M9.mean <- mean(Exact_High_S1_M9)

# High together 
Approx_High_S025 <- c(Approx_High_S025_M1,Approx_High_S025_M3,Approx_High_S025_M5,Approx_High_S025_M7,Approx_High_S025_M9)
Approx_High_S05  <- c(Approx_High_S05_M1,Approx_High_S05_M3,Approx_High_S05_M5,Approx_High_S05_M7,Approx_High_S05_M9)
Approx_High_S075 <- c(Approx_High_S075_M1,Approx_High_S075_M3,Approx_High_S075_M5,Approx_High_S075_M7,Approx_High_S075_M9)
Approx_High_S1   <- c(Approx_High_S1_M1,Approx_High_S1_M3,Approx_High_S1_M5,Approx_High_S1_M7,Approx_High_S1_M9)

Approx_High_S025.mean <- c(Approx_High_S025_M1[1],Approx_High_S025_M3[1],Approx_High_S025_M5[1],Approx_High_S025_M7[1],Approx_High_S025_M9[1])
Approx_High_S05.mean  <- c(Approx_High_S05_M1[1],Approx_High_S05_M3[1],Approx_High_S05_M5[1],Approx_High_S05_M7[1],Approx_High_S05_M9[1])
Approx_High_S075.mean <- c(Approx_High_S075_M1[1],Approx_High_S075_M3[1],Approx_High_S075_M5[1],Approx_High_S075_M7[1],Approx_High_S075_M9[1])
Approx_High_S1.mean   <- c(Approx_High_S1_M1[1],Approx_High_S1_M3[1],Approx_High_S1_M5[1],Approx_High_S1_M7[1],Approx_High_S1_M9[1])

Approx_High           <- c(Approx_High_S025,Approx_High_S05,Approx_High_S075,Approx_High_S1)
Approx_High.mean      <- c(Approx_High_S025.mean,Approx_High_S05.mean,Approx_High_S075.mean,Approx_High_S1.mean)


Exact_High_S025 <- c(Exact_High_S025_M1,Exact_High_S025_M3,Exact_High_S025_M5,Exact_High_S025_M7,Exact_High_S025_M9)
Exact_High_S05  <- c(Exact_High_S05_M1,Exact_High_S05_M3,Exact_High_S05_M5,Exact_High_S05_M7,Exact_High_S05_M9)
Exact_High_S075 <- c(Exact_High_S075_M1,Exact_High_S075_M3,Exact_High_S075_M5,Exact_High_S075_M7,Exact_High_S075_M9)
Exact_High_S1   <- c(Exact_High_S1_M1,Exact_High_S1_M3,Exact_High_S1_M5,Exact_High_S1_M7,Exact_High_S1_M9)

Exact_High_S025.mean <- c(Exact_High_S025_M1.mean,Exact_High_S025_M3.mean,Exact_High_S025_M5.mean,Exact_High_S025_M7.mean,Exact_High_S025_M9.mean)
Exact_High_S05.mean  <- c(Exact_High_S05_M1.mean,Exact_High_S05_M3.mean,Exact_High_S05_M5.mean,Exact_High_S05_M7.mean,Exact_High_S05_M9.mean)
Exact_High_S075.mean <- c(Exact_High_S075_M1.mean,Exact_High_S075_M3.mean,Exact_High_S075_M5.mean,Exact_High_S075_M7.mean,Exact_High_S075_M9.mean)
Exact_High_S1.mean   <- c(Exact_High_S1_M1.mean,Exact_High_S1_M3.mean,Exact_High_S1_M5.mean,Exact_High_S1_M7.mean,Exact_High_S1_M9.mean)


Exact_High           <- c(Exact_High_S025,Exact_High_S05,Exact_High_S075,Exact_High_S1)
Exact_High.mean      <- c(Exact_High_S025.mean,Exact_High_S05.mean,Exact_High_S075.mean,Exact_High_S1.mean)


plot(Exact_High.mean~Approx_High.mean)
abline(coef = c(0,1),col="black",lty=2,lwd=3)


#################################
##### Med autocorrelation #######
#################################

# sigma_h = 0.025
Sigma <- .025

Rad   <- 1
Approx_Med_S025_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E1_S025_MED),LEN)
Exact_Med_S025_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E1_S025_MED))
Exact_Med_S025_M1.mean <- mean(Exact_Med_S025_M1)

Rad   <- 3
Approx_Med_S025_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E9_S025_MED),LEN)
Exact_Med_S025_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E9_S025_MED))
Exact_Med_S025_M3.mean <- mean(Exact_Med_S025_M3)

Rad   <- 5
Approx_Med_S025_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E25_S025_MED),LEN)
Exact_Med_S025_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E25_S025_MED))
Exact_Med_S025_M5.mean <- mean(Exact_Med_S025_M5)

Rad   <- 7
Approx_Med_S025_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E49_S025_MED),LEN)
Exact_Med_S025_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E49_S025_MED))
Exact_Med_S025_M7.mean <- mean(Exact_Med_S025_M7)

Rad   <- 9
Approx_Med_S025_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E81_S025_MED),LEN)
Exact_Med_S025_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E81_S025_MED))
Exact_Med_S025_M9.mean <- mean(Exact_Med_S025_M9)


# sigma_h = 0.05
Sigma <- .05

Rad   <- 1
Approx_Med_S05_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E1_S05_MED),LEN)
Exact_Med_S05_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E1_S05_MED))
Exact_Med_S05_M1.mean <- mean(Exact_Med_S05_M1)

Rad   <- 3
Approx_Med_S05_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E9_S05_MED),LEN)
Exact_Med_S05_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E9_S05_MED))
Exact_Med_S05_M3.mean <- mean(Exact_Med_S05_M3)

Rad   <- 5
Approx_Med_S05_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E25_S05_MED),LEN)
Exact_Med_S05_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E25_S05_MED))
Exact_Med_S05_M5.mean <- mean(Exact_Med_S05_M5)

Rad   <- 7
Approx_Med_S05_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E49_S05_MED),LEN)
Exact_Med_S05_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E49_S05_MED))
Exact_Med_S05_M7.mean <- mean(Exact_Med_S05_M7)

Rad   <- 9
Approx_Med_S05_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E81_S05_MED),LEN)
Exact_Med_S05_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E81_S05_MED))
Exact_Med_S05_M9.mean <- mean(Exact_Med_S05_M9)

# sigma_h = 0.075
Sigma <- .075

Rad   <- 1
Approx_Med_S075_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E1_S075_MED),LEN)
Exact_Med_S075_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E1_S075_MED))
Exact_Med_S075_M1.mean <- mean(Exact_Med_S075_M1)

Rad   <- 3
Approx_Med_S075_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E9_S075_MED),LEN)
Exact_Med_S075_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E9_S075_MED))
Exact_Med_S075_M3.mean <- mean(Exact_Med_S075_M3)

Rad   <- 5
Approx_Med_S075_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E25_S075_MED),LEN)
Exact_Med_S075_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E25_S075_MED))
Exact_Med_S075_M5.mean <- mean(Exact_Med_S075_M5)

Rad   <- 7
Approx_Med_S075_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E49_S075_MED),LEN)
Exact_Med_S075_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E49_S075_MED))
Exact_Med_S075_M7.mean <- mean(Exact_Med_S075_M7)

Rad   <- 9
Approx_Med_S075_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E81_S075_MED),LEN)
Exact_Med_S075_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E81_S075_MED))
Exact_Med_S075_M9.mean <- mean(Exact_Med_S075_M9)


# sigma_h = 0.1
Sigma <- .1

Rad   <- 1
Approx_Med_S1_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E1_S1_MED),LEN)
Exact_Med_S1_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E1_S1_MED))
Exact_Med_S1_M1.mean <- mean(Exact_Med_S1_M1)

Rad   <- 3
Approx_Med_S1_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E9_S1_MED),LEN)
Exact_Med_S1_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E9_S1_MED))
Exact_Med_S1_M3.mean <- mean(Exact_Med_S1_M3)

Rad   <- 5
Approx_Med_S1_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E25_S1_MED),LEN)
Exact_Med_S1_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E25_S1_MED))
Exact_Med_S1_M5.mean <- mean(Exact_Med_S1_M5)

Rad   <- 7
Approx_Med_S1_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E49_S1_MED),LEN)
Exact_Med_S1_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E49_S1_MED))
Exact_Med_S1_M7.mean <- mean(Exact_Med_S1_M7)

Rad   <- 9
Approx_Med_S1_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Med,E81_S1_MED),LEN)
Exact_Med_S1_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Med,E81_S1_MED))
Exact_Med_S1_M9.mean <- mean(Exact_Med_S1_M9)

# Med together 
Approx_Med_S025 <- c(Approx_Med_S025_M1,Approx_Med_S025_M3,Approx_Med_S025_M5,Approx_Med_S025_M7,Approx_Med_S025_M9)
Approx_Med_S05  <- c(Approx_Med_S05_M1,Approx_Med_S05_M3,Approx_Med_S05_M5,Approx_Med_S05_M7,Approx_Med_S05_M9)
Approx_Med_S075 <- c(Approx_Med_S075_M1,Approx_Med_S075_M3,Approx_Med_S075_M5,Approx_Med_S075_M7,Approx_Med_S075_M9)
Approx_Med_S1   <- c(Approx_Med_S1_M1,Approx_Med_S1_M3,Approx_Med_S1_M5,Approx_Med_S1_M7,Approx_Med_S1_M9)

Approx_Med_S025.mean <- c(Approx_Med_S025_M1[1],Approx_Med_S025_M3[1],Approx_Med_S025_M5[1],Approx_Med_S025_M7[1],Approx_Med_S025_M9[1])
Approx_Med_S05.mean  <- c(Approx_Med_S05_M1[1],Approx_Med_S05_M3[1],Approx_Med_S05_M5[1],Approx_Med_S05_M7[1],Approx_Med_S05_M9[1])
Approx_Med_S075.mean <- c(Approx_Med_S075_M1[1],Approx_Med_S075_M3[1],Approx_Med_S075_M5[1],Approx_Med_S075_M7[1],Approx_Med_S075_M9[1])
Approx_Med_S1.mean   <- c(Approx_Med_S1_M1[1],Approx_Med_S1_M3[1],Approx_Med_S1_M5[1],Approx_Med_S1_M7[1],Approx_Med_S1_M9[1])

Approx_Med           <- c(Approx_Med_S025,Approx_Med_S05,Approx_Med_S075,Approx_Med_S1)
Approx_Med.mean      <- c(Approx_Med_S025.mean,Approx_Med_S05.mean,Approx_Med_S075.mean,Approx_Med_S1.mean)


Exact_Med_S025 <- c(Exact_Med_S025_M1,Exact_Med_S025_M3,Exact_Med_S025_M5,Exact_Med_S025_M7,Exact_Med_S025_M9)
Exact_Med_S05  <- c(Exact_Med_S05_M1,Exact_Med_S05_M3,Exact_Med_S05_M5,Exact_Med_S05_M7,Exact_Med_S05_M9)
Exact_Med_S075 <- c(Exact_Med_S075_M1,Exact_Med_S075_M3,Exact_Med_S075_M5,Exact_Med_S075_M7,Exact_Med_S075_M9)
Exact_Med_S1   <- c(Exact_Med_S1_M1,Exact_Med_S1_M3,Exact_Med_S1_M5,Exact_Med_S1_M7,Exact_Med_S1_M9)

Exact_Med_S025.mean <- c(Exact_Med_S025_M1.mean,Exact_Med_S025_M3.mean,Exact_Med_S025_M5.mean,Exact_Med_S025_M7.mean,Exact_Med_S025_M9.mean)
Exact_Med_S05.mean  <- c(Exact_Med_S05_M1.mean,Exact_Med_S05_M3.mean,Exact_Med_S05_M5.mean,Exact_Med_S05_M7.mean,Exact_Med_S05_M9.mean)
Exact_Med_S075.mean <- c(Exact_Med_S075_M1.mean,Exact_Med_S075_M3.mean,Exact_Med_S075_M5.mean,Exact_Med_S075_M7.mean,Exact_Med_S075_M9.mean)
Exact_Med_S1.mean   <- c(Exact_Med_S1_M1.mean,Exact_Med_S1_M3.mean,Exact_Med_S1_M5.mean,Exact_Med_S1_M7.mean,Exact_Med_S1_M9.mean)


Exact_Med           <- c(Exact_Med_S025,Exact_Med_S05,Exact_Med_S075,Exact_Med_S1)
Exact_Med.mean      <- c(Exact_Med_S025.mean,Exact_Med_S05.mean,Exact_Med_S075.mean,Exact_Med_S1.mean)


plot(Exact_Med.mean~Approx_Med.mean)
abline(coef = c(0,1),col="black",lty=2,lwd=3)


#################################
##### Low autocorrelation #######
#################################

# sigma_h = 0.025
Sigma <- .025

Rad   <- 1
Approx_Low_S025_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E1_S025_LOW),LEN)
Exact_Low_S025_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E1_S025_LOW))
Exact_Low_S025_M1.mean <- mean(Exact_Low_S025_M1)

Rad   <- 3
Approx_Low_S025_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E9_S025_LOW),LEN)
Exact_Low_S025_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E9_S025_LOW))
Exact_Low_S025_M3.mean <- mean(Exact_Low_S025_M3)

Rad   <- 5
Approx_Low_S025_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E25_S025_LOW),LEN)
Exact_Low_S025_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E25_S025_LOW))
Exact_Low_S025_M5.mean <- mean(Exact_Low_S025_M5)

Rad   <- 7
Approx_Low_S025_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E49_S025_LOW),LEN)
Exact_Low_S025_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E49_S025_LOW))
Exact_Low_S025_M7.mean <- mean(Exact_Low_S025_M7)

Rad   <- 9
Approx_Low_S025_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E81_S025_LOW),LEN)
Exact_Low_S025_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E81_S025_LOW))
Exact_Low_S025_M9.mean <- mean(Exact_Low_S025_M9)

# sigma_h = 0.05
Sigma <- .05

Rad   <- 1
Approx_Low_S05_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E1_S05_LOW),LEN)
Exact_Low_S05_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E1_S05_LOW))
Exact_Low_S05_M1.mean <- mean(Exact_Low_S05_M1)

Rad   <- 3
Approx_Low_S05_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E9_S05_LOW),LEN)
Exact_Low_S05_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E9_S05_LOW))
Exact_Low_S05_M3.mean <- mean(Exact_Low_S05_M3)

Rad   <- 5
Approx_Low_S05_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E25_S05_LOW),LEN)
Exact_Low_S05_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E25_S05_LOW))
Exact_Low_S05_M5.mean <- mean(Exact_Low_S05_M5)

Rad   <- 7
Approx_Low_S05_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E49_S05_LOW),LEN)
Exact_Low_S05_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E49_S05_LOW))
Exact_Low_S05_M7.mean <- mean(Exact_Low_S05_M7)

Rad   <- 9
Approx_Low_S05_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E81_S05_LOW),LEN)
Exact_Low_S05_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E81_S05_LOW))
Exact_Low_S05_M9.mean <- mean(Exact_Low_S05_M9)

# sigma_h = 0.075
Sigma <- .075

Rad   <- 1
Approx_Low_S075_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E1_S075_LOW),LEN)
Exact_Low_S075_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E1_S075_LOW))
Exact_Low_S075_M1.mean <- mean(Exact_Low_S075_M1)

Rad   <- 3
Approx_Low_S075_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E9_S075_LOW),LEN)
Exact_Low_S075_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E9_S075_LOW))
Exact_Low_S075_M3.mean <- mean(Exact_Low_S075_M3)

Rad   <- 5
Approx_Low_S075_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E25_S075_LOW),LEN)
Exact_Low_S075_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E25_S075_LOW))
Exact_Low_S075_M5.mean <- mean(Exact_Low_S075_M5)

Rad   <- 7
Approx_Low_S075_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E49_S075_LOW),LEN)
Exact_Low_S075_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E49_S075_LOW))
Exact_Low_S075_M7.mean <- mean(Exact_Low_S075_M7)

Rad   <- 9
Approx_Low_S075_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E81_S075_LOW),LEN)
Exact_Low_S075_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E81_S075_LOW))
Exact_Low_S075_M9.mean <- mean(Exact_Low_S075_M9)


# sigma_h = 0.1
Sigma <- .1

Rad   <- 1
Approx_Low_S1_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E1_S1_LOW),LEN)
Exact_Low_S1_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E1_S1_LOW))
Exact_Low_S1_M1.mean <- mean(Exact_Low_S1_M1)

Rad   <- 3
Approx_Low_S1_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E9_S1_LOW),LEN)
Exact_Low_S1_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E9_S1_LOW))
Exact_Low_S1_M3.mean <- mean(Exact_Low_S1_M3)

Rad   <- 5
Approx_Low_S1_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E25_S1_LOW),LEN)
Exact_Low_S1_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E25_S1_LOW))
Exact_Low_S1_M5.mean <- mean(Exact_Low_S1_M5)

Rad   <- 7
Approx_Low_S1_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E49_S1_LOW),LEN)
Exact_Low_S1_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E49_S1_LOW))
Exact_Low_S1_M7.mean <- mean(Exact_Low_S1_M7)

Rad   <- 9
Approx_Low_S1_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Low,E81_S1_LOW),LEN)
Exact_Low_S1_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E81_S1_LOW))
Exact_Low_S1_M9.mean <- mean(Exact_Low_S1_M9)

# Low together 
Approx_Low_S025 <- c(Approx_Low_S025_M1,Approx_Low_S025_M3,Approx_Low_S025_M5,Approx_Low_S025_M7,Approx_Low_S025_M9)
Approx_Low_S05  <- c(Approx_Low_S05_M1,Approx_Low_S05_M3,Approx_Low_S05_M5,Approx_Low_S05_M7,Approx_Low_S05_M9)
Approx_Low_S075 <- c(Approx_Low_S075_M1,Approx_Low_S075_M3,Approx_Low_S075_M5,Approx_Low_S075_M7,Approx_Low_S075_M9)
Approx_Low_S1   <- c(Approx_Low_S1_M1,Approx_Low_S1_M3,Approx_Low_S1_M5,Approx_Low_S1_M7,Approx_Low_S1_M9)

Approx_Low_S025.mean <- c(Approx_Low_S025_M1[1],Approx_Low_S025_M3[1],Approx_Low_S025_M5[1],Approx_Low_S025_M7[1],Approx_Low_S025_M9[1])
Approx_Low_S05.mean  <- c(Approx_Low_S05_M1[1],Approx_Low_S05_M3[1],Approx_Low_S05_M5[1],Approx_Low_S05_M7[1],Approx_Low_S05_M9[1])
Approx_Low_S075.mean <- c(Approx_Low_S075_M1[1],Approx_Low_S075_M3[1],Approx_Low_S075_M5[1],Approx_Low_S075_M7[1],Approx_Low_S075_M9[1])
Approx_Low_S1.mean   <- c(Approx_Low_S1_M1[1],Approx_Low_S1_M3[1],Approx_Low_S1_M5[1],Approx_Low_S1_M7[1],Approx_Low_S1_M9[1])

Approx_Low           <- c(Approx_Low_S025,Approx_Low_S05,Approx_Low_S075,Approx_Low_S1)
Approx_Low.mean      <- c(Approx_Low_S025.mean,Approx_Low_S05.mean,Approx_Low_S075.mean,Approx_Low_S1.mean)


Exact_Low_S025 <- c(Exact_Low_S025_M1,Exact_Low_S025_M3,Exact_Low_S025_M5,Exact_Low_S025_M7,Exact_Low_S025_M9)
Exact_Low_S05  <- c(Exact_Low_S05_M1,Exact_Low_S05_M3,Exact_Low_S05_M5,Exact_Low_S05_M7,Exact_Low_S05_M9)
Exact_Low_S075 <- c(Exact_Low_S075_M1,Exact_Low_S075_M3,Exact_Low_S075_M5,Exact_Low_S075_M7,Exact_Low_S075_M9)
Exact_Low_S1   <- c(Exact_Low_S1_M1,Exact_Low_S1_M3,Exact_Low_S1_M5,Exact_Low_S1_M7,Exact_Low_S1_M9)

Exact_Low_S025.mean <- c(Exact_Low_S025_M1.mean,Exact_Low_S025_M3.mean,Exact_Low_S025_M5.mean,Exact_Low_S025_M7.mean,Exact_Low_S025_M9.mean)
Exact_Low_S05.mean  <- c(Exact_Low_S05_M1.mean,Exact_Low_S05_M3.mean,Exact_Low_S05_M5.mean,Exact_Low_S05_M7.mean,Exact_Low_S05_M9.mean)
Exact_Low_S075.mean <- c(Exact_Low_S075_M1.mean,Exact_Low_S075_M3.mean,Exact_Low_S075_M5.mean,Exact_Low_S075_M7.mean,Exact_Low_S075_M9.mean)
Exact_Low_S1.mean   <- c(Exact_Low_S1_M1.mean,Exact_Low_S1_M3.mean,Exact_Low_S1_M5.mean,Exact_Low_S1_M7.mean,Exact_Low_S1_M9.mean)


Exact_Low           <- c(Exact_Low_S025,Exact_Low_S05,Exact_Low_S075,Exact_Low_S1)
Exact_Low.mean      <- c(Exact_Low_S025.mean,Exact_Low_S05.mean,Exact_Low_S075.mean,Exact_Low_S1.mean)



plot(Exact_Low.mean~Approx_Low.mean)

abline(coef = c(0,1),col="black",lty=2,lwd=3)


##################################
##### Zero autocorrelation #######
##################################

# sigma_h = 0.025
Sigma <- .025

Rad   <- 1
Approx_Rand_S025_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E1_S025_RAND),LEN)
Exact_Rand_S025_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E1_S025_RAND))
Exact_Rand_S025_M1.mean <- mean(Exact_Rand_S025_M1)

Rad   <- 3
Approx_Rand_S025_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E9_S025_RAND),LEN)
Exact_Rand_S025_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E9_S025_RAND))
Exact_Rand_S025_M3.mean <- mean(Exact_Rand_S025_M3)

Rad   <- 5
Approx_Rand_S025_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E25_S025_RAND),LEN)
Exact_Rand_S025_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E25_S025_RAND))
Exact_Rand_S025_M5.mean <- mean(Exact_Rand_S025_M5)

Rad   <- 7
Approx_Rand_S025_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E49_S025_RAND),LEN)
Exact_Rand_S025_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E49_S025_RAND))
Exact_Rand_S025_M7.mean <- mean(Exact_Rand_S025_M7)

Rad   <- 9
Approx_Rand_S025_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E81_S025_RAND),LEN)
Exact_Rand_S025_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E81_S025_RAND))
Exact_Rand_S025_M9.mean <- mean(Exact_Rand_S025_M9)

# sigma_h = 0.05
Sigma <- .05

Rad   <- 1
Approx_Rand_S05_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E1_S05_RAND),LEN)
Exact_Rand_S05_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E1_S05_RAND))
Exact_Rand_S05_M1.mean <- mean(Exact_Rand_S05_M1)

Rad   <- 3
Approx_Rand_S05_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E9_S05_RAND),LEN)
Exact_Rand_S05_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E9_S05_RAND))
Exact_Rand_S05_M3.mean <- mean(Exact_Rand_S05_M3)

Rad   <- 5
Approx_Rand_S05_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E25_S05_RAND),LEN)
Exact_Rand_S05_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E25_S05_RAND))
Exact_Rand_S05_M5.mean <- mean(Exact_Rand_S05_M5)

Rad   <- 7
Approx_Rand_S05_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E49_S05_RAND),LEN)
Exact_Rand_S05_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E49_S05_RAND))
Exact_Rand_S05_M7.mean <- mean(Exact_Rand_S05_M7)

Rad   <- 9
Approx_Rand_S05_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E81_S05_RAND),LEN)
Exact_Rand_S05_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E81_S05_RAND))
Exact_Rand_S05_M9.mean <- mean(Exact_Rand_S05_M9)

# sigma_h = 0.075
Sigma <- .075

Rad   <- 1
Approx_Rand_S075_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E1_S075_RAND),LEN)
Exact_Rand_S075_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E1_S075_RAND))
Exact_Rand_S075_M1.mean <- mean(Exact_Rand_S075_M1)

Rad   <- 3
Approx_Rand_S075_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E9_S075_RAND),LEN)
Exact_Rand_S075_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E9_S075_RAND))
Exact_Rand_S075_M3.mean <- mean(Exact_Rand_S075_M3)

Rad   <- 5
Approx_Rand_S075_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E25_S075_RAND),LEN)
Exact_Rand_S075_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E25_S075_RAND))
Exact_Rand_S075_M5.mean <- mean(Exact_Rand_S075_M5)

Rad   <- 7
Approx_Rand_S075_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E49_S075_RAND),LEN)
Exact_Rand_S075_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E49_S075_RAND))
Exact_Rand_S075_M7.mean <- mean(Exact_Rand_S075_M7)

Rad   <- 9
Approx_Rand_S075_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E81_S075_RAND),LEN)
Exact_Rand_S075_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E81_S075_RAND))
Exact_Rand_S075_M9.mean <- mean(Exact_Rand_S075_M9)


# sigma_h = 0.1
Sigma <- .1

Rad   <- 1
Approx_Rand_S1_M1 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E1_S1_RAND),LEN)
Exact_Rand_S1_M1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E1_S1_RAND))
Exact_Rand_S1_M1.mean <- mean(Exact_Rand_S1_M1)

Rad   <- 3
Approx_Rand_S1_M3 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E9_S1_RAND),LEN)
Exact_Rand_S1_M3  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E9_S1_RAND))
Exact_Rand_S1_M3.mean <- mean(Exact_Rand_S1_M3)

Rad   <- 5
Approx_Rand_S1_M5 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E25_S1_RAND),LEN)
Exact_Rand_S1_M5  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E25_S1_RAND))
Exact_Rand_S1_M5.mean <- mean(Exact_Rand_S1_M5)

Rad   <- 7
Approx_Rand_S1_M7 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E49_S1_RAND),LEN)
Exact_Rand_S1_M7  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E49_S1_RAND))
Exact_Rand_S1_M7.mean <- mean(Exact_Rand_S1_M7)

Rad   <- 9
Approx_Rand_S1_M9 <- rep(Inv.Approx(Sigma,A,Y,Rad,Hab.Rand,E81_S1_RAND),LEN)
Exact_Rand_S1_M9  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Rand,E81_S1_RAND))
Exact_Rand_S1_M9.mean <- mean(Exact_Rand_S1_M9)

# Rand together 
Approx_Rand_S025 <- c(Approx_Rand_S025_M1,Approx_Rand_S025_M3,Approx_Rand_S025_M5,Approx_Rand_S025_M7,Approx_Rand_S025_M9)
Approx_Rand_S05  <- c(Approx_Rand_S05_M1,Approx_Rand_S05_M3,Approx_Rand_S05_M5,Approx_Rand_S05_M7,Approx_Rand_S05_M9)
Approx_Rand_S075 <- c(Approx_Rand_S075_M1,Approx_Rand_S075_M3,Approx_Rand_S075_M5,Approx_Rand_S075_M7,Approx_Rand_S075_M9)
Approx_Rand_S1   <- c(Approx_Rand_S1_M1,Approx_Rand_S1_M3,Approx_Rand_S1_M5,Approx_Rand_S1_M7,Approx_Rand_S1_M9)

Approx_Rand_S025.mean <- c(Approx_Rand_S025_M1[1],Approx_Rand_S025_M3[1],Approx_Rand_S025_M5[1],Approx_Rand_S025_M7[1],Approx_Rand_S025_M9[1])
Approx_Rand_S05.mean  <- c(Approx_Rand_S05_M1[1],Approx_Rand_S05_M3[1],Approx_Rand_S05_M5[1],Approx_Rand_S05_M7[1],Approx_Rand_S05_M9[1])
Approx_Rand_S075.mean <- c(Approx_Rand_S075_M1[1],Approx_Rand_S075_M3[1],Approx_Rand_S075_M5[1],Approx_Rand_S075_M7[1],Approx_Rand_S075_M9[1])
Approx_Rand_S1.mean   <- c(Approx_Rand_S1_M1[1],Approx_Rand_S1_M3[1],Approx_Rand_S1_M5[1],Approx_Rand_S1_M7[1],Approx_Rand_S1_M9[1])

Approx_Rand           <- c(Approx_Rand_S025,Approx_Rand_S05,Approx_Rand_S075,Approx_Rand_S1)
Approx_Rand.mean      <- c(Approx_Rand_S025.mean,Approx_Rand_S05.mean,Approx_Rand_S075.mean,Approx_Rand_S1.mean)


Exact_Rand_S025 <- c(Exact_Rand_S025_M1,Exact_Rand_S025_M3,Exact_Rand_S025_M5,Exact_Rand_S025_M7,Exact_Rand_S025_M9)
Exact_Rand_S05  <- c(Exact_Rand_S05_M1,Exact_Rand_S05_M3,Exact_Rand_S05_M5,Exact_Rand_S05_M7,Exact_Rand_S05_M9)
Exact_Rand_S075 <- c(Exact_Rand_S075_M1,Exact_Rand_S075_M3,Exact_Rand_S075_M5,Exact_Rand_S075_M7,Exact_Rand_S075_M9)
Exact_Rand_S1   <- c(Exact_Rand_S1_M1,Exact_Rand_S1_M3,Exact_Rand_S1_M5,Exact_Rand_S1_M7,Exact_Rand_S1_M9)

Exact_Rand_S025.mean <- c(Exact_Rand_S025_M1.mean,Exact_Rand_S025_M3.mean,Exact_Rand_S025_M5.mean,Exact_Rand_S025_M7.mean,Exact_Rand_S025_M9.mean)
Exact_Rand_S05.mean  <- c(Exact_Rand_S05_M1.mean,Exact_Rand_S05_M3.mean,Exact_Rand_S05_M5.mean,Exact_Rand_S05_M7.mean,Exact_Rand_S05_M9.mean)
Exact_Rand_S075.mean <- c(Exact_Rand_S075_M1.mean,Exact_Rand_S075_M3.mean,Exact_Rand_S075_M5.mean,Exact_Rand_S075_M7.mean,Exact_Rand_S075_M9.mean)
Exact_Rand_S1.mean   <- c(Exact_Rand_S1_M1.mean,Exact_Rand_S1_M3.mean,Exact_Rand_S1_M5.mean,Exact_Rand_S1_M7.mean,Exact_Rand_S1_M9.mean)


Exact_Rand           <- c(Exact_Rand_S025,Exact_Rand_S05,Exact_Rand_S075,Exact_Rand_S1)
Exact_Rand.mean      <- c(Exact_Rand_S025.mean,Exact_Rand_S05.mean,Exact_Rand_S075.mean,Exact_Rand_S1.mean)

plot(Exact_Rand.mean~Approx_Rand.mean)

abline(coef = c(0,1),col="black",lty=2,lwd=3)





#################################
############ JCE Only ###########
#################################

Rad   <- 1
Approx_M1 <- rep(Inv.Approx_JCE(Sigma,A,Y,Rad,Hab.Low,E1_JCE),1)
Exact_M1  <- sapply(.5, function(x) Inv.Exact_JCE(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E1_JCE))
Exact_M1.mean <- mean(Exact_M1)

Rad   <- 3
Approx_M3 <- rep(Inv.Approx_JCE(Sigma,A,Y,Rad,Hab.Low,E9_JCE),1)
Exact_M3  <- sapply(.5, function(x) Inv.Exact_JCE(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E9_JCE))
Exact_M3.mean <- mean(Exact_M3)

Rad   <- 5
Approx_M5 <- rep(Inv.Approx_JCE(Sigma,A,Y,Rad,Hab.Low,E25_JCE),1)
Exact_M5  <- sapply(.5, function(x) Inv.Exact_JCE(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E25_JCE))
Exact_M5.mean <- mean(Exact_M5)

Rad   <- 7
Approx_M7 <- rep(Inv.Approx_JCE(Sigma,A,Y,Rad,Hab.Low,E49_JCE),1)
Exact_M7  <- sapply(.5, function(x) Inv.Exact_JCE(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E49_JCE))
Exact_M7.mean <- mean(Exact_M7)

Rad   <- 9
Approx_M9 <- rep(Inv.Approx_JCE(Sigma,A,Y,Rad,Hab.Low,E81_JCE),1)
Exact_M9  <- sapply(.5, function(x) Inv.Exact_JCE(x,H_Opt,Sigma,S,A,Y,Rad,Hab.Low,E81_JCE))
Exact_M9.mean <- mean(Exact_M9)

JCE.Approx <- c(Approx_M1,Approx_M3,Approx_M5,Approx_M7,Approx_M9)
JCE.Exact <- c(Exact_M1,Exact_M3,Exact_M5,Exact_M7,Exact_M9)


##################################
############# HP Only ############
##################################

Rad   <- 1 # JCEs don't matter, so this parameter doesn't mean anything... just here as a dummy parameter #

Sigma <- .025
Approx_S025 <- rep(Inv.Approx(Sigma,0,Y,Rad,Hab.High2,S025),1)
Exact_S025  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,0,Y,Rad,Hab.High2,S025))
Exact_S025.mean <- mean(Exact_S025)

Sigma <- .05
Approx_S05 <- rep(Inv.Approx(Sigma,0,Y,Rad,Hab.High2,S05),1)
Exact_S05  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,0,Y,Rad,Hab.High2,S05))
Exact_S05.mean <- mean(Exact_S05)

Sigma <- .075
Approx_S075 <- rep(Inv.Approx(Sigma,0,Y,Rad,Hab.High2,S075),1)
Exact_S075  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,0,Y,Rad,Hab.High2,S075))
Exact_S075.mean <- mean(Exact_S075)

Sigma <- .1
Approx_S1 <- rep(Inv.Approx(Sigma,0,Y,Rad,Hab.High2,S1),1)
Exact_S1  <- sapply(OPTS, function(x) Inv.Exact(x,H_Opt,Sigma,S,0,Y,Rad,Hab.High2,S1))
Exact_S1.mean <- mean(Exact_S1)

HP.Approx <- c(Approx_S025,Approx_S05,Approx_S075,Approx_S1)
HP.Exact <- c(Exact_S025.mean,Exact_S05.mean,Exact_S075.mean,Exact_S1.mean)

############################################
############## Make the plot ###############
############################################

# set axes 
LIMS <- c(0,13.5)
LIMX <- c(0,13.5)

# make plot (will save as an svg file)
svg(filename="Fig_S2.svg", 
    width=5, 
    height=5.5, 
    pointsize=12)

plot(jitter(Exact_High.mean,factor = 1,amount=.1)~jitter(Approx_High.mean,factor = 1),xaxt="n", yaxt="n",pch=21,xlab="",ylab="",col="black", bg=alpha("red", 0.70),lwd=2,cex=1.25,ylim=LIMS,xlim=LIMX,xaxt='n',yaxt='n')
par(new=T)
plot(jitter(Exact_Med.mean,factor = 1,amount=.1)~jitter(Approx_Med.mean,factor = 1),xaxt="n", yaxt="n",pch=21,xlab="",ylab="",col="black", bg=alpha("orange", 0.70),lwd=2,cex=1.25,ylim=LIMS,xlim=LIMX,xaxt='n',yaxt='n')
par(new=T)
plot(jitter(Exact_Low.mean,factor = 1,amount=.1)~jitter(Approx_Low.mean,factor = 1),xaxt="n", yaxt="n",pch=21,xlab="",ylab="",col="black", bg=alpha("purple", 0.70),lwd=2,cex=1.25,ylim=LIMS,xlim=LIMX,xaxt='n',yaxt='n')
par(new=T)
plot(jitter(Exact_Rand.mean,factor = 1,amount=.1)~jitter(Approx_Rand.mean,factor = 1),xaxt="n", yaxt="n",pch=21,xlab="",ylab="",col="black", bg=alpha("green", 0.70),lwd=2,cex=1.25,ylim=LIMS,xlim=LIMX,xaxt='n',yaxt='n')
par(new=T)
plot(jitter(JCE.Exact,factor = 1,amount=.1)~jitter(JCE.Approx,factor = 1),xaxt="n", yaxt="n",pch=21,xlab="",ylab="",col="black", bg=alpha("blue", 0.70),lwd=2,cex=1.25,ylim=LIMS,xlim=LIMS,xaxt='n',yaxt='n')
par(new=T)
plot(jitter(HP.Exact,factor = 1,amount=.1)~jitter(HP.Approx,factor = 1),xaxt="n", yaxt="n",pch=21,xlab="",ylab="",col="black", bg=alpha("yellow", 0.7),lwd=2,cex=1.25,ylim=LIMS,xlim=LIMS,xaxt='n',yaxt='n')

abline(coef = c(0,1),col="black",lty=2,lwd=3)
ticksx <- seq(0,14,by=2)
ticksy <- seq(0,14,by=2)
axis(side = 1, at = ticksx)
axis(side = 2,at = ticksy,las=2)

dev.off()


