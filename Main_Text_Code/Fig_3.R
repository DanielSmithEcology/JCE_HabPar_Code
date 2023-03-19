#################################################################################
############################## Fig. 3 Code ######################################
#################################################################################

# this code will reproduce Fig. 3 from the main text 
# note that this code simply makes the Fig. 3 assuming the Spatially Explicit Model (SEM) simulations have already been run
# the SEM outputs are available in the folder "CSV_Files" 
# therefore, you set your working directory to "CSV_Files" for this code to work. 


# Required package
library(latex2exp)

setwd("C:/Users/smith/Outputs2") 


# some baseline parameters 
S <- 300 # initial number of species in the community (300) 
dm <- 499 # dimension of the community (499 x 499)


####### Load the Spatially Explicit Model Outputs #######

# code will load the data and produce vectors of species diversity for each relevant case
# relevant cases include: different sigma_h and different levels of spatial autocorrelation

# REMINDER: Set your working directory to the folder called "CSV_Files" 

E1_S025_HIGH <- read.csv("DIST_E1_MEDHIGH_BOTH_S025.csv")
E1_S025_HIGH <- table(factor(as.matrix(E1_S025_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S025_HIGH <- as.vector(E1_S025_HIGH)
E1_S025_HIGH <- length(E1_S025_HIGH[E1_S025_HIGH>0])

E9_S025_HIGH <- read.csv("DIST_E9_MEDHIGH_BOTH_S025.csv")
E9_S025_HIGH <- table(factor(as.matrix(E9_S025_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S025_HIGH <- as.vector(E9_S025_HIGH)
E9_S025_HIGH <- length(E9_S025_HIGH[E9_S025_HIGH>0])

E25_S025_HIGH <- read.csv("DIST_E25_MEDHIGH_BOTH_S025.csv")
E25_S025_HIGH <- table(factor(as.matrix(E25_S025_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S025_HIGH <- as.vector(E25_S025_HIGH)
E25_S025_HIGH <- length(E25_S025_HIGH[E25_S025_HIGH>0])

E49_S025_HIGH <- read.csv("DIST_E49_MEDHIGH_BOTH_S025.csv")
E49_S025_HIGH <- table(factor(as.matrix(E49_S025_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S025_HIGH <- as.vector(E49_S025_HIGH)
E49_S025_HIGH <- length(E49_S025_HIGH[E49_S025_HIGH>0])


E81_S025_HIGH <- read.csv("DIST_E81_MEDHIGH_BOTH_S025.csv")
E81_S025_HIGH <- table(factor(as.matrix(E81_S025_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S025_HIGH <- as.vector(E81_S025_HIGH)
E81_S025_HIGH <- length(E81_S025_HIGH[E81_S025_HIGH>0])

D_S025_HIGH <- c(E1_S025_HIGH,E9_S025_HIGH,E25_S025_HIGH,E49_S025_HIGH,E81_S025_HIGH)

E1_S05_HIGH <- read.csv("DIST_E1_MEDHIGH_BOTH_S05.csv")
E1_S05_HIGH <- table(factor(as.matrix(E1_S05_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S05_HIGH <- as.vector(E1_S05_HIGH)
E1_S05_HIGH <- length(E1_S05_HIGH[E1_S05_HIGH>0])

E9_S05_HIGH <- read.csv("DIST_E9_MEDHIGH_BOTH_S05.csv")
E9_S05_HIGH <- table(factor(as.matrix(E9_S05_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S05_HIGH <- as.vector(E9_S05_HIGH)
E9_S05_HIGH <- length(E9_S05_HIGH[E9_S05_HIGH>0])

E25_S05_HIGH <- read.csv("DIST_E25_MEDHIGH_BOTH_S05.csv")
E25_S05_HIGH <- table(factor(as.matrix(E25_S05_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S05_HIGH <- as.vector(E25_S05_HIGH)
E25_S05_HIGH <- length(E25_S05_HIGH[E25_S05_HIGH>0])

E49_S05_HIGH <- read.csv("DIST_E49_MEDHIGH_BOTH_S05.csv")
E49_S05_HIGH <- table(factor(as.matrix(E49_S05_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S05_HIGH <- as.vector(E49_S05_HIGH)
E49_S05_HIGH <- length(E49_S05_HIGH[E49_S05_HIGH>0])


E81_S05_HIGH <- read.csv("DIST_E81_MEDHIGH_BOTH_S05.csv")
E81_S05_HIGH <- table(factor(as.matrix(E81_S05_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S05_HIGH <- as.vector(E81_S05_HIGH)
E81_S05_HIGH <- length(E81_S05_HIGH[E81_S05_HIGH>0])

D_S05_HIGH <- c(E1_S05_HIGH,E9_S05_HIGH,E25_S05_HIGH,E49_S05_HIGH,E81_S05_HIGH)


E1_S075_HIGH <- read.csv("DIST_E1_MEDHIGH_BOTH_S075.csv")
E1_S075_HIGH <- table(factor(as.matrix(E1_S075_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S075_HIGH <- as.vector(E1_S075_HIGH)
E1_S075_HIGH <- length(E1_S075_HIGH[E1_S075_HIGH>0])

E9_S075_HIGH <- read.csv("DIST_E9_MEDHIGH_BOTH_S075.csv")
E9_S075_HIGH <- table(factor(as.matrix(E9_S075_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S075_HIGH <- as.vector(E9_S075_HIGH)
E9_S075_HIGH <- length(E9_S075_HIGH[E9_S075_HIGH>0])

E25_S075_HIGH <- read.csv("DIST_E25_MEDHIGH_BOTH_S075.csv")
E25_S075_HIGH <- table(factor(as.matrix(E25_S075_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S075_HIGH <- as.vector(E25_S075_HIGH)
E25_S075_HIGH <- length(E25_S075_HIGH[E25_S075_HIGH>0])

E49_S075_HIGH <- read.csv("DIST_E49_MEDHIGH_BOTH_S075.csv")
E49_S075_HIGH <- table(factor(as.matrix(E49_S075_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S075_HIGH <- as.vector(E49_S075_HIGH)
E49_S075_HIGH <- length(E49_S075_HIGH[E49_S075_HIGH>0])


E81_S075_HIGH <- read.csv("DIST_E81_MEDHIGH_BOTH_S075.csv")
E81_S075_HIGH <- table(factor(as.matrix(E81_S075_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S075_HIGH <- as.vector(E81_S075_HIGH)
E81_S075_HIGH <- length(E81_S075_HIGH[E81_S075_HIGH>0])

D_S075_HIGH <- c(E1_S075_HIGH,E9_S075_HIGH,E25_S075_HIGH,E49_S075_HIGH,E81_S075_HIGH)


E1_S1_HIGH <- read.csv("DIST_E1_MEDHIGH_BOTH_S1.csv")
E1_S1_HIGH <- table(factor(as.matrix(E1_S1_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S1_HIGH <- as.vector(E1_S1_HIGH)
E1_S1_HIGH <- length(E1_S1_HIGH[E1_S1_HIGH>0])

E9_S1_HIGH <- read.csv("DIST_E9_MEDHIGH_BOTH_S1.csv")
E9_S1_HIGH <- table(factor(as.matrix(E9_S1_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S1_HIGH <- as.vector(E9_S1_HIGH)
E9_S1_HIGH <- length(E9_S1_HIGH[E9_S1_HIGH>0])

E25_S1_HIGH <- read.csv("DIST_E25_MEDHIGH_BOTH_S1.csv")
E25_S1_HIGH <- table(factor(as.matrix(E25_S1_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S1_HIGH <- as.vector(E25_S1_HIGH)
E25_S1_HIGH <- length(E25_S1_HIGH[E25_S1_HIGH>0])

E49_S1_HIGH <- read.csv("DIST_E49_MEDHIGH_BOTH_S1.csv")
E49_S1_HIGH <- table(factor(as.matrix(E49_S1_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S1_HIGH <- as.vector(E49_S1_HIGH)
E49_S1_HIGH <- length(E49_S1_HIGH[E49_S1_HIGH>0])


E81_S1_HIGH <- read.csv("DIST_E81_MEDHIGH_BOTH_S1.csv")
E81_S1_HIGH <- table(factor(as.matrix(E81_S1_HIGH,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S1_HIGH <- as.vector(E81_S1_HIGH)
E81_S1_HIGH <- length(E81_S1_HIGH[E81_S1_HIGH>0])

D_S1_HIGH <- c(E1_S1_HIGH,E9_S1_HIGH,E25_S1_HIGH,E49_S1_HIGH,E81_S1_HIGH)


E1_S025_MED <- read.csv("DIST_E1_MED_BOTH_S025.csv")
E1_S025_MED <- table(factor(as.matrix(E1_S025_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S025_MED <- as.vector(E1_S025_MED)
E1_S025_MED <- length(E1_S025_MED[E1_S025_MED>0])

E9_S025_MED <- read.csv("DIST_E9_MED_BOTH_S025.csv")
E9_S025_MED <- table(factor(as.matrix(E9_S025_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S025_MED <- as.vector(E9_S025_MED)
E9_S025_MED <- length(E9_S025_MED[E9_S025_MED>0])

E25_S025_MED <- read.csv("DIST_E25_MED_BOTH_S025.csv")
E25_S025_MED <- table(factor(as.matrix(E25_S025_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S025_MED <- as.vector(E25_S025_MED)
E25_S025_MED <- length(E25_S025_MED[E25_S025_MED>0])

E49_S025_MED <- read.csv("DIST_E49_MED_BOTH_S025.csv")
E49_S025_MED <- table(factor(as.matrix(E49_S025_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S025_MED <- as.vector(E49_S025_MED)
E49_S025_MED <- length(E49_S025_MED[E49_S025_MED>0])


E81_S025_MED <- read.csv("DIST_E81_MED_BOTH_S025.csv")
E81_S025_MED <- table(factor(as.matrix(E81_S025_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S025_MED <- as.vector(E81_S025_MED)
E81_S025_MED <- length(E81_S025_MED[E81_S025_MED>0])

D_S025_MED <- c(E1_S025_MED,E9_S025_MED,E25_S025_MED,E49_S025_MED,E81_S025_MED)

E1_S05_MED <- read.csv("DIST_E1_MED_BOTH_S05.csv")
E1_S05_MED <- table(factor(as.matrix(E1_S05_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S05_MED <- as.vector(E1_S05_MED)
E1_S05_MED <- length(E1_S05_MED[E1_S05_MED>0])

E9_S05_MED <- read.csv("DIST_E9_MED_BOTH_S05.csv")
E9_S05_MED <- table(factor(as.matrix(E9_S05_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S05_MED <- as.vector(E9_S05_MED)
E9_S05_MED <- length(E9_S05_MED[E9_S05_MED>0])

E25_S05_MED <- read.csv("DIST_E25_MED_BOTH_S05.csv")
E25_S05_MED <- table(factor(as.matrix(E25_S05_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S05_MED <- as.vector(E25_S05_MED)
E25_S05_MED <- length(E25_S05_MED[E25_S05_MED>0])

E49_S05_MED <- read.csv("DIST_E49_MED_BOTH_S05.csv")
E49_S05_MED <- table(factor(as.matrix(E49_S05_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S05_MED <- as.vector(E49_S05_MED)
E49_S05_MED <- length(E49_S05_MED[E49_S05_MED>0])


E81_S05_MED <- read.csv("DIST_E81_MED_BOTH_S05.csv")
E81_S05_MED <- table(factor(as.matrix(E81_S05_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S05_MED <- as.vector(E81_S05_MED)
E81_S05_MED <- length(E81_S05_MED[E81_S05_MED>0])

D_S05_MED <- c(E1_S05_MED,E9_S05_MED,E25_S05_MED,E49_S05_MED,E81_S05_MED)


E1_S075_MED <- read.csv("DIST_E1_MED_BOTH_S075.csv")
E1_S075_MED <- table(factor(as.matrix(E1_S075_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S075_MED <- as.vector(E1_S075_MED)
E1_S075_MED <- length(E1_S075_MED[E1_S075_MED>0])

E9_S075_MED <- read.csv("DIST_E9_MED_BOTH_S075.csv")
E9_S075_MED <- table(factor(as.matrix(E9_S075_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S075_MED <- as.vector(E9_S075_MED)
E9_S075_MED <- length(E9_S075_MED[E9_S075_MED>0])

E25_S075_MED <- read.csv("DIST_E25_MED_BOTH_S075.csv")
E25_S075_MED <- table(factor(as.matrix(E25_S075_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S075_MED <- as.vector(E25_S075_MED)
E25_S075_MED <- length(E25_S075_MED[E25_S075_MED>0])

E49_S075_MED <- read.csv("DIST_E49_MED_BOTH_S075.csv")
E49_S075_MED <- table(factor(as.matrix(E49_S075_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S075_MED <- as.vector(E49_S075_MED)
E49_S075_MED <- length(E49_S075_MED[E49_S075_MED>0])


E81_S075_MED <- read.csv("DIST_E81_MED_BOTH_S075.csv")
E81_S075_MED <- table(factor(as.matrix(E81_S075_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S075_MED <- as.vector(E81_S075_MED)
E81_S075_MED <- length(E81_S075_MED[E81_S075_MED>0])

D_S075_MED <- c(E1_S075_MED,E9_S075_MED,E25_S075_MED,E49_S075_MED,E81_S075_MED)


E1_S1_MED <- read.csv("DIST_E1_MED_BOTH_S1.csv")
E1_S1_MED <- table(factor(as.matrix(E1_S1_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S1_MED <- as.vector(E1_S1_MED)
E1_S1_MED <- length(E1_S1_MED[E1_S1_MED>0])

E9_S1_MED <- read.csv("DIST_E9_MED_BOTH_S1.csv")
E9_S1_MED <- table(factor(as.matrix(E9_S1_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S1_MED <- as.vector(E9_S1_MED)
E9_S1_MED <- length(E9_S1_MED[E9_S1_MED>0])

E25_S1_MED <- read.csv("DIST_E25_MED_BOTH_S1.csv")
E25_S1_MED <- table(factor(as.matrix(E25_S1_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S1_MED <- as.vector(E25_S1_MED)
E25_S1_MED <- length(E25_S1_MED[E25_S1_MED>0])

E49_S1_MED <- read.csv("DIST_E49_MED_BOTH_S1.csv")
E49_S1_MED <- table(factor(as.matrix(E49_S1_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S1_MED <- as.vector(E49_S1_MED)
E49_S1_MED <- length(E49_S1_MED[E49_S1_MED>0])


E81_S1_MED <- read.csv("DIST_E81_MED_BOTH_S1.csv")
E81_S1_MED <- table(factor(as.matrix(E81_S1_MED,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S1_MED <- as.vector(E81_S1_MED)
E81_S1_MED <- length(E81_S1_MED[E81_S1_MED>0])

D_S1_MED <- c(E1_S1_MED,E9_S1_MED,E25_S1_MED,E49_S1_MED,E81_S1_MED)





E1_S025_LOW <- read.csv("DIST_E1_MEDLOW_BOTH_S025.csv")
E1_S025_LOW <- table(factor(as.matrix(E1_S025_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S025_LOW <- as.vector(E1_S025_LOW)
E1_S025_LOW <- length(E1_S025_LOW[E1_S025_LOW>0])

E9_S025_LOW <- read.csv("DIST_E9_MEDLOW_BOTH_S025.csv")
E9_S025_LOW <- table(factor(as.matrix(E9_S025_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S025_LOW <- as.vector(E9_S025_LOW)
E9_S025_LOW <- length(E9_S025_LOW[E9_S025_LOW>0])

E25_S025_LOW <- read.csv("DIST_E25_MEDLOW_BOTH_S025.csv")
E25_S025_LOW <- table(factor(as.matrix(E25_S025_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S025_LOW <- as.vector(E25_S025_LOW)
E25_S025_LOW <- length(E25_S025_LOW[E25_S025_LOW>0])

E49_S025_LOW <- read.csv("DIST_E49_MEDLOW_BOTH_S025.csv")
E49_S025_LOW <- table(factor(as.matrix(E49_S025_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S025_LOW <- as.vector(E49_S025_LOW)
E49_S025_LOW <- length(E49_S025_LOW[E49_S025_LOW>0])


E81_S025_LOW <- read.csv("DIST_E81_MEDLOW_BOTH_S025.csv")
E81_S025_LOW <- table(factor(as.matrix(E81_S025_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S025_LOW <- as.vector(E81_S025_LOW)
E81_S025_LOW <- length(E81_S025_LOW[E81_S025_LOW>0])

D_S025_LOW <- c(E1_S025_LOW,E9_S025_LOW,E25_S025_LOW,E49_S025_LOW,E81_S025_LOW)

E1_S05_LOW <- read.csv("DIST_E1_MEDLOW_BOTH_S05.csv")
E1_S05_LOW <- table(factor(as.matrix(E1_S05_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S05_LOW <- as.vector(E1_S05_LOW)
E1_S05_LOW <- length(E1_S05_LOW[E1_S05_LOW>0])

E9_S05_LOW <- read.csv("DIST_E9_MEDLOW_BOTH_S05.csv")
E9_S05_LOW <- table(factor(as.matrix(E9_S05_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S05_LOW <- as.vector(E9_S05_LOW)
E9_S05_LOW <- length(E9_S05_LOW[E9_S05_LOW>0])

E25_S05_LOW <- read.csv("DIST_E25_MEDLOW_BOTH_S05.csv")
E25_S05_LOW <- table(factor(as.matrix(E25_S05_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S05_LOW <- as.vector(E25_S05_LOW)
E25_S05_LOW <- length(E25_S05_LOW[E25_S05_LOW>0])

E49_S05_LOW <- read.csv("DIST_E49_MEDLOW_BOTH_S05.csv")
E49_S05_LOW <- table(factor(as.matrix(E49_S05_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S05_LOW <- as.vector(E49_S05_LOW)
E49_S05_LOW <- length(E49_S05_LOW[E49_S05_LOW>0])


E81_S05_LOW <- read.csv("DIST_E81_MEDLOW_BOTH_S05.csv")
E81_S05_LOW <- table(factor(as.matrix(E81_S05_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S05_LOW <- as.vector(E81_S05_LOW)
E81_S05_LOW <- length(E81_S05_LOW[E81_S05_LOW>0])

D_S05_LOW <- c(E1_S05_LOW,E9_S05_LOW,E25_S05_LOW,E49_S05_LOW,E81_S05_LOW)


E1_S075_LOW <- read.csv("DIST_E1_MEDLOW_BOTH_S075.csv")
E1_S075_LOW <- table(factor(as.matrix(E1_S075_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S075_LOW <- as.vector(E1_S075_LOW)
E1_S075_LOW <- length(E1_S075_LOW[E1_S075_LOW>0])

E9_S075_LOW <- read.csv("DIST_E9_MEDLOW_BOTH_S075.csv")
E9_S075_LOW <- table(factor(as.matrix(E9_S075_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S075_LOW <- as.vector(E9_S075_LOW)
E9_S075_LOW <- length(E9_S075_LOW[E9_S075_LOW>0])

E25_S075_LOW <- read.csv("DIST_E25_MEDLOW_BOTH_S075.csv")
E25_S075_LOW <- table(factor(as.matrix(E25_S075_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S075_LOW <- as.vector(E25_S075_LOW)
E25_S075_LOW <- length(E25_S075_LOW[E25_S075_LOW>0])

E49_S075_LOW <- read.csv("DIST_E49_MEDLOW_BOTH_S075.csv")
E49_S075_LOW <- table(factor(as.matrix(E49_S075_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S075_LOW <- as.vector(E49_S075_LOW)
E49_S075_LOW <- length(E49_S075_LOW[E49_S075_LOW>0])


E81_S075_LOW <- read.csv("DIST_E81_MEDLOW_BOTH_S075.csv")
E81_S075_LOW <- table(factor(as.matrix(E81_S075_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S075_LOW <- as.vector(E81_S075_LOW)
E81_S075_LOW <- length(E81_S075_LOW[E81_S075_LOW>0])


D_S075_LOW <- c(E1_S075_LOW,E9_S075_LOW,E25_S075_LOW,E49_S075_LOW,E81_S075_LOW)


E1_S1_LOW <- read.csv("DIST_E1_MEDLOW_BOTH_S1.csv")
E1_S1_LOW <- table(factor(as.matrix(E1_S1_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S1_LOW <- as.vector(E1_S1_LOW)
E1_S1_LOW <- length(E1_S1_LOW[E1_S1_LOW>0])

E9_S1_LOW <- read.csv("DIST_E9_MEDLOW_BOTH_S1.csv")
E9_S1_LOW <- table(factor(as.matrix(E9_S1_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S1_LOW <- as.vector(E9_S1_LOW)
E9_S1_LOW <- length(E9_S1_LOW[E9_S1_LOW>0])

E25_S1_LOW <- read.csv("DIST_E25_MEDLOW_BOTH_S1.csv")
E25_S1_LOW <- table(factor(as.matrix(E25_S1_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S1_LOW <- as.vector(E25_S1_LOW)
E25_S1_LOW <- length(E25_S1_LOW[E25_S1_LOW>0])

E49_S1_LOW <- read.csv("DIST_E49_MEDLOW_BOTH_S1.csv")
E49_S1_LOW <- table(factor(as.matrix(E49_S1_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S1_LOW <- as.vector(E49_S1_LOW)
E49_S1_LOW <- length(E49_S1_LOW[E49_S1_LOW>0])


E81_S1_LOW <- read.csv("DIST_E81_MEDLOW_BOTH_S1.csv")
E81_S1_LOW <- table(factor(as.matrix(E81_S1_LOW,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S1_LOW <- as.vector(E81_S1_LOW)
E81_S1_LOW <- length(E81_S1_LOW[E81_S1_LOW>0])


D_S1_LOW <- c(E1_S1_LOW,E9_S1_LOW,E25_S1_LOW,E49_S1_LOW,E81_S1_LOW)


E1_S025_RAND <- read.csv("DIST_E1_RAND_BOTH_S025.csv")
E1_S025_RAND <- table(factor(as.matrix(E1_S025_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S025_RAND <- as.vector(E1_S025_RAND)
E1_S025_RAND <- length(E1_S025_RAND[E1_S025_RAND>0])

E9_S025_RAND <- read.csv("DIST_E9_RAND_BOTH_S025.csv")
E9_S025_RAND <- table(factor(as.matrix(E9_S025_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S025_RAND <- as.vector(E9_S025_RAND)
E9_S025_RAND <- length(E9_S025_RAND[E9_S025_RAND>0])

E25_S025_RAND <- read.csv("DIST_E25_RAND_BOTH_S025.csv")
E25_S025_RAND <- table(factor(as.matrix(E25_S025_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S025_RAND <- as.vector(E25_S025_RAND)
E25_S025_RAND <- length(E25_S025_RAND[E25_S025_RAND>0])

E49_S025_RAND <- read.csv("DIST_E49_RAND_BOTH_S025.csv")
E49_S025_RAND <- table(factor(as.matrix(E49_S025_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S025_RAND <- as.vector(E49_S025_RAND)
E49_S025_RAND <- length(E49_S025_RAND[E49_S025_RAND>0])


E81_S025_RAND <- read.csv("DIST_E81_RAND_BOTH_S025.csv")
E81_S025_RAND <- table(factor(as.matrix(E81_S025_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S025_RAND <- as.vector(E81_S025_RAND)
E81_S025_RAND <- length(E81_S025_RAND[E81_S025_RAND>0])

D_S025_RAND <- c(E1_S025_RAND,E9_S025_RAND,E25_S025_RAND,E49_S025_RAND,E81_S025_RAND)

E1_S05_RAND <- read.csv("DIST_E1_RAND_BOTH_S05.csv")
E1_S05_RAND <- table(factor(as.matrix(E1_S05_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S05_RAND <- as.vector(E1_S05_RAND)
E1_S05_RAND <- length(E1_S05_RAND[E1_S05_RAND>0])

E9_S05_RAND <- read.csv("DIST_E9_RAND_BOTH_S05.csv")
E9_S05_RAND <- table(factor(as.matrix(E9_S05_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S05_RAND <- as.vector(E9_S05_RAND)
E9_S05_RAND <- length(E9_S05_RAND[E9_S05_RAND>0])

E25_S05_RAND <- read.csv("DIST_E25_RAND_BOTH_S05.csv")
E25_S05_RAND <- table(factor(as.matrix(E25_S05_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S05_RAND <- as.vector(E25_S05_RAND)
E25_S05_RAND <- length(E25_S05_RAND[E25_S05_RAND>0])

E49_S05_RAND <- read.csv("DIST_E49_RAND_BOTH_S05.csv")
E49_S05_RAND <- table(factor(as.matrix(E49_S05_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S05_RAND <- as.vector(E49_S05_RAND)
E49_S05_RAND <- length(E49_S05_RAND[E49_S05_RAND>0])


E81_S05_RAND <- read.csv("DIST_E81_RAND_BOTH_S05.csv")
E81_S05_RAND <- table(factor(as.matrix(E81_S05_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S05_RAND <- as.vector(E81_S05_RAND)
E81_S05_RAND <- length(E81_S05_RAND[E81_S05_RAND>0])

D_S05_RAND <- c(E1_S05_RAND,E9_S05_RAND,E25_S05_RAND,E49_S05_RAND,E81_S05_RAND)


E1_S075_RAND <- read.csv("DIST_E1_RAND_BOTH_S075.csv")
E1_S075_RAND <- table(factor(as.matrix(E1_S075_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S075_RAND <- as.vector(E1_S075_RAND)
E1_S075_RAND <- length(E1_S075_RAND[E1_S075_RAND>0])

E9_S075_RAND <- read.csv("DIST_E9_RAND_BOTH_S075.csv")
E9_S075_RAND <- table(factor(as.matrix(E9_S075_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S075_RAND <- as.vector(E9_S075_RAND)
E9_S075_RAND <- length(E9_S075_RAND[E9_S075_RAND>0])

E25_S075_RAND <- read.csv("DIST_E25_RAND_BOTH_S075.csv")
E25_S075_RAND <- table(factor(as.matrix(E25_S075_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S075_RAND <- as.vector(E25_S075_RAND)
E25_S075_RAND <- length(E25_S075_RAND[E25_S075_RAND>0])

E49_S075_RAND <- read.csv("DIST_E49_RAND_BOTH_S075.csv")
E49_S075_RAND <- table(factor(as.matrix(E49_S075_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S075_RAND <- as.vector(E49_S075_RAND)
E49_S075_RAND <- length(E49_S075_RAND[E49_S075_RAND>0])


E81_S075_RAND <- read.csv("DIST_E81_RAND_BOTH_S075.csv")
E81_S075_RAND <- table(factor(as.matrix(E81_S075_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S075_RAND <- as.vector(E81_S075_RAND)
E81_S075_RAND <- length(E81_S075_RAND[E81_S075_RAND>0])

D_S075_RAND <- c(E1_S075_RAND,E9_S075_RAND,E25_S075_RAND,E49_S075_RAND,E81_S075_RAND)



E1_S1_RAND <- read.csv("DIST_E1_RAND_BOTH_S1.csv")
E1_S1_RAND <- table(factor(as.matrix(E1_S1_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1_S1_RAND <- as.vector(E1_S1_RAND)
E1_S1_RAND <- length(E1_S1_RAND[E1_S1_RAND>0])

E9_S1_RAND <- read.csv("DIST_E9_RAND_BOTH_S1.csv")
E9_S1_RAND <- table(factor(as.matrix(E9_S1_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9_S1_RAND <- as.vector(E9_S1_RAND)
E9_S1_RAND <- length(E9_S1_RAND[E9_S1_RAND>0])

E25_S1_RAND <- read.csv("DIST_E25_RAND_BOTH_S1.csv")
E25_S1_RAND <- table(factor(as.matrix(E25_S1_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25_S1_RAND <- as.vector(E25_S1_RAND)
E25_S1_RAND <- length(E25_S1_RAND[E25_S1_RAND>0])

E49_S1_RAND <- read.csv("DIST_E49_RAND_BOTH_S1.csv")
E49_S1_RAND <- table(factor(as.matrix(E49_S1_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49_S1_RAND <- as.vector(E49_S1_RAND)
E49_S1_RAND <- length(E49_S1_RAND[E49_S1_RAND>0])


E81_S1_RAND <- read.csv("DIST_E81_RAND_BOTH_S1.csv")
E81_S1_RAND <- table(factor(as.matrix(E81_S1_RAND,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81_S1_RAND <- as.vector(E81_S1_RAND)
E81_S1_RAND <- length(E81_S1_RAND[E81_S1_RAND>0])

D_S1_RAND <- c(E1_S1_RAND,E9_S1_RAND,E25_S1_RAND,E49_S1_RAND,E81_S1_RAND)


E1 <- read.csv("DIST_E1_JCE.csv")
E1 <- table(factor(as.matrix(E1,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E1 <- as.vector(E1)
E1 <- length(E1[E1>0])

E9 <- read.csv("DIST_E9_JCE.csv")
E9 <- table(factor(as.matrix(E9,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E9 <- as.vector(E9)
E9 <- length(E9[E9>0])

E25 <- read.csv("DIST_E25_JCE.csv")
E25 <- table(factor(as.matrix(E25,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E25 <- as.vector(E25)
E25 <- length(E25[E25>0])

E49 <- read.csv("DIST_E49_JCE.csv")
E49 <- table(factor(as.matrix(E49,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E49 <- as.vector(E49)
E49 <- length(E49[E49>0])

E81 <- read.csv("DIST_E81_JCE.csv")
E81 <- table(factor(as.matrix(E81,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
E81 <- as.vector(E81)
E81 <- length(E81[E81>0])

Divs.JCE <- c(E1,E9,E25,E49,E81)
Ez       <- c(1,3,5,7,9)

S025 <- read.csv("DIST_HAB_S025.csv")
S025 <- table(factor(as.matrix(S025,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
S025 <- as.vector(S025)
S025 <- length(S025[S025>0])

S05 <- read.csv("DIST_HAB_S05.csv")
S05 <- table(factor(as.matrix(S05,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
S05 <- as.vector(S05)
S05 <- length(S05[S05>0])

S075 <- read.csv("DIST_HAB_S075.csv")
S075 <- table(factor(as.matrix(S075,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
S075 <- as.vector(S075)
S075 <- length(S075[S075>0])

S1 <- read.csv("DIST_HAB_S1.csv")
S1 <- table(factor(as.matrix(S1,nrow=dm,ncol=dm), levels = 1:S))/(dm*dm)
S1 <- as.vector(S1)
S1 <- length(S1[S1>0])

Divs.HP <- c(S025,S05,S075,S1) 



#########################################################
################## Make the Figures #####################
#########################################################


# Axes stuff 
YLIM <- c(0,265)
ticksx <- c(1,3,5,7,9)
ticksy <- c(0,100,200)


#### save the figure ####

# will save figure as an SVG file in your working directory

svg("Fig_3B.svg",
    width=12*1.2*1.075, 
    height=12*1.075*1.1, 
    pointsize=18)

par(mfrow=c(7,7), tcl=-.5, family="serif", mai=c(0.25,0.1,0,0.1), omi=c(.5,.5,0,.25))

plot.new()
plot.new()
mtext("Level of spatial autocorrelation", side=1,padj=5.25,cex=1.5,adj=-.65)
plot.new()
plot.new()
plot.new()
plot.new()

plot.new()
plot.new()
plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext("High", side=1,padj=-.85,cex=1.5)
plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext("Medium", side=1,padj=-.85,cex=1.5)
plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext("Low", side=1,padj=-.85,cex=1.5)
plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext("Zero", side=1,padj=-.85,cex=1.5)
plot.new()


plot.new()
plot.new()

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S025_HIGH~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[1],lty=2,lwd=2,col='sienna2')
axis(side = 2,at = ticksy,las=2)
mtext(paste0("(", toupper(letters[1]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S025_MED~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[1],lty=2,lwd=2,col='sienna2')
mtext(paste0("(", toupper(letters[2]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S025_LOW~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[1],lty=2,lwd=2,col='sienna2')
mtext(paste0("(", toupper(letters[3]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S025_RAND~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[1],lty=2,lwd=2,col='sienna2')
mtext(paste0("(", toupper(letters[4]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot.new()

rect(-.085, 1.035, .175, -.025,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h=0.025$)'), side=2,padj=1.05,cex=1.45,adj=.5)


plot.new()
plot.new()

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S05_HIGH~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[2],lty=2,lwd=2,col='sienna2')
axis(side = 2,at = ticksy,las=2)
mtext(paste0("(", toupper(letters[5]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S05_MED~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[2],lty=2,lwd=2,col='sienna2')
mtext(paste0("(", toupper(letters[6]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S05_LOW~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[2],lty=2,lwd=2,col='sienna2')
mtext(paste0("(", toupper(letters[7]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S05_RAND~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[2],lty=2,lwd=2,col='sienna2')
mtext(paste0("(", toupper(letters[8]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot.new()
rect(-.085, 1.035, .175, -.025,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h=0.05$)'), side=2,padj=1.05,cex=1.45,adj=.5)



##

plot.new()
plot.new()
mtext(TeX(r'(Species Richness)'), side=2,padj=3,cex=2.35,adj=.1)


plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S075_HIGH~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[3],lty=2,lwd=2,col='sienna2')
axis(side = 2,at = ticksy,las=2)
mtext(paste0("(", toupper(letters[9]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S075_MED~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[3],lty=2,lwd=2,col='sienna2')
mtext(paste0("(", toupper(letters[10]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S075_LOW~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[3],lty=2,lwd=2,col='sienna2')
mtext(paste0("(", toupper(letters[11]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S075_RAND~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",xaxt="n",ylim=YLIM,yaxt="n")
abline(h=Divs.HP[3],lty=2,lwd=2,col='sienna2')
mtext(paste0("(", toupper(letters[12]), ")"), side = 3, adj = 0.05, 
      line = -1.3)


plot.new()
rect(-.085, 1.035, .175, -.025,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h=0.075$)'), side=2,padj=1.05,cex=1.45,adj=.5)





plot.new()
plot.new()

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S1_HIGH~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[4],lty=2,lwd=2,col='sienna2')
axis(side = 2,at = ticksy,las=2)
axis(side = 1, at = ticksx)
mtext(paste0("(", toupper(letters[13]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S1_MED~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[4],lty=2,lwd=2,col='sienna2')
axis(side = 1, at = ticksx)
mtext(paste0("(", toupper(letters[14]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S1_LOW~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[4],lty=2,lwd=2,col='sienna2')
axis(side = 1, at = ticksx)
mtext(paste0("(", toupper(letters[15]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM,type="b",col="black", pch=21,bg="grey",cex=1.5,xaxt="n",yaxt="n")
par(new=T)
plot(D_S1_RAND~Ez,col="black", pch=21,bg="red",cex=1.5,type="b",ylim=YLIM,xaxt="n",yaxt="n")
abline(h=Divs.HP[4],lty=2,lwd=2,col='sienna2')
axis(side = 1, at = ticksx)
mtext(paste0("(", toupper(letters[16]), ")"), side = 3, adj = 0.05, 
      line = -1.3)

plot.new()
rect(-.085, 1.035, .175, -.025,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h=0.10$)'), side=2,padj=1.05,cex=1.45,adj=.5)



plot.new()
plot.new()
mtext(TeX(r'($M$ (Moore neighborhood size) )'), side=1,padj=-2.5,cex=2.1,adj=-.7)
plot.new()
plot.new()
plot.new()

dev.off()



