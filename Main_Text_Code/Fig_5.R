########################################################################
############################ Fig 5 Code ################################
########################################################################


# This code will calculate k_M for each habitat type for each value of M and sigma_h 
# Then, the code will plot species diversity vs. (1-e^-a) M^2 * k_M 


# your working directory must be set to "CSV_Files"  for the code to run correctly. 




################## Load the relevant data #################

# Once again: your working directory must be set to "CSV_Files" 


################ Load different Habitat Types ############## 

# REMINDER: you working directory must be set to "CSV_Files" 

dm <- 499 # dimension of community (needed to remake habitat matrices)
S <- 300

# High Autocorrelation 
Hab.High <- read.csv("Hab_MedHigh.csv")
Hab.High <- unname(unlist(Hab.High))
Hab.High <- matrix(Hab.High,nrow=dm,ncol=dm)

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


####################### Load the Spatially Explicit Model Outputs ##############################

# code will load the data and produce vectors of species diversity for each relevant case
# relevant cases include: different sigma_h and different levels of spatial autocorrelation (as in Fig. 3)

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

## put species diversity into ordered vectors  

RANDS <- c(D_S025_RAND,D_S05_RAND,D_S075_RAND,D_S1_RAND)
LOWS <- c(D_S025_LOW,D_S05_LOW,D_S075_LOW,D_S1_LOW)
MEDS <- c(D_S025_MED,D_S05_MED,D_S075_MED,D_S1_MED)
HIGHS <- c(D_S025_HIGH,D_S05_HIGH,D_S075_HIGH,D_S1_HIGH)

All.Diversity <- c(RANDS,LOWS,MEDS,HIGHS)

## And ordered for plots 
RANDS2 <- data.frame(rbind(D_S025_RAND,D_S05_RAND,D_S075_RAND,D_S1_RAND))
LOWS2  <- data.frame(rbind(D_S025_LOW,D_S05_LOW,D_S075_LOW,D_S1_LOW))
MEDS2  <- data.frame(rbind(D_S025_MED,D_S05_MED,D_S075_MED,D_S1_MED))
HIGHS2 <- data.frame(rbind(D_S025_HIGH,D_S05_HIGH,D_S075_HIGH,D_S1_HIGH))

All.Diversity_DF <- rbind(RANDS2,LOWS2,MEDS2,HIGHS2)

##################### Required functions / parameters #######################

## mat.torus ##
# takes a n x n matrix and treats it like a torus 
# relevant for the calculations / simulations (which take place on a torus, but are numerically saved in a matrix)
mat.torus <- function(Matrix,Rad,xcord,ycord){      # Torus of Space
  
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

## Count.Func ##
# calculates number of similar habitat types within M x M Moore neighborhood based on a focal patch 
Count.Func <- function(M,O,S){ 
  
  O.min <- O - S*sqrt(2*pi)*.5
  O.max <- O + S*sqrt(2*pi)*.5
  
  IN    <- length(M[M>O.min & M<O.max]) 
  
  return(IN)
  
}


# HP Term (that is, R(sigma_h, N) ) from main text (Eq. 2)
R.Func <- function(Sig,N){
  
  Coef     <- Sig*sqrt(2*pi)*N 
  
  Coef2    <- exp(-Coef)
  
  R <- 1 - Coef2
  
  return(R)
  
}



##########  parameters ###########
Seq.Sh <- c(.025,.05,.075,.1) # different values of sigma_h examined 
Seq.M  <- c(1,3,5,7,9)        # Different values of M (neighborhood effect size) examined 


###############################################################################################################
############################## Runs code to calculate k_M  ####################################################
###############################################################################################################


# this code will calculate k_M (the autocorrelation coefficient) for all values of Sigma_h and M 
# thus, for each habitat (High, Med, Low, Zero autocorrelation) this code will calculate k_M for:
# Sigma_h = 0.025, 0.05, 0.075, 0.10
# M = 1, 3, 5, 7, 9

# parameter 
LEN <- dm*dm # number of patches in each habitat 

########################################################################################################
############################ High Autocorrelation ######################################################
########################################################################################################

### Sigma_h = 0.025 ##
Sh025 <- Seq.Sh[1]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
High.Sh025 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh025*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.High[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.High,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh025) # number of similar patches therein 
    }
    ))
}
)

### Sigma_h = 0.05 ##
Sh05 <- Seq.Sh[2]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
High.Sh05 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh05*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.High[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.High,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh05) # number of similar patches therein 
    }
    ))
}
)

### Sigma_h = 0.075 ##
Sh075 <- Seq.Sh[3]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
High.Sh075 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh075*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.High[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.High,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh075) # number of similar patches therein 
    }
    ))
}
)


### Sigma_h = 0.10 ##
Sh10 <- Seq.Sh[4]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
High.Sh10 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh10*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.High[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.High,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh10) # number of similar patches therein 
    }
    ))
}
)

k_M.High <- c(High.Sh025,High.Sh05,High.Sh075,High.Sh10)

k_M.High_DF <- data.frame(rbind(High.Sh025,High.Sh05,High.Sh075,High.Sh10))

##########################################################################################################
############################ Medium Autocorrelation ######################################################
##########################################################################################################

### Sigma_h = 0.025 ##
Sh025 <- Seq.Sh[1]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
Med.Sh025 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh025*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.Med[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.Med,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh025) # number of similar patches therein 
    }
    ))
}
)

### Sigma_h = 0.05 ##
Sh05 <- Seq.Sh[2]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
Med.Sh05 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh05*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.Med[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.Med,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh05) # number of similar patches therein 
    }
    ))
}
)

### Sigma_h = 0.075 ##
Sh075 <- Seq.Sh[3]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
Med.Sh075 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh075*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.Med[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.Med,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh075) # number of similar patches therein 
    }
    ))
}
)


### Sigma_h = 0.10 ##
Sh10 <- Seq.Sh[4]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
Med.Sh10 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh10*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.Med[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.Med,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh10) # number of similar patches therein 
    }
    ))
}
)

k_M.Med <- c(Med.Sh025,Med.Sh05,Med.Sh075,Med.Sh10)
k_M.Med_DF <- data.frame(rbind(Med.Sh025,Med.Sh05,Med.Sh075,Med.Sh10))

##########################################################################################################
############################### Low Autocorrelation ######################################################
##########################################################################################################

### Sigma_h = 0.025 ##
Sh025 <- Seq.Sh[1]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
Low.Sh025 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh025*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.Low[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.Low,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh025) # number of similar patches therein 
    }
    ))
}
)

### Sigma_h = 0.05 ##
Sh05 <- Seq.Sh[2]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
Low.Sh05 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh05*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.Low[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.Low,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh05) # number of similar patches therein 
    }
    ))
}
)

### Sigma_h = 0.075 ##
Sh075 <- Seq.Sh[3]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
Low.Sh075 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh075*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.Low[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.Low,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh075) # number of similar patches therein 
    }
    ))
}
)


### Sigma_h = 0.10 ##
Sh10 <- Seq.Sh[4]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
Low.Sh10 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh10*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.Low[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.Low,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh10) # number of similar patches therein 
    }
    ))
}
)

k_M.Low <- c(Low.Sh025,Low.Sh05,Low.Sh075,Low.Sh10)
k_M.Low_DF <- data.frame(rbind(Low.Sh025,Low.Sh05,Low.Sh075,Low.Sh10))


##########################################################################################################
############################## Zero Autocorrelation ######################################################
##########################################################################################################

### Sigma_h = 0.025 ##
Sh025 <- Seq.Sh[1]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
Rand.Sh025 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh025*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.Rand[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.Rand,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh025) # number of similar patches therein 
    }
    ))
}
)

### Sigma_h = 0.05 ##
Sh05 <- Seq.Sh[2]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
Rand.Sh05 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh05*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.Rand[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.Rand,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh05) # number of similar patches therein 
    }
    ))
}
)

### Sigma_h = 0.075 ##
Sh075 <- Seq.Sh[3]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
Rand.Sh075 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh075*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.Rand[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.Rand,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh075) # number of similar patches therein 
    }
    ))
}
)


### Sigma_h = 0.10 ##
Sh10 <- Seq.Sh[4]  # set Sigma_h value 

# return k_M for all values of M (1x1, 3x3, 5x5, 7x7, 9x9)
Rand.Sh10 <- sapply(Seq.M, function(M){                   # for each M 
  (1/(Sh10*sqrt(2*pi)*M^2)) *               # scaling coefficient for k_M
    mean(sapply(1:LEN, function(x){          # examine each of the dm * dm points in the habitat  
      xi <- x - floor( (x-1)/dm)*dm            # index for column of matrix 
      yi <- ceiling(x/dm)                      # index for row of matrix 
      H.x.y   <- Hab.Rand[xi,yi]               # habitat type of focal patch 
      M.Neigh <- mat.torus(Hab.Rand,M,xi,yi)   # M x M Moore neighborhood around patch  
      C.Neigh <- Count.Func(M.Neigh,H.x.y,Sh10) # number of similar patches therein 
    }
    ))
}
)

k_M.Rand <- c(Rand.Sh025,Rand.Sh05,Rand.Sh075,Rand.Sh10)
k_M.Rand_DF <- data.frame(rbind(Rand.Sh025,Rand.Sh05,Rand.Sh075,Rand.Sh10))

#############################################################################################################################
################################# Plot Construction #########################################################################
#############################################################################################################################

#### Put together vector of (1-exp(-a))*M * k_M #### 

## All k_M ## 
k_M.vec      <- c(k_M.Rand,k_M.Low,k_M.Med,k_M.High)
k_M.vec_DF  <- rbind(k_M.Rand_DF,k_M.Low_DF,k_M.Med_DF,k_M.High_DF)


## Rfunc ## 
Sigs <- rep(c(rep(Seq.Sh[1],5),rep(Seq.Sh[2],5),rep(Seq.Sh[3],5),rep(Seq.Sh[4],5) ),4)

Sigs_DF <- data.frame(t(matrix(rep(c(rep(Seq.Sh[1],5),rep(Seq.Sh[2],5),rep(Seq.Sh[3],5),rep(Seq.Sh[4],5) ),4),nrow=5)))
M_DF <-  data.frame(t(matrix(rep(rep(Seq.M,4),4)^2,nrow=5))) 

R.Vals <- sapply(1:length(All.Diversity), function(x) R.Func(Sigs[x],All.Diversity[x]) )


R.Vals_DF <- data.frame(matrix(NA,ncol=5,nrow=16))
for(hh in 1:5) R.Vals_DF[,hh] <- sapply(1:length(All.Diversity_DF[,hh]), function(x) R.Func(Sigs[x],All.Diversity_DF[x,hh]) )


a <- .5
## (1-exp(-a))*M * k_M ##
Summary.Stat <- k_M.vec * (1 - exp(-a)) * (rep(rep(Seq.M,4),4)^2) * R.Vals 
Summary.Stat_DF <- (1 - exp(-a))*k_M.vec_DF*R.Vals_DF*M_DF
Summary.Stat2 <-  (1 - exp(-a)) * (rep(rep(Seq.M,4),4)^2) * R.Vals 
Summary.Stat2_DF <- (1 - exp(-a))*R.Vals_DF*M_DF


### Plot axes stuff ## 
XAX <- c(.5,5,50,500)
YAY <- c(10,50,250)

XLIM5 <- c(15,485)
YLIM5 <- c(8,250)
XLIM5 <- c(4.25,445)
YLIM5 <- c(13,250)


XLIM5 <- c(.23,445)
YLIM5 <- c(5,250)

### make the plots (will save to your working directory) ###


LM1 <- lm(log(All.Diversity,base=10)~log(Summary.Stat,base=10))
LM2 <- lm(log(All.Diversity,base=10)~log(Summary.Stat2,base=10))

set.seed(143)

svg("Fig_5.svg",
    width=7*1.5, 
    height=5, 
    pointsize=18)

par(mfrow=c(1,2), tcl=-.5, family="serif", mai=c(0.25,0.375,0.1,0.1), omi=c(.5,.5,0.1,.25))

plot(jitter(All.Diversity_DF$X1,factor=1)~jitter(Summary.Stat_DF$X1,factor=0),xaxt="n",yaxt="n",type = "p", pch = 21,col="black", bg="red",cex=1,xlab="",ylab="",log="yx",xlim=c(XLIM5),ylim=c(YLIM5))
par(new=T)
plot(jitter(All.Diversity_DF$X2,factor=1)~jitter(Summary.Stat_DF$X2,factor=0),xaxt="n",yaxt="n",type = "p", pch = 21,col="black", bg="orange",cex=1,xlab="",ylab="",log="yx",xlim=c(XLIM5),ylim=c(YLIM5))
par(new=T)
plot(jitter(All.Diversity_DF$X3,factor=1)~jitter(Summary.Stat_DF$X3,factor=0),xaxt="n",yaxt="n",type = "p", pch = 21,col="black", bg="green",cex=1,xlab="",ylab="",log="yx",xlim=c(XLIM5),ylim=c(YLIM5))
par(new=T)
plot(jitter(All.Diversity_DF$X4,factor=1)~jitter(Summary.Stat_DF$X4,factor=0),xaxt="n",yaxt="n",type = "p", pch = 21,col="black", bg="blue",cex=1,xlab="",ylab="",log="yx",xlim=c(XLIM5),ylim=c(YLIM5))
par(new=T)
plot(jitter(All.Diversity_DF$X5,factor=1)~jitter(Summary.Stat_DF$X5,factor=0),xaxt="n",yaxt="n",type = "p", pch = 21,col="black", bg="purple",cex=1,xlab="",ylab="",log="yx",xlim=c(XLIM5),ylim=c(YLIM5))

abline(LM1,col="black",lwd=5,lty=2)

mtext(paste0("(", toupper(letters[1]), ")"), side = 3, adj = 0.065, padj=-1,
      line = -2)  
axis(side=1,at=XAX,label=XAX,cex.axis=1.5)
axis(side=2,at=YAY,label=YAY,cex.axis=1.5,las=2)

plot(jitter(All.Diversity_DF$X1,factor=1)~jitter(Summary.Stat2_DF$X1,factor=0),xaxt="n",yaxt="n",type = "p", pch = 21,col="black", bg="red",cex=1,xlab="",ylab="",log="yx",xlim=c(XLIM5),ylim=c(YLIM5))
par(new=T)
plot(jitter(All.Diversity_DF$X2,factor=1)~jitter(Summary.Stat2_DF$X2,factor=0),xaxt="n",yaxt="n",type = "p", pch = 21,col="black", bg="orange",cex=1,xlab="",ylab="",log="yx",xlim=c(XLIM5),ylim=c(YLIM5))
par(new=T)
plot(jitter(All.Diversity_DF$X3,factor=1)~jitter(Summary.Stat2_DF$X3,factor=0),xaxt="n",yaxt="n",type = "p", pch = 21,col="black", bg="green",cex=1,xlab="",ylab="",log="yx",xlim=c(XLIM5),ylim=c(YLIM5))
par(new=T)
plot(jitter(All.Diversity_DF$X4,factor=1)~jitter(Summary.Stat2_DF$X4,factor=0),xaxt="n",yaxt="n",type = "p", pch = 21,col="black", bg="blue",cex=1,xlab="",ylab="",log="yx",xlim=c(XLIM5),ylim=c(YLIM5))
par(new=T)
plot(jitter(All.Diversity_DF$X5,factor=1)~jitter(Summary.Stat2_DF$X5,factor=0),xaxt="n",yaxt="n",type = "p", pch = 21,col="black", bg="purple",cex=1,xlab="",ylab="",log="yx",xlim=c(XLIM5),ylim=c(YLIM5))

abline(LM2,col="black",lwd=5,lty=2)

mtext(paste0("(", toupper(letters[2]), ")"), side = 3, adj = 0.065, padj=-1,
      line = -2)  
axis(side=1,at=XAX,label=XAX,cex.axis=1.5)

dev.off()


DATA <- data.frame(cbind(log(All.Diversity,base=10),log(Summary.Stat,base=10),log(Summary.Stat2,base=10)))
colnames(DATA) <- c("All.Diversity","Summary.Stat", "Summary.Stat2")

BF <- regressionBF(All.Diversity~Summary.Stat+Summary.Stat2,data=DATA)
BF2 <- BF/max(BF)
BF2
