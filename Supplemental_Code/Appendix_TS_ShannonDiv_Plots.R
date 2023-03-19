#####################################################################################
###################### Appendix Time series plots (Fig. S8-S11) ######################
#####################################################################################


# This code will reproduce the time series plots shown in the Appendix 
# To correctly produce the code, you need to set your working directory to "CSV_Files" 


# Required package 
library(latex2exp)


### Function(s) ###

# return shannon diversity 
ShDiv <- function(Dist){ 
  Dist2 <- Dist[Dist>0]
  
  -sum( Dist2*log(Dist2) )
  
}



# Load all the time series data (it will take 5-10 minutes, depending on your computer)

TS1_E1_RAND <- read.csv("TS_E1_RAND_BOTH_S025.csv")
TS2_E1_RAND <- read.csv("TS_E1_RAND_BOTH_S05.csv")
TS3_E1_RAND <- read.csv("TS_E1_RAND_BOTH_S075.csv")

TS1_E9_RAND <- read.csv("TS_E9_RAND_BOTH_S025.csv")
TS2_E9_RAND <- read.csv("TS_E9_RAND_BOTH_S05.csv")
TS3_E9_RAND <- read.csv("TS_E9_RAND_BOTH_S075.csv")

TS1_E25_RAND <- read.csv("TS_E25_RAND_BOTH_S025.csv")
TS2_E25_RAND <- read.csv("TS_E25_RAND_BOTH_S05.csv")
TS3_E25_RAND <- read.csv("TS_E25_RAND_BOTH_S075.csv")

TS1_E49_RAND <- read.csv("TS_E49_RAND_BOTH_S025.csv")
TS2_E49_RAND <- read.csv("TS_E49_RAND_BOTH_S05.csv")
TS3_E49_RAND <- read.csv("TS_E49_RAND_BOTH_S075.csv")

TS1_E81_RAND <- read.csv("TS_E81_RAND_BOTH_S025.csv")
TS2_E81_RAND <- read.csv("TS_E81_RAND_BOTH_S05.csv")
TS3_E81_RAND <- read.csv("TS_E81_RAND_BOTH_S075.csv")


TS1_E1_MEDLOW <- read.csv("TS_E1_MEDLOW_BOTH_S025.csv")
TS2_E1_MEDLOW <- read.csv("TS_E1_MEDLOW_BOTH_S05.csv")
TS3_E1_MEDLOW <- read.csv("TS_E1_MEDLOW_BOTH_S075.csv")

TS1_E9_MEDLOW <- read.csv("TS_E9_MEDLOW_BOTH_S025.csv")
TS2_E9_MEDLOW <- read.csv("TS_E9_MEDLOW_BOTH_S05.csv")
TS3_E9_MEDLOW <- read.csv("TS_E9_MEDLOW_BOTH_S075.csv")

TS1_E25_MEDLOW <- read.csv("TS_E25_MEDLOW_BOTH_S025.csv")
TS2_E25_MEDLOW <- read.csv("TS_E25_MEDLOW_BOTH_S05.csv")
TS3_E25_MEDLOW <- read.csv("TS_E25_MEDLOW_BOTH_S075.csv")

TS1_E49_MEDLOW <- read.csv("TS_E49_MEDLOW_BOTH_S025.csv")
TS2_E49_MEDLOW <- read.csv("TS_E49_MEDLOW_BOTH_S05.csv")
TS3_E49_MEDLOW <- read.csv("TS_E49_MEDLOW_BOTH_S075.csv")

TS1_E81_MEDLOW <- read.csv("TS_E81_MEDLOW_BOTH_S025.csv")
TS2_E81_MEDLOW <- read.csv("TS_E81_MEDLOW_BOTH_S05.csv")
TS3_E81_MEDLOW <- read.csv("TS_E81_MEDLOW_BOTH_S075.csv")


TS1_E1_MEDHIGH <- read.csv("TS_E1_MEDHIGH_BOTH_S025.csv")
TS2_E1_MEDHIGH <- read.csv("TS_E1_MEDHIGH_BOTH_S05.csv")
TS3_E1_MEDHIGH <- read.csv("TS_E1_MEDHIGH_BOTH_S075.csv")

TS1_E9_MEDHIGH <- read.csv("TS_E9_MEDHIGH_BOTH_S025.csv")
TS2_E9_MEDHIGH <- read.csv("TS_E9_MEDHIGH_BOTH_S05.csv")
TS3_E9_MEDHIGH <- read.csv("TS_E9_MEDHIGH_BOTH_S075.csv")

TS1_E25_MEDHIGH <- read.csv("TS_E25_MEDHIGH_BOTH_S025.csv")
TS2_E25_MEDHIGH <- read.csv("TS_E25_MEDHIGH_BOTH_S05.csv")
TS3_E25_MEDHIGH <- read.csv("TS_E25_MEDHIGH_BOTH_S075.csv")

TS1_E49_MEDHIGH <- read.csv("TS_E49_MEDHIGH_BOTH_S025.csv")
TS2_E49_MEDHIGH <- read.csv("TS_E49_MEDHIGH_BOTH_S05.csv")
TS3_E49_MEDHIGH <- read.csv("TS_E49_MEDHIGH_BOTH_S075.csv")

TS1_E81_MEDHIGH <- read.csv("TS_E81_MEDHIGH_BOTH_S025.csv")
TS2_E81_MEDHIGH <- read.csv("TS_E81_MEDHIGH_BOTH_S05.csv")
TS3_E81_MEDHIGH <- read.csv("TS_E81_MEDHIGH_BOTH_S075.csv")


TS1_E1_MED <- read.csv("TS_E1_MED_BOTH_S025.csv")
TS2_E1_MED <- read.csv("TS_E1_MED_BOTH_S05.csv")
TS3_E1_MED <- read.csv("TS_E1_MED_BOTH_S075.csv")

TS1_E9_MED <- read.csv("TS_E9_MED_BOTH_S025.csv")
TS2_E9_MED <- read.csv("TS_E9_MED_BOTH_S05.csv")
TS3_E9_MED <- read.csv("TS_E9_MED_BOTH_S075.csv")

TS1_E25_MED <- read.csv("TS_E25_MED_BOTH_S025.csv")
TS2_E25_MED <- read.csv("TS_E25_MED_BOTH_S05.csv")
TS3_E25_MED <- read.csv("TS_E25_MED_BOTH_S075.csv")

TS1_E49_MED <- read.csv("TS_E49_MED_BOTH_S025.csv")
TS2_E49_MED <- read.csv("TS_E49_MED_BOTH_S05.csv")
TS3_E49_MED <- read.csv("TS_E49_MED_BOTH_S075.csv")

TS1_E81_MED <- read.csv("TS_E81_MED_BOTH_S025.csv")
TS2_E81_MED <- read.csv("TS_E81_MED_BOTH_S05.csv")
TS3_E81_MED <- read.csv("TS_E81_MED_BOTH_S075.csv")


## Time-step  ##
# will examine time series every 400 time-steps (out of 50000 total time-steps)
# makes the code run faster / the final plots smaller in size 
Times <- seq(1,50001,by=400) 
dm <- 499

### Time series -- species richnes ### 

# For each of the above time-series, the following code will produce a vector
# The vector is the speies diversity every 400 time-stpes (starting from 1 and ending at 50000)

TS1_E1_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E1_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E1_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E1_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E1_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E1_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E9_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E9_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E9_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E9_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E9_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E9_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E25_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E25_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E25_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E25_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E25_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E25_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E49_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E49_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E49_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E49_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E49_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E49_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E81_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E81_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E81_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E81_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E81_RAND.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E81_RAND[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)





TS1_E1_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E1_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E1_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E1_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E1_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E1_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E9_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E9_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E9_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E9_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E9_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E9_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E25_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E25_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E25_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E25_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E25_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E25_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E49_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E49_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E49_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E49_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E49_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E49_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E81_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E81_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E81_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E81_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E81_MEDLOW.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E81_MEDLOW[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)





TS1_E1_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E1_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E1_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E1_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E1_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E1_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E9_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E9_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E9_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E9_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E9_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E9_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E25_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E25_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E25_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E25_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E25_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E25_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E49_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E49_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E49_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E49_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E49_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E49_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E81_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E81_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E81_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E81_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E81_MED.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E81_MED[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)



TS1_E1_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E1_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E1_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E1_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E1_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E1_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E9_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E9_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E9_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E9_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E9_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E9_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E25_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E25_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E25_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E25_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E25_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E25_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E49_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E49_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E49_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E49_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E49_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E49_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS1_E81_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS1_E81_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS2_E81_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS2_E81_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


TS3_E81_MEDHIGH.D <- sapply(Times,function(x) {
  
  LEN <- TS3_E81_MEDHIGH[x,2:301]
  LEN <- LEN[LEN>(0/dm^2)]
  return(exp(ShDiv(LEN)))
  
}
)


####################################################
################# Make the Plots ################### 
####################################################

# Code for axes
XLIM <- c(0,125)
XSEQ <- seq(0,125,length=length(Times))

ticksx <- c(0,25,50,75,100,125)
ticksy <- c(0,150,300)


### make the plots ###

# This will make each of the time series plots in the Appendix
# The files will save as SVGs in your working directory  

svg("Appendix_RAND_Shannon.svg",
    width=8*1.25*1.5, 
    height=11*1.5, 
    pointsize=18)

par(mfrow=c(6,4), tcl=-.5, family="serif", mai=c(0.25,0.1,.15,0.1), omi=c(.5,.5,0.15,.25))

plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h = 0.025)'), side=1,padj=-.45,cex=1.75)
plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h = 0.05)'), side=1,padj=-.45,cex=1.75)
plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h = 0.075)'), side=1,padj=-.45,cex=1.75)
plot.new()

plot(TS1_E1_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E1_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E1_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 1)'), side=2,padj=1.65,cex=1.5,adj=.5)


plot(TS1_E9_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E9_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E9_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 3)'), side=2,padj=1.65,cex=1.5,adj=.5)


plot(TS1_E25_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E25_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E25_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 5)'), side=2,padj=1.65,cex=1.5,adj=.5)


plot(TS1_E49_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E49_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E49_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 7)'), side=2,padj=1.65,cex=1.5,adj=.5)

plot(TS1_E81_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
axis(side = 1, at = ticksx)
plot(TS2_E81_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 1, at = ticksx)
plot(TS3_E81_RAND.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 1, at = ticksx)

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 9)'), side=2,padj=1.65,cex=1.5,adj=.5)

dev.off()



XLIM <- c(0,125)
XSEQ <- seq(0,125,length=length(Times))

ticksx <- c(0,25,50,75,100,125)
ticksy <- c(0,150,300)


svg("Appendix_MEDLOW_Shannon.svg",
    width=8*1.25*1.5, 
    height=11*1.5, 
    pointsize=18)

par(mfrow=c(6,4), tcl=-.5, family="serif", mai=c(0.25,0.1,.15,0.1), omi=c(.5,.5,0.15,.25))

plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h = 0.025)'), side=1,padj=-.45,cex=1.75)
plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h = 0.05)'), side=1,padj=-.45,cex=1.75)
plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h = 0.075)'), side=1,padj=-.45,cex=1.75)
plot.new()

plot(TS1_E1_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E1_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E1_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 1)'), side=2,padj=1.65,cex=1.5,adj=.5)


plot(TS1_E9_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E9_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E9_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 3)'), side=2,padj=1.65,cex=1.5,adj=.5)


plot(TS1_E25_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E25_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E25_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 5)'), side=2,padj=1.65,cex=1.5,adj=.5)


plot(TS1_E49_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E49_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E49_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 7)'), side=2,padj=1.65,cex=1.5,adj=.5)

plot(TS1_E81_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
axis(side = 1, at = ticksx)
plot(TS2_E81_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 1, at = ticksx)
plot(TS3_E81_MEDLOW.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 1, at = ticksx)

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 9)'), side=2,padj=1.65,cex=1.5,adj=.5)

dev.off()





XLIM <- c(0,125)
XSEQ <- seq(0,125,length=length(Times))

ticksx <- c(0,25,50,75,100,125)
ticksy <- c(0,150,300)


svg("Appendix_MED_Shannon.svg",
    width=8*1.25*1.5, 
    height=11*1.5, 
    pointsize=18)

par(mfrow=c(6,4), tcl=-.5, family="serif", mai=c(0.25,0.1,.15,0.1), omi=c(.5,.5,0.15,.25))

plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h = 0.025)'), side=1,padj=-.45,cex=1.75)
plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h = 0.05)'), side=1,padj=-.45,cex=1.75)
plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h = 0.075)'), side=1,padj=-.45,cex=1.75)
plot.new()

plot(TS1_E1_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E1_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E1_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 1)'), side=2,padj=1.65,cex=1.5,adj=.5)


plot(TS1_E9_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E9_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E9_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 3)'), side=2,padj=1.65,cex=1.5,adj=.5)


plot(TS1_E25_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E25_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E25_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 5)'), side=2,padj=1.65,cex=1.5,adj=.5)


plot(TS1_E49_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E49_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E49_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 7)'), side=2,padj=1.65,cex=1.5,adj=.5)

plot(TS1_E81_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
axis(side = 1, at = ticksx)
plot(TS2_E81_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 1, at = ticksx)
plot(TS3_E81_MED.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 1, at = ticksx)

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 9)'), side=2,padj=1.65,cex=1.5,adj=.5)

dev.off()



XLIM <- c(0,125)
XSEQ <- seq(0,125,length=length(Times))

ticksx <- c(0,25,50,75,100,125)
ticksy <- c(0,150,300)


svg("Appendix_MEDHIGH_Shannon.svg",
    width=8*1.25*1.5, 
    height=11*1.5, 
    pointsize=18)

par(mfrow=c(6,4), tcl=-.5, family="serif", mai=c(0.25,0.1,.15,0.1), omi=c(.5,.5,0.15,.25))

plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h = 0.025)'), side=1,padj=-.45,cex=1.75)
plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h = 0.05)'), side=1,padj=-.45,cex=1.75)
plot.new()
rect(-.05, .175, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($\sigma_h = 0.075)'), side=1,padj=-.45,cex=1.75)
plot.new()

plot(TS1_E1_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E1_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E1_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 1)'), side=2,padj=1.65,cex=1.5,adj=.5)


plot(TS1_E9_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E9_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E9_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 3)'), side=2,padj=1.65,cex=1.5,adj=.5)


plot(TS1_E25_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E25_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E25_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 5)'), side=2,padj=1.65,cex=1.5,adj=.5)


plot(TS1_E49_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
plot(TS2_E49_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
plot(TS3_E49_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 7)'), side=2,padj=1.65,cex=1.5,adj=.5)

plot(TS1_E81_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 2,at = ticksy,las=2)
axis(side = 1, at = ticksx)
plot(TS2_E81_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 1, at = ticksx)
plot(TS3_E81_MEDHIGH.D~XSEQ,ylim=c(8,298), xlim=c(2.5,121.5),type="p",pch=19,xaxt='n',yaxt='n',xlab="",ylab="")
axis(side = 1, at = ticksx)

plot.new()
rect(-.04, 1.0385, .127, -.05,xpd=T,lwd=3,col="lightgray")
mtext(TeX(r'($M = 9)'), side=2,padj=1.65,cex=1.5,adj=.5)

dev.off()


