#################################################################################
############################## Fig. 4 Code ######################################
#################################################################################

# This code will produce Fig. 4 From the main text 
# This will produce the plots, but it will lack axes labels / the labels for k_M
# the plot was finished in Powerpoint, where these additional lables were added. 


###########################
####### Functions #########
###########################

# HP Term (that is, R(sigma_h, N) ) from main text (Eq. 2)
R.Func <- function(Sig,N){
  
  Coef     <- Sig*sqrt(2*pi)*N 
  
  Coef2    <- exp(-Coef)
  
  R <- 1 - Coef2
  
  return(R)
  
}



# JCE + HP Term from the main text (Eq. 1)
JCE.TERM <- function(a,k,M,N,Sig){
  
  Coef     <- Sig*sqrt(2*pi)*N 
  
  Coef2    <- exp(-Coef)
  
  R <- 1 - Coef2
  
  JCE <- exp(-(1-exp(-a))*((M^2)/N)*k*R)
  
  
} 


###########################################
####### Produce vectors for plot ##########
###########################################

# parameters 
Length <- 10000 # length of vectors (arbitrary)

S_h <- .05 # sigma_h 
M1 <- 5 # Moore neighborhood size (5 x 5 Moore neighborhood)
M2 <- 7 # Moore neighborhood size (7 x 7 Moore neighborhood)
a <- 0.5 # baseline JCE strength 
k1 <- 1 # 1st value of k_M
k2 <- .5/(.05*sqrt(2*pi)) # 2nd value of k_M
k3 <- 1/(.05*sqrt(2*pi)) # 3rd value of k_M

Nz <- seq(.01,300,length=Length) # values of N (number of resident species)

## vector HP Term ##
R.S  <- sapply(Nz, function(x) R.Func(S_h,x)) 

# vectors JCE + HP Term ##
J.S1A <- sapply(Nz, function(x) JCE.TERM(a,k1,M1,x,S_h))
J.S2A <- sapply(Nz, function(x) JCE.TERM(a,k2,M1,x,S_h))
J.S3A <- sapply(Nz, function(x) JCE.TERM(a,k3,M1,x,S_h))

J.S1B <- sapply(Nz, function(x) JCE.TERM(a,k1,M2,x,S_h))
J.S2B <- sapply(Nz, function(x) JCE.TERM(a,k2,M2,x,S_h))
J.S3B <- sapply(Nz, function(x) JCE.TERM(a,k3,M2,x,S_h))

######## Make the Plot ######

# Plot axes stuff... 
XLIM <- c(5,289)
YLIM <- c(.02,.98)

XTIC <- c(0,100,200,300)
YTIC <- c(0,.5,1)

## Make figure (will save as SVG file to your directory) ##

svg("Fig_4.svg",
    width=8*2.5*.35*.5, 
    height=8*2.5*.35*1.25, 
    pointsize=18)

par(mfrow=c(3,1), tcl=-.5, family="serif", mai=c(0.25,0.25,0.1,0.1), omi=c(.5,.5,0.1,.25))


plot(R.S~Nz,type="l",ylim=YLIM,col="black",lwd=3,xlim=XLIM,xaxt="n",yaxt="n",xlab=" ",ylab=" ")
mtext(paste0("(", toupper(letters[1]), ")"), side = 3, adj = 0.065, padj=0,
      line = -2)      
axis(side=2,at=YTIC,label=YTIC,cex.axis=1,las=1)

plot(J.S1A~Nz,type="l",ylim=YLIM,xlim=XLIM,col="grey",lwd=3,xaxt="n",yaxt="n",xlab=" ",ylab=" ")
par(new=T)
plot(J.S1B~Nz,type="l",ylim=YLIM,xlim=XLIM,col="grey",lty=3,lwd=3,xaxt="n",yaxt="n",xlab=" ",ylab=" ")
par(new=T)
plot(J.S2A~Nz,type="l",ylim=YLIM,xlim=XLIM,col="orange",lwd=3,xaxt="n",yaxt="n",xlab=" ",ylab=" ")
par(new=T)
plot(J.S2B+.015~Nz,type="l",ylim=YLIM,xlim=XLIM,col="orange",lty=3,lwd=3,xaxt="n",yaxt="n",xlab=" ",ylab=" ") # minor adjust for visualization
par(new=T)
plot(J.S3A~Nz,type="l",ylim=YLIM,xlim=XLIM,col="red",lwd=3,xaxt="n",yaxt="n",xlab=" ",ylab=" ")
par(new=T)
plot(J.S3B~Nz,type="l",ylim=YLIM,xlim=XLIM,col="red",lty=3,lwd=3,xaxt="n",yaxt="n",xlab=" ",ylab=" ")
mtext(paste0("(", toupper(letters[2]), ")"), side = 3, adj = 0.065,  padj=0,
      line = -2)      
axis(side=2,at=YTIC,label=YTIC,cex.axis=1,las=1)


plot(J.S1A*R.S~Nz,type="l",ylim=YLIM,xlim=XLIM,col="grey",lwd=3,xaxt="n",yaxt="n",xlab=" ",ylab=" ")
par(new=T)
plot(J.S1B*R.S~Nz,type="l",ylim=YLIM,xlim=XLIM,lty=3,col="grey",lwd=3,xaxt="n",yaxt="n",xlab=" ",ylab=" ")
par(new=T)
plot(J.S2A*R.S~Nz,type="l",ylim=YLIM,xlim=XLIM,col="orange",lwd=3,xaxt="n",yaxt="n",xlab=" ",ylab=" ")
par(new=T)
plot(J.S2B*R.S+.015~Nz,type="l",ylim=YLIM,xlim=XLIM,lty=3,col="orange",lwd=3,xaxt="n",yaxt="n",xlab=" ",ylab=" ") # minor adjust for visualization
par(new=T)
plot(J.S3A*R.S~Nz,type="l",ylim=YLIM,xlim=XLIM,col="red",lwd=3,xaxt="n",yaxt="n",xlab=" ",ylab=" ")
par(new=T)
plot(J.S3B*R.S~Nz,type="l",ylim=YLIM,xlim=XLIM,lty=3,col="red",lwd=3,xaxt="n",yaxt="n",xlab=" ",ylab=" ")
mtext(paste0("(", toupper(letters[3]), ")"), side = 3, adj = 0.065,  padj=0,
      line = -2)      

axis(side=1,at=XTIC,label=XTIC,cex.axis=1)
axis(side=2,at=YTIC,label=YTIC,cex.axis=1,las=1)

dev.off()

