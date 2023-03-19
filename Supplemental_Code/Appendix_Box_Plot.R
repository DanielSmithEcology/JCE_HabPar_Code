#######################################################
############# Appendix Fig. S1: Box Plot ###############
#######################################################

# this code will reproduce supplementary Figure 1 

######### Functions ############

# Gaussian function 
Gaus <- function(x,y,s){
  
  Num <- ((x-y)^2)/(2*s^2)  
  Gau <- exp(-Num)
  
  return(Gau)  
}
# "Box" approximation 
Box <- function(x,y,z){
  
  if(x<y | x>z) return(0)
  
  return(1)
  
  
}


####### parameters for plot #######

# stuff for creating vectors 
LEN <- 10000
Vals <- seq(0,1,length=LEN)

# different sigma_h values 
Sh1 <- .025
Sh2 <- .05
Sh3 <- .075
Sh4 <- .1

# hopt_i (habitat preference; set to 0.5 so it's centered)
H <- .5

#### Run vectors for Gaussian and box functions ####
GF1<- sapply(Vals,function(x) Gaus(H,x,Sh1)) 
BF1 <- sapply(Vals,function(x) Box(x,H- (Sh1*sqrt(2*pi)*.5),H + (Sh1*sqrt(2*pi)*.5)  ) )

GF2<- sapply(Vals,function(x) Gaus(H,x,Sh2))
BF2 <- sapply(Vals,function(x) Box(x,H- (Sh2*sqrt(2*pi)*.5),H + (Sh2*sqrt(2*pi)*.5)  ) )

GF3 <- sapply(Vals,function(x) Gaus(H,x,Sh3))
BF3 <- sapply(Vals,function(x) Box(x,H- (Sh3*sqrt(2*pi)*.5),H + (Sh3*sqrt(2*pi)*.5)  ) )

GF4 <- sapply(Vals,function(x) Gaus(H,x,Sh4))
BF4 <- sapply(Vals,function(x) Box(x,H- (Sh4*sqrt(2*pi)*.5),H + (Sh4*sqrt(2*pi)*.5)  ) )


svg("Fig_S1.svg",
    width=8*1.85*.35*1.25, 
    height=8*1.85*.35*1.25, 
    pointsize=15)

par(mfrow=c(2,2), tcl=-.5, family="serif", mai=c(0.25,0.25,0.1,0.1), omi=c(.5,.5,0.1,.25))


YLIM=c(.03,.98)
XLIM=c(.03,.97)

plot(GF1~Vals,type="l",col="red",lwd=2,ylim=YLIM,xlim=XLIM,xlab="",ylab="",xaxt="n",yaxt="n")
par(new=T)
plot(BF1~Vals,type="l",col="black",lty=2,lwd=2,ylim=YLIM,xlim=XLIM,xlab="",ylab="",xaxt="n")
mtext(paste0("(", toupper(letters[1]), ")"), side = 3, adj = 0.025, padj=-1,
      line = -2)      

plot(GF2~Vals,type="l",col="red",lwd=2,ylim=YLIM,xlim=XLIM,xlab="",ylab="",xaxt="n",yaxt="n")
par(new=T)
plot(BF2~Vals,type="l",col="black",lty=2,lwd=2,ylim=YLIM,xlim=XLIM,xlab="",ylab="",yaxt="n",xaxt="n")
mtext(paste0("(", toupper(letters[2]), ")"), side = 3, adj = 0.025, padj=-1,
      line = -2)      

plot(GF3~Vals,type="l",col="red",lwd=2,ylim=YLIM,xlim=XLIM,xlab="",ylab="",xaxt="n",yaxt="n")
par(new=T)
plot(BF3~Vals,type="l",col="black",lty=2,lwd=2,ylim=YLIM,xlim=XLIM,xlab="",ylab="")
mtext(paste0("(", toupper(letters[3]), ")"), side = 3, adj = 0.025, padj=-1,
      line = -2)      

plot(GF4~Vals,type="l",col="red",lwd=2,ylim=YLIM,xlim=XLIM,xlab="",ylab="",xaxt="n",yaxt="n")
par(new=T)
plot(BF4~Vals,type="l",col="black",lty=2,lwd=2,ylim=YLIM,xlim=XLIM,xlab="",ylab="",yaxt="n",yaxt="n")
mtext(paste0("(", toupper(letters[4]), ")"), side = 3, adj = 0.025, padj=-1,
      line = -2)      

dev.off()


# This will create the plot lacking the axes labels. The figure was finished in Powerpoint (in which the labels were added)
