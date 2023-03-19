#################################################################################
############################## Fig. 2 Code ######################################
#################################################################################

# This R script will re-create Fig. 2 based on the SEM runs 
# This uses the SEM Outputs from the file "CSV_Files" 

######### your working directory must be set to "CSV_Files"  ####################



## required packages 
library(scales)




########## Load the Data ###########

## baseline parameters 
S <- 300 # initial number of species 
dm <- 499 # dimension of the community (499 x 499)

# Load diversity of each simulation (from "CSV_Files")
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

################################################
############# Make the plot ####################
################################################

# parameters 
Sh      <- c(.025,.05,.075,.1)
YLIM <- c(2.25,60)
XLIM <- c(.025,.1)
XLIM2 <- c(1,9)

TicksY <- c(0,20,40,60)
TicksX1 <- c(.025,.05,.075,.1)
TicksX2 <- c(1,3,5,7,9)


# lower bound of species diversity 
Div.Est <- sapply(Sh, function(x) 1/(x*sqrt(2*pi)))


svg("Fig_2.svg",
    width=8, 
    height=8, 
    pointsize=18)

par(mfrow=c(2,2), tcl=-.5, family="serif", mai=c(0.25,0.1,0,0.1), omi=c(.5,.5,0,.25))

plot.new()
rect(-.05, .05, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext("Communities with only HP", side=1,padj=-1.25,cex=1)
plot.new()
rect(-.05, .05, 1.05, -.075,xpd=T,lwd=3,col="lightgray")
mtext("Communities with only JCEs", side=1,padj=-1.25,cex=1.)



plot(Divs.HP~Sh,ylim=YLIM, xlim=XLIM,col="pink",xaxt="n",yaxt="n",type = "b",pch = 21,lty=1,lwd=2,xlab="",ylab="")
points(Sh, Divs.HP, pch = 21,col="black", bg="pink",cex=2)
par(new=T)
plot(Div.Est~Sh,ylim=YLIM, xlim=XLIM,col=alpha("black",.5),xaxt="n",yaxt="n",type = "b",pch = 21,lty=1,lwd=2,xlab="",ylab="",)
points(Sh, Div.Est, pch = 21,col=alpha("black",.5), bg=alpha("black",.5),cex=1.25)
axis(side=2,at=TicksY,label=TicksY,cex.axis=1,las=2)
axis(side=1,at=TicksX1,label=TicksX1,cex.axis=1,las=1)
mtext(paste0("(", toupper(letters[1]), ")"), side = 3, adj = 0.05, cex=1.05,padj=.5,
      line = -1.3)

plot(Divs.JCE~Ez,ylim=YLIM, xlim=XLIM2,col="grey",xaxt="n",yaxt="n",type = "b",pch = 21,lty=1,lwd=2,xlab="",ylab="")
points(Ez, Divs.JCE, pch = 21,col="black", bg="grey",cex=2)
axis(side=1,at=TicksX2,label=TicksX2,cex.axis=1,las=1)
mtext(paste0("(", toupper(letters[2]), ")"), side = 3, adj = 0.05, cex=1.05,padj=.5,
      line = -1.3)
dev.off()

