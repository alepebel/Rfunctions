source("D:/UB/Exp Alex/myUtils.R")
ntrials=2000
aux<-NULL
xdata<-NULL
for(i in 1:ntrials){

#TRIAL SETUP
SOA <- c(0,1)
soa<- sample(SOA, 1)
CONG<- c(0,1,2)
cong <- sample(CONG, 1)
SSD <- seq(1, 10, length=5)
ssd <- sample(SSD, 1)

N <- 1000

Nd<-100
thresH <- 10
thresV <-12
thresA <- 10

#VISUAL & AUDITORY DISTRIBUTIONS

#if(CONG!=2){
muV <- 0.60
muA <- 0.5
sigmaV <- 0.2
sigmaA <- 0.2


rateV <- rnorm(Nd,muV,sigmaV)
rateA <- rnorm(Nd,muA,sigmaA)
#hist(rate)
rtV <- thresV/rateV                   #RTs = threshold/pendiente  (Rango de pendientes siguiendo la distribución normal)
rtA <- thresA/rateA
#hist(rt1)
##xq()

NT<- seq(0,50, length=N)

#Visual stimulous
v<-sample(rateV, 1)
golineV <- v *NT
xthresV <- length(which(golineV<thresH))
golinesV <- v *NT[1:xthresV]

#Auditory stimulous

a<- sample(rateA, 1)
L<- -ssd*a
golineA <- (a *NT)+L
xthresA <- length(which(golineA<thresH))
golinesA <- (a *NT[1:xthresA])+L


plot(NT[1:xthresV], jitter(golinesV, 120), xlim=c(0,30), ylim=c(0,12), type='l', col="Purple" )
lines(NT[1:xthresA], jitter(golinesA, 120), col="Pink")
 T <-0*NT+thresH
lines(NT, T , col="Blue")

#SECOND STAGE

#GO & STOP DOSTRIBUTIONS
muG <- 0.5
muS <-0.7
sigmaG<-0.2
sigmaS<-0.2
thresG <- 10
thresS <-10

#INTERACTIONS   CONG=1 -> congruente   CONG=2 -> visual

if (xthresA>xthresV & CONG==1)
muG<-muG+0.075
if (xthresA<xthresV & CONG==1)
muG<-muG+0.25
if (xthresA>xthresV & CONG==0)
muG<- muG-0.
if (xthresA<xthresV & CONG==0)
muG<-muG+0.15
if (CONG==2)
muG<-muG

rateG <- rnorm(Nd,muG,sigmaG)
rateS <- rnorm(Nd,muS,sigmaS)
rtg <- thresG/rateG                   #RTs = threshold/pendiente  (Rango de pendientes siguiendo la distribución normal)
rts <- thresS/rateS

g <-sample(rateG, 1)
rtG <-thresG/g

goline <- g *NT
xthresG <- length(which(goline<thresH))
golines <- g *NT[1:xthresG]


s <-sample(rateS, 1)
rtS <-thresS/s

l<- -ssd*s
stopline <- (s *NT)+l
xthresS <- length(which(stopline<thresH))
stoplines <- (s *NT[1:xthresS])+l


plot(NT[1:xthresG], jitter(golines, 120), xlim=c(0,30), ylim=c(0,12), type='l', col="Green" )
lines(NT[1:xthresS], jitter(stoplines, 120), col="Red")
lines(NT, T , col="Blue")

correcto <-0
if(xthresG>xthresS)
correcto <-1


data <- NULL 
data<- c(i,cong,ssd,soa,correcto, rtG, rtS)

xdata<-rbind(xdata,data)
}

rownames(xdata) <- seq(1,dim(xdata)[1])
colnames(xdata) <- c("Trial","Cong","Ssd","Soa", "Correcto", "RT", "RTstop")
xdata <- as.data.frame(xdata) 



#ORGANIZANDO LOS DATOS

#dataset <-subset(xdata, Soa==1)
dataset <- subset(xdata, Soa==0)

dataV <- subset(dataset, Cong==2)
dataAV <- subset(dataset, Cong==1)
dataAbV <- subset(dataset, Cong==0)


datosV <- ag(dataV, list(Correcto=1-Correcto, RT=RT), list (Ssd=Ssd), mean)
datosSAV <- ag(dataAV,list(Correcto=1-Correcto, RT=RT), list (Ssd=Ssd), mean)

datosSAbV <- ag(dataAbV,list(Correcto=1-Correcto, RT=RT), list (Ssd=Ssd), mean)

plot (datosSAV$Ssd, datosSAV$Correcto, type='l', xlim= c(0,12), ylim=c(0.0,0.9), col="Red")
lines (datosSAbV$Ssd, datosSAbV$Correcto, col="Blue")
lines  (datosV$Ssd, datosV$Correcto, col="Green")
#lines(datosV$SSDT, datosV$RT, col="Green")
#lines (datosDAV$SSDT, datosDAV$RT, col="Orange")

 
windows()

  





for(i in 1:length(NT)){

golines[] <- rate[i] *NT

lines(NT, golines[], col="Green")
}









race.simul <- function(mu,sigma,threshold,N=1000)
{

	rate <- rnorm(N,mu,sigma)
	(threshold/rate)
}

later.lms <- function(p,xrt,N=length(xrt))
{
 #p[1]= mu
 #p[2]= sigma
 #p[3]= threshold

	x <- sort(xrt)

	rate <- rnorm(N,p[1],abs(p[2]))
	rthat <- sort(p[3]/rate)
	
	(sum((rthat-x)^2))

}

#call

fit <- optim(c(1,0.2,1),later.lms,xrt=rt1)

fit$par # to get the params: mu, sigma and threshold
                                
