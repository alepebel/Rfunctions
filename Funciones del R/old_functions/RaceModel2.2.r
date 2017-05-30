require(ggplot2)    #Carga GGplot2

source("D:/UB/Exp Alex/myUtils.R")
source("D:/Funciones del R/stat-gfit.R")
source("D:/Funciones del R/fitfun.R")
# Cargar ggplot.R stagfit a mano

stat_gfit <- function (mapping = NULL, data = NULL, geom = "path", position = "identity", 
     n = 101, args = list(), ...) 
StatFit$new(mapping = mapping, data = data, geom = geom, 
    position = position, n = n, args = args, ...)
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

ntrials <- 10000              
sdata <- NULL

SSD <- seq(0.01, 0.17, by=0.04)
NT<- seq(0,0.5, length=N)
CONAV<- c(0,1,2)
Soa <- c(-0.04,0,0.04)
#SOA<- 0.04
 
#Generating distributions
N=2000

THres <- 0.01
percTH <- THres/6

#GO VISUAL
muG <- 0.035
sigmaG <- 0.0055
rateG <- rnorm(N,muG,sigmaG)

Rtdist <- THres/rateG

#AUDITORY
muA <- 0.045
sigmaA <- 0.0051
rateA <- rnorm(N,muA,sigmaA)

#RdistA <- THres/rateA
Coef <- 0.8
#STOP
muS <- 0.05
sigmaS <- 0.005
rateS <- rnorm(N,muS,sigmaS)
RtdistS <- THres/rateS
#SIMULATING

for(i in 1:ntrials){

rA <- sample(rateA,1) #Audio
rG <- sample(rateG,1) #Go
rS <- sample(rateS,1) #Stop

conav <- sample(CONAV,1)
soa <- sample(Soa,1)
ssd <- sample(SSD, 1)

#INESPECIFIC EFFECTS PRODUCED BY THE AUDITORY SIGNAL PRESENCE
#Efecto de la Warning Signal sobre el Threshold para la StopSignal
b <-0.0005-(-0.04*-0.005)
W <- 0
TUR <-0
if (conav!=2){
TUR <-   (-0.005* soa) + b -0.00005            # Linea de caida del efecto de Temporal Uncertainity Reduction
W  <- 0.00035
}
Stop <- (THres+W)/rS
Rtstop <- Stop + ssd

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
THa <- 0
THv <- 0
sOa <- 0

if(soa==-0.04)
{THa <- -soa* muA      #40 ms interaction before the visual target
 }
 
if(soa==0.04){
THv <- 0.04 * rG
}

if(soa==0.04){ #es el tiempo que pasa hasta que aparece el estímulo auditivo en SOA +40 ms
sOa <- soa }
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
 
 
if(conav==0){
RtG <- ((percTH - THa - THv)/((rA+rG)*Coef)) +  ((THres - percTH - TUR)/rG) + sOa
 }      #Si el SOA es 0 no hay tanta facilitación espacial

if(conav!=0){ #Para incongruente y visual,
RtG <-(THres - TUR)/rG
}

# if(conav==0 & SOA==){
#RtG <- ((percTH - THv)/((rA+rG)*Coef)) +  ((THres - percTH - W)/rG)
#}


correcto <- 0

if(RtG>Rtstop){
correcto <- 1
       }

datas <- c(i,ssd, soa, conav, RtG, Rtstop,correcto)
sdata <- data.frame(rbind(sdata, datas) )

}


names(sdata) <- c("trial","SSDT", "SOA" , "Conav","RT","RTstop","Correcto")

Datos <- ag(sdata, list(Correcto=1-Correcto,RT=RT),list(SOA=SOA, Conav=Conav, SSDT=SSDT), mean)

ggplot(aes(x=SSDT,y=Correcto,color=factor(Conav)),data=Datos) + geom_point(size=4) + stat_gfit(args=list(fun=cgauss.lms,dfun=cgauss,ipars=c(0.03,0.04)),size=1.0)+ facet_grid(~SOA)



d4 <- ddply(subset(sdata, sdata$RT>0),.(SOA,Conav),ecdfs)
ggplot(aes(x=rRT,y=qp,colour=factor(Conav)),data=d4) + geom_line( size=0.8)  + facet_grid(~SOA) + xlim(0.18,0.45)





d3 <- ddply(sdata,.(SOA,Conav),reciprobit)


xbr <- seq(-7,-2) #the -recripocals
xlab <- as.character(round( -1/xbr,digits=3))
#xlab[len(xlab)] <- "Inf"

ybr <- seq(-1.0,4,by=0.5)
ylab <- as.character(round(pnorm( ybr)*100,digits=1))

ggplot(aes(x=rRT,y=qp,colour=factor(Conav)),data=d3) + geom_point( size=0.7)  + facet_grid(.~SOA) + scale_x_continuous("RT (s)",limits=c(-7,-2),breaks=xbr,labels=xlab) + scale_y_continuous("cumulative Probability (%)",breaks=ybr,labels=ylab) + theme_bw(15)








## HAY Q MIRAR COMO SOLUCIONAR LO REFERENTE A LOS RT EN CONDICIONES INCONGRUENTES




if(SOA==0.0 & conav==1)
RtSP <-    percTH/(rA+rG)   +  ((THres - percTH )/rG)



















x <- seq(-5,5, by=1)
y<- x* muG
ya <- x* muA
plot(x,y)
lines (x,ya)