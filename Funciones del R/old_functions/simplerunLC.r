 #xdata <- load("xdata.rda")
path <- "D:/DATA/datacm/DatosLC"

setwd(path)

#fnames <- c(file.path(path,dir("data_det_rd",pattern="^jm[a-zA-Z0-9_]*fdetec_rd_full.log$")))

fnames <-  c( file.path(path,dir(path,pattern="txt$")))

modality <- c("AV", "V", "AV", "V","AV", "V","AV", "V", "AV", "V")
subj <- c("A.L.") #, "A.G.", "A.L.", "A.L.","B.J.", "B.J.","C.R.", "C.R.", "J.N.", "J.N.")

aux <- NULL
for(i in 1:length(fnames)){
	xdata<-read.table(fnames[i],header=TRUE)
	xdata<-cbind(xdata,Subj=subj[i]) #, modality=modality[i])  # ,Sessio=i ,Cond="audio")
    xdata<-rbind(aux,xdata)
    aux<-xdata
}

rm(aux)

# REORGANIZACIÓN DE LOS DATOS #

xdata$conav[xdata$AV=="0"] <- 2   ## le doy valor 2 a Conav para la condición Visual

aux <- subset(xdata, conav==2)
aux$SOA <- -0.04
xdatas <- rbind(xdata, aux)
aux$SOA <- 0.04
xdatas <- rbind(xdatas, aux)
rm(aux)

xdatas$ssignal <- 0       #hay sonido o no en el trial
xdatas$ssignal[xdatas$conss<1] <- 1

xdatas$button <- 0
xdatas$button[xdatas$boton==0]<- -1
xdatas$button[xdatas$boton==1]<- 1

xdatas <- xdatas[,-8]




xdatas$correcto <-0
xdatas$correcto[xdatas$ssignal==1 & xdatas$button==0]<-1

idata <- subset (xdatas, location!=button & ssignal==0) # Guardamos errores

xdatas<-subset(xdatas, (ssignal==0 & location==button) | ssignal==1)       # Y eliminamos los errores

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#


reciprobit <- function(dd)
{

	e1 <- ecdf(dd$RT)
	p1 <- e1(dd$RT)
	qp1 <- qnorm(p1)
	(data.frame(rRT=-1/dd$RT,qp=qp1))
}


ecdfs <- function(dd)
{

	e1 <- ecdf(dd$RT)
	p1 <- e1(dd$RT)
	qp1 <-p1
	(data.frame(rRT=dd$RT,qp=qp1))
}

 xd2 <- subset(xdatas,  ssignal==1)
 xd2$correcto <- 1
 xd2$correcto[xd2$button==0] <- 0
 

Data <- ag(xd2, list(Correcto=correcto,RT=RT),list (SSDT=SSDs, Subj=Subj, SOA=SOA, conav=conav), mean)

ggplot(aes(x=SSDT,y=Correcto,color=factor(conav)),data=Data) + geom_point(size=4) + stat_gfit(args=list(fun=cgauss.lms,dfun=cgauss,ipars=c(0.03,0.04)),size=1.0)+ facet_grid(Subj~SOA) + theme_bw(15) + scale_y_continuous("Proportion of non canceled responses") + scale_x_continuous("Stop signal delay")



d4 <- ddply(subset(xdatas, xdatas$RT>0),.(Subj, SOA,conav),ecdfs)
ggplot(aes(x=rRT,y=qp,colour=factor(conav)),data=d4) + geom_line( size=0.8) + theme_bw(15) + facet_grid(Subj~SOA)  + scale_x_continuous("RT (s)") + scale_y_continuous("Probability") + xlim(0.18,0.45)


xd1<- subset(xdatas, RT>0)

d2 <- ddply(Datos,.(Subj, conav, SOA),df.fit,vars=c("SSDT","Correcto"),ipars=c(0.04,0.03))

d1 <- ddply(subset(xd1, ssignal==1),.(Subj, conav, SOA), median.df)


MEDIASLC <- ag(xd1, list(Correcto=correcto,RT=RT),list (Subj=Subj, SOA=SOA, conav=conav), mean)
MEDIASLC$CT <- 0

load("xdataHC.rda")        #cargo el data frame anterior

MEDIASHC <- ag(subset(xd1, xd1$Subj=="A.L."), list(Correcto=correcto,RT=RT),list (Subj=Subj, SOA=SOA, conav=conav), mean)
MEDIASHC$CT <- 1

MEDIAS <- rbind(MEDIASLC, MEDIASHC)
ggplot(aes(x=CT,y=RT,color=factor(conav), ),data=MEDIAS) + geom_point(size=4) + facet_grid(.~SOA) 


save(xdatas, "xdataHC.rda")

d3 <- ddply(subset(xdatas, xdatas$Subj=="joan"),.(SOA,conav, ssignal),reciprobit)

d3 <- ddply(subset(xdatas, xdatas$RT>0 & xdatas$ssignal==0),.(Subj, SOA,conav),reciprobit)


xbr <- seq(-7,-2) #the -recripocals
xlab <- as.character(round( -1/xbr,digits=3))
#xlab[len(xlab)] <- "Inf"

ybr <- seq(-1.0,4,by=0.5)
ylab <- as.character(round(pnorm( ybr)*100,digits=1))

ggplot(aes(x=rRT,y=qp,colour=factor(conav)),data=d3) + geom_point( size=0.7)  + facet_grid(Subj~SOA)  + theme_bw(15)+ scale_x_continuous("RT (s)",limits=c(-7,-2),breaks=xbr,labels=xlab) + scale_y_continuous("Cumulative Probability (%)",breaks=ybr,labels=ylab) + theme_bw(15)





#Bins bakery
d4 <- ddply(subset(xdatas, xdatas$RT>0 & xdatas$RT<0.5),.(SOA,conav, Subj),ecdfs)
d4 <- d3

n <- 0
bins <- data.frame()
bsize <- 0.05
Nbins <- (1/bsize)-1

for(i in 1:Nbins){
  d5  <- ag(subset(d4, d4$qp>n & d4$qp<bsize+n), list(rRT=rRT),list (SOA,conav, Subj), mean)
  n <- n+ bsize
  d5$n <- n
  bins <-rbind(bins, d5)
  }
names(bins) <- c("SOA", "conav", "Subj","RT", "n")

#~#~#~#~#~#~#~#~#~#~#~#~#~#


ggplot(aes(x=RT,y=n,colour=factor(conav)),data=bins) + geom_line( size=0.8)  + facet_grid(Subj~SOA) + xlim(0.18,0.45)   + scale_y_continuous("Probability") + scale_x_continuous("RT(s)")


alls <- ddply(bins,.(SOA,conav, n),mean) # lets joing the results in one subject
ggplot(aes(x=RT,y=n,colour=factor(conav)),data=alls) + geom_line( size=0.8)  + facet_grid(.~SOA)   + theme_bw(15)+ xlim(0.18,0.45)   + scale_y_continuous("Probability") + scale_x_continuous("RT(s)")


ggplot(aes(x=RT,y=n,colour=factor(conav)),data=bins) + geom_line( size=0.8)  + facet_grid(Subj~SOA) + xlim(0.18,0.45)   + scale_y_continuous("Probability") + scale_x_continuous("RT(s)")


ggplot(aes(x=rRT,y=qp,colour=factor(conav)),data=d4) + geom_line( size=0.8)  + facet_grid(Subj~SOA) + xlim(0.18,0.45)   + scale_y_continuous("Probability") + scale_x_continuous("RT(s)")

 + pal(2)



























#Vamos a intentar sacar los fittings de cada linea

Lfits <- function(aa){     #function to apply to the ddpy function
  coeff <- coefficients(lsfit(aa$rRT, aa$qp))
  (data.frame(b=coeff[2]))
  }


dH <- subset(d3, d3$qp>-2.5 & d3$qp< -0.8) #en reciprobits aprox el primer cuartil
dHH <- subset(d3, d3$qp>-0.8 & d3$qp< 2)

fitsH <-  ddply(dH,.(Subj, SOA,conav),Lfits)
fitsHH <-  ddply(dHH,.(Subj, SOA,conav),Lfits)

fitsH$b1 <- fitsHH$b

Lfits <- function(aa){     #calcular pendientes para ddply
  coeff <- coefficients(lsfit(aa$rRT, aa$qp))
  (data.frame(b=coeff[2]))
  }
