require(ggplot2)    #Carga GGplot2
source("D:/Funciones del R/cbootstrap.R")
source("D:/Funciones del R/myUtils.R")
source("D:/Funciones del R/stat-gfit.R")
source("D:/Funciones del R/fitfun.R")
source("D:/Funciones del R/pieron.R")
# Cargar ggplot.R stagfit a mano


path <- "d:/data/ContrastsData/alex"
setwd(path)

#fnames <- c(file.path(path,dir("data_det_rd",pattern="^jm[a-zA-Z0-9_]*fdetec_rd_full.log$")))

fnames <-  c( file.path(path,dir(path,pattern="txt$")))


modality <- c("B", "B", "B", "B","B", "B","AV", "V", "AV", "V")
subj <- c("A.L.", "C.R.","J.N.","K.T.", "X.A.","A.L.", "A.L.", "B.J.", "B.J.","C.R.", "C.R.", "J.N.", "J.N.")
experiment <- c("SF1","SF1","SF1","SF1","SF1","SF1","SF1","SF1")


aux <- NULL
for(i in 1:length(fnames)){
	xdata<-read.table(fnames[i],header=TRUE)
	xdata<-cbind(xdata,Subj=subj[i]) #, modality=modality[i], experiment=experiment[i])  # ,Sessio=i ,Cond="audio")
    xdata<-rbind(aux,xdata)
    aux<-xdata
}

rm(aux)
rm(subj)

# REORGANIZACIÓN DE LOS DATOS #       7, 8

#tipos de error


#tipos de error


xdata$errorA <- 0
xdata$errorA[(xdata$ct==0) & (xdata$ButtonPressed==1)] <- 1

xdata$errorB <- 0
xdata$errorB[(xdata$ct!=0) & (xdata$ButtonPressed==0)] <- 1


xdata$correcto <-0
xdata$correcto[xdata$errorA==0 & xdata$errorB==0]<-1


ct0 <- subset(xdata, ct==0)       # Analizando Error tipo FA


DatoErrorS <- ag(ct0, list(correcto=correcto),list ( Block=Block, Subj=Subj, Exp=Exp, SF=SF, AV=av, Session=Session), mean)

#xdata <- merge(xdata,  DatoErrorS, by.x = c("Subj", "SF","Session","Block" , "Exp", "av"), by.y = c("Subj", "SF", "Session",  "Block", "Exp", "AV"))


Xdata<- xdata #guardando los datos para mas adelante

xdata <- subset(xdata, ct!=0 & RT>0 )

xdata <- ddply(xdata,.(Subj, SF, Exp, Contrast,av ),dfilter)

#dataerror <- subset(xdata, correcto==0 & errorA==1)


Datos <- ag(subset(xdata), list(RT=RT),list ( Subj=Subj, SF=SF, Exp=Exp, Contrast=Contrast, Block=Block, AV=av), mean)
Datoslen <- ag(subset(xdata), list(RT=RT),list ( Subj=Subj, SF=SF,  Exp=Exp, Contrast=Contrast, Block=Block, AV=av), len)
DatoSD <- ag( subset(xdata), list(RT=RT),list ( Subj=Subj, SF=SF,  Exp=Exp, Contrast=Contrast, Block=Block, AV=av), sd)
Datos$SD <- DatoSD$RT
Datos$FA <- ag(subset(xdata), list(errorA=errorA),list ( Subj=Subj, SF=SF,  Exp=Exp, Contrast=Contrast, Block=Block, AV=av), len)$errorA

Datosem <- ag(subset(xdata), list(RT=RT),list ( Subj=Subj, SF=SF,  Exp=Exp, Contrast=Contrast, Block=Block, AV=av), sem)
Datos$sem <- Datosem$RT
Datos$L <- Datos$RT - 1.96 * Datos$sem
Datos$H <- Datos$RT + 1.96 * Datos$sem

limit <- subset(Datos)
limits <- aes(ymax=limit$H, ymin=limit$L)

ct0 <- subset(Xdata, ct==0)



##ANÁLISIS 1 EXPERIMENTO
lim1 <- subset( limit, Exp==1)
limits <- aes(ymax=lim1$H, ymin=lim1$L)

optims <- ddply(subset(Datos, Exp==1),.(Subj, Block, SF,AV, Exp ),pieron.optim.fit, vars=c("Contrast","RT", "sem"),ipars=c(0.03,0.4,0.1))

ddply(subset(Datos, Exp==1),.(Subj, Block, SF,AV, Exp ),powerf.fit, vars=c("Contrast","RT", "sem"),ipars=c(0.03,0.4),k=-1)



ggplot(aes(x=1/Contrast,y=RT, color=factor(SF)),data=subset(Datos,  Exp==1)) + geom_point(size=1.5) + theme_bw(15) +  facet_grid(.~AV)  + stat_smooth(method="lm", se=T)

ggplot(aes(x=Contrast,y=RT, color=factor(AV)),data=subset(Datos,  Exp==1 )) + geom_point(size=2.5) + stat_gfit(args=list(fun=pieron.lms,dfun=pieron,ipars=c(0.03,0.4,0.2)),size=1.0) + theme_bw(15) +  facet_grid(Subj~SF)
  + geom_errorbar(limits, width=0.1)
  
  
  
 lfuns <- ddply(subset(Datos, Exp==1),.(AV, Exp, SF, Subj ), vars=c("Contrast","RT"),lmodes.fit) #lmodes is configured to account for 1/C in the ecuation
 ggplot(aes(x=SF ,y=1/fits, color=factor(AV)),data=subset(lfuns)) + geom_point(size=1) + theme_bw(15) +  facet_grid(.~.)  + geom_abline(intercept=0,slope=1)  + stat_smooth(se=F, size=0.8) + nogrid

 anov <- aov( fits~(SF*AV)+ Error(Subj/(SF*AV)), subset(lfuns))
  summary(anov)

  Da1 <- subset(lfuns, AV==0)
  Da1 <- cbind(Da1, fitAV=subset(lfuns, AV==1)$fits)


ggplot(aes(x=fits ,y=fitAV, shape=factor(SF), color=factor(SF)),data=subset(Da1)) + geom_point(size=4) + theme_bw(15) +  facet_grid(.~.)  + geom_abline(intercept=0,slope=1)  + stat_smooth(method="lm", se=T, size=0.8) + xlim(c(0,0.02)) + ylim(c(0,0.02))



ct0 <- subset(Xdata,  Exp == 1 & ct==0)       # Analizando Error tipo FA
DatoErrorA <- ag(ct0, list(correcto=correcto),list ( Block=Block, Subj=Subj,  SF=SF, AV=av), mean)
ggplot(DatoErrorA, aes(x=SF, y=correcto,fill=factor(AV))) + geom_bar(position=position_dodge() ,stat="identity")   + facet_grid(Subj~.)      + theme_bw(15)


corFA <- cbind(DatoErrorA, slopes=lfuns$fits)
ggplot(aes(x=slopes ,y=correcto, shape=factor(SF), color=factor(SF)),data=subset(corFA)) + geom_point(size=4) + theme_bw(15) +  facet_grid(AV~.)   + stat_smooth(method="lm", se=T, size=0.8)
+ xlim(c(0,1.5)) + ylim(c(0,1.5))



Datos2 <- subset(Datos, Exp!=1 & Exp!=4 & Subj!="C.R." )       #el anova no refleja diferencias significativas en Exp 2 y 3
Datos2 <- ag( subset(xdata, Exp!=1 & Exp!=4 & Subj!="C.R." ), list(RT=RT),list ( Subj=Subj, SF=SF,  Contrast=Contrast, Block=Block, AV=av), mean)


ggplot(aes(x=1/Contrast,y=RT, color=factor(SF)),data=subset(Datos2)) + geom_point(size=1.5) + theme_bw(15) +  facet_grid(.~AV)  + stat_smooth(method="lm", se=F) + nogrid
ggplot(aes(x=1/Contrast,y=RT, shape=factor(AV), color=factor(Subj)),data=subset(Datos2)) + geom_point(size=3.5) + theme_bw(15) +  facet_grid(.~SF)  + stat_smooth(method="lm", se=T)

Datoslen <- ag(subset(xdata, Exp!=1 & Exp!=4 & Subj!="C.R." ), list(RT=RT),list ( Subj=Subj, SF=SF,   Contrast=Contrast,  AV=av), len)

 lfuns2 <- ddply(subset(Datos2),.(AV, SF, Subj ), vars=c("Contrast","RT"),lmodes.fit)
  anov <- aov( fits~(SF*AV)+ Error(Subj/(SF*AV)), subset(lfuns2))
  summary(anov)
  
  ggplot(aes(x=SF ,y=1/fits, color=factor(AV)),data=subset(lfuns2)) + geom_point(size=1) + theme_bw(15) +  facet_grid(.~.)  + geom_abline(intercept=0,slope=1)  + stat_smooth(se=F, size=0.8) + nogrid
            + xlim(c(0,0.03)) + ylim(c(0,0.03))

 Da2 <- subset(lfuns2, AV==0)
  Da2<- cbind(Da2, fitAV=subset(lfuns2, AV==1)$fits)

ggplot(aes(x=fits ,y=fitAV, color=factor(SF)),data=subset(Da2)) + geom_point(size=4) + theme_bw(15) +  facet_grid(.~.)  + geom_abline(intercept=0,slope=1)  + stat_smooth(method="lm", se=F, size=0.8) + xlim(c(0,0.03)) + ylim(c(0,0.03)) + nogrid

 nogrid <- opts(
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank(),
    panel.background = theme_blank(),
    axis.ticks = theme_blank()
  )
                                                   # &  Subj!="K.T."
t.test(fits ~ AV, data =  subset(lfuns2, SF==8.16 ), paired=T ,  alternative = c("greater"))

#lmodes is configured to account for 1/C in the ecuation
 ggplot(aes(x=AV,y=fits, color=factor(SF)),data=subset(lfuns2)) + geom_point(size=1.5) + theme_bw(15) +  facet_grid(.~.)  + stat_smooth(method="lm", se=F)