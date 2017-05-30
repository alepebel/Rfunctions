
x <- seq(1,10)
y <- pnorm(x,5,0.5) #+ runif(10,0,0.1)
#y[y>0.999] <- 0.999
y2 <- pnorm(x,5,2.5) #+ runif(10,0,0.1)
#y2[y2>0.999] <- 0.999


x <- seq(1,10, by=0.01)
y <- dnorm(x,5,0.5) #+ runif(10,0,0.1)
#y[y>0.999] <- 0.999

plot(x,y)
points(x,y2,col=2)
lines(x,y,col=2)

d1 <- rbind(data.frame(x=x,y=y,f=0),data.frame(x=x,y=y2,f=1))

summary(fit <- glm(y ~ x,family=binomial(link="probit"))) #MLE
summary(fit2 <- glm(y2 ~ x,family=binomial(link="probit"))) #MLE


yhat <- predict(fit,newdata=data.frame(x=seq(1,10,len=100)))

lines(x=seq(1,10,len=100), pnorm(yhat), col="purple", lwd=2)

#probit(beta.0 + beta.1*5, inverse=T)
def("ggplot")
qplot(x,y,"point") + stat_smooth(method="glm",se=F,formula=y ~ x,family=binomial(link="probit")) 
last_plot() + stat_smooth(method="glm",se=F,formula=y ~ x,family=binomial(link="logit"),colour="red")

# modelfree and psyphy
library(modelfree)
library(psyphy)

x <- example01$x;#stim
r <- example01$r;# yes resp
m <- example01$m;# N trials

nobs <- 30
x <- a$soa
r <- a$Responses * nobs
m <- 30

resp.mat <- matrix(c(r, m-r), ncol = 2) # for psyphy
fitp <- psyfun.2asym(resp.mat ~ x, link = probit.2asym) #psyphy estima guesses i lapses

glmdata <- data.frame( cbind(x, r/m ,m ) );
names( glmdata ) <- c( "x", "resp", "m");
glmformula <- c( "resp ~ x" );
userlink<-probit_link(  0.008202256 , 0.09225316 );  # guesses and lapses estimats from psyfun.2asym   0.1168922 , 0.09875763 )
fit <- glm( glmformula, data = glmdata, weights = m, family = binomial( userlink ) );
 


probit.par <- function(probitfit)
{
  
  p <- (as.numeric(coef(probitfit)))
  list(mean=p[1]/(-p[2]),sd=1/-p[2])
  
}

probit.par(fit)  #first the intercept (x), second the slope (sd)

#obtenir els estimates de gamma i lambda de psyphy
zz <- textConnection("out.txt", "w",local=TRUE)
sink(zz)
fitp <- psyfun.2asym(resp.mat ~ x, link = probit.2asym) #psyphy estima guesses i lapses
sink()
close(zz)
lg <- as.numeric(strsplit(out.txt," ")[[1]][c(4,9)]) #lambda i gamma
as.numeric(strsplit(out.txt," ")[[2]][c(4,5)]) #lambda std error
as.numeric(strsplit(out.txt," ")[[2]][c(4,5)]) #gamma stderror
userlink<-probit_link( lg[2] , lg[1] )  # guesses and lapses estimats from psyfun.2asym
fit01 <- glm( glmformula , data = glmdata, weights = m, family = binomial("probit") )
fit <- glm( glmformula , data = glmdata, weights = m, family = binomial( userlink ) )
anova(fit01,fit,test="Chisq")
1-pchisq(deviance(fit01) - deviance(fit),1) #p=0.08 one-tail is significant

def("ggplot")
ggplot(glmdata,aes(x,resp,weight=m)) + geom_point(size=5) + stat_smooth(method="glm",se=F,formula=y ~ x,family=binomial(link="probit")) + ylim(c(0,1))# saturated
last_plot() + stat_smooth(method="glm",se=F,formula=y ~ x,family=binomial(link=userlink),col=2,size=1)


#cat(out.txt,sep="\n")


#mean in probit: intercept/slope
#sd: 1/slope

kk1 <- simul.pfun(ntrials=30,Nsub=5)
kk2 <- simul.pfun(ntrials=30,mu=5,sigma=4,Nsub=5,Cond=1)
kk1$R <- kk1$Resp/kk1$Total
kk2$R <- kk2$Resp/kk2$Total

sim1.lst <- lmList(R ~ Intensity | Subj,data=kk1,binomial(probit) )

print(plot(confint(sim1.lst)),more=T,split=c(1,1,3,1))


ggplot(rbind(kk1,kk2),aes(Intensity,Resp/Total,colour=as.factor(Cond))) + geom_point()+ stat_smooth(method="glm",se=F,formula=y ~ x,family=binomial(link="probit"),size=3) + scale_color_manual(values=myscale.col)

m1 <- glmer(cbind(Resp, Total - Resp) ~ Intensity + (1 | Subj),      	#fit the GLMM(probit link function)
            family = binomial(link = "probit"), data = kk1, nAGQ = 10) #rbind(kk1,kk2)
summary(m1)
m2 <- glmer(cbind(Resp, Total - Resp) ~ Intensity + (1 | Subj),      	#fit the GLMM(probit link function)
            family = binomial(link = "probit"), data = kk2, nAGQ = 10) #rbind(kk1,kk2)
delta.psy.probit(m1, lme4 = T)  
delta.psy.probit(m2, lme4 = T)  

m12 <- glmer(cbind(Resp, Total - Resp) ~ Intensity +Cond  + (1 | Subj),  		#fit the GLMM(probit link function)
             family = binomial(link = "probit"), data = rbind(kk1,kk2), nAGQ = 10) #rbind(kk1,kk2)
summary(m12)

m22 <- glmer(cbind(Resp, Total - Resp) ~ Intensity+Cond+Intensity:Cond + (1 | Subj),    	#fit the GLMM(probit link function)
             family = binomial(link = "probit"), data = rbind(kk1,kk2), nAGQ = 10) #rbind(kk1,kk2)
summary(m22)



anova(m12, m22, test = "Chisq")
delta.psy.probit(m1, lme4 = F)  		

