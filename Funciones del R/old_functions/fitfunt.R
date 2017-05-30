#cgauss.mle <- function(pse=NULL,sig=NULL)
# 	-sum( n *(y*log(stats::pnorm(x,pse,sig)) + (1-y)*log(1-(stats::pnorm(x,pse,sig)) ) ))
#
#
#logis.mle <- function(pse=NULL,sig=NULL)
# 	-sum( n *(y*log(stats::plogis(x,pse,sig)) + (1-y)*log(1-(stats::plogis(x,pse,sig)) ) ))
#
#
#cgaussian.mle <- function(p,x,y,no=7)
# 	-sum( no *(y*log(stats::pnorm(x,p[1],p[2])) + (1-y)*log(1-(stats::pnorm(x,p[1],p[2])) ) ))



#gaussian density
gauss <- function(x,mu,sigma)
{
		
	y <- 1/(sqrt(2*pi)*sigma)* exp(-((x - mu)^2/(2* sigma^2)))
	y
}

#gaussian curve 0-1 modulated by pmax and pmin
gauss01 <- function(x,p,prmax=1.0,prmin=0.0)
{
	
	y <-  (p[3]-p[4])*exp(-((x - p[1])^2/(2* p[2]^2))) + p[4]
	y
}

gauss01.lms <- function(p,x,y,weight=nobs)
{
	
	yhat <-  (p[3]-p[4])*exp(-((x - p[1])^2/(2* p[2]^2))) + p[4]
	res <- sum((y-yhat)^2)
    	return(res)
}


gauss01.fit <- function(gauss, vars=NULL,ffunc=gauss01.lms,dfunc=gauss01,ipars=c(1,1,1,1),... )
{
   parname <- c("par1","par2","par3","par4","par5")
  res <- NULL
	
	if(is.null(vars))
		stop("vars is null")
	
  f1 <- match(vars,names(gauss))
  
 	fit <- nlm(ffunc,ipars,x=gauss[,f1[1]],y=gauss[,f1[2]],weight=gauss[,f1[3]],...)$estimate
	
  np <- length(fit)
	res <- data.frame(cbind(t(rep(0,np))))
	names(res) <- parname[1:np]
	for(i in 1:np)
		res[1,i] <- fit[i]

	res	

}


#(1-g)*pnorm(xw,prior,b) + g
cgauss.lms   <- function(p,x,y,weight=1,escala=0.0,k=NULL) { 
#	w <- 1

	w <- weight/sum(weight)
 	yhat<- (1-escala)*pnorm(x,p[1],p[2]) + escala 
	
	res <- sum(((y-yhat)^2)*w)
    	return(res)
}

#cgauss.mle <- function(p,x,y,nobs){
#	-sum( nobs *(y*log(stats::pnorm(x,p[1],p[2])) + (1-y)*log(1-(stats::pnorm(x,p[1],p[2])) ) ))
#}


cgauss.mle <- function(p,x,y,weight,escala=0.0,k=NULL){

	suppressWarnings(p1 <- sum(weight *(y*log((1-escala)*stats::pnorm(x,p[1],p[2]) +escala) + (1-y)*log(1-((1-escala)*stats::pnorm(x,p[1],p[2])+escala ) ) )))
	fr <- weight*y
	p2 <- sum(log(factorial(weight)/(factorial(weight-fr)*factorial(fr))))
	-sum(p1,p2)

}

#cgauss.mle <- function(p,x,y,nobs){
#
#	suppressWarnings(p1 <- sum(nobs *(y*log(stats::pnorm(x,p[1],p[2])) + (1-y)*log(1-(stats::pnorm(x,p[1],p[2])) ) )))
#	fr <- nobs*y
#	p2 <- sum(log(factorial(nobs)/(factorial(nobs-fr)*factorial(fr))))
#	-sum(p1,p2)
#
#}

#plotting function
cgauss <- function(x,p,escala=0.0,k=NULL) {
	
	 ((1-escala)*pnorm(x,p[1],p[2])+escala)
}

cdprime <- function(x,p,escala=0.0,k=NULL)
{
	#p[1] alpha = threshold at 76%
	#p[2] beta = steepness
	y <- pnorm(( (x/p[1])^p[2])/sqrt(2))
	y
}



cdprime.lms <- function(p,x,y,weight=1,escala=0.0,k=NULL)
{

	#p[1] alpha = threshold at 76%
	#p[2] beta = steepness

	w <- weight/sum(weight)

	yhat <- pnorm(( (x/p[1])^p[2])/sqrt(2))
	res <- sum(((y-yhat)^2)*w)
    	return(res)
}

cdprime.mle <- function(p,x,y,weight,escala=0.0,k=NULL){

	p1 <- sum(weight *(y*log(stats::pnorm(( (x/p[1])^p[2])/sqrt(2))) + (1-y)*log(1-(stats::pnorm(( (x/p[1])^p[2])/sqrt(2))) ) ))
	fr <- weight*y
	p2 <- sum(log(factorial(weight)/(factorial(weight-fr)*factorial(fr))))
	-sum(p1,p2)

}


dprime.lms <- function(p,x,y,weight=1,escala=0.0,k=NULL)
{

	#p[1] alpha = threshold at 76%
	#p[2] beta = steepness

	w <- weight/sum(weight)

	yhat <- ((x/p[1])^p[2])/sqrt(2)
	res <- sum(((y-yhat)^2)*w)
    	return(res)
}




dprime <- function(x,p)
{
	y <- (x/p[1])^p[2]
	y

}
	
## END OF GAUSSIAN FIT FUNCTIONS


q_weibull <- function(x,p)
{
#p=beta,gamma,T,epsilon
# example in log units: q_weibull(-0.327,c(3.5/20,0.5,-0.3,-0.9))	
	exponent <- p[1]*(x-p[3]+p[4])
		
	(1-(1-p[2])*exp( -10^(exponent)))
	#(1-(1-0.0)*exp( -10^((p[1]/20)*(x-p[2]+0.0))  )) #fixem params
}


weibull <- function(x,p,k=2)
{

	y <- (1/k)+(1-1/k)* (1-exp(-((x/p[1])^p[2])/sqrt(k)))
#	y <- (1/k)+(1-1/k -p[1])* (1-exp(-((x/p[2])^p[3])/sqrt(k)))
	
	y
}

weibull.lms <- function(p,x,y,k=2,weight=NULL,escala=0.0)
{

		w <- weight/sum(weight)
	
	yhat <- weibull(x,p,k=k)
	res <- sum(((y-yhat)^2)*w)
	return(res)	
}


addfit <- function(x,y,ffit=cgauss.lms,pfit=cgauss,param=c(1,1),...)
{
	x1 <- seq(min(x),max(x),length=100)
	fit <- nlm(ffit,param,x=x,y=y)
	y1 <- pfit(x1,fit$estimate)
	lines(x1,y1,...)
	list(pars=fit$estimate,x=x1,y=y1)
}

data.frame.fit <- function(dfr,vars,groups,ffunc=cgauss.lms,dfunc=cgauss,ipars=c(1,1),...)
{

	parname <- c("par1","par2","par3","par4","par5")
	
	n1 <- match(groups,names(dfr)) 
	M <- length(n1)
	if(M==1)
		grid.ex <- expand.grid(unique(dfr[,n1[1]]))	
	else if(M==2)
		grid.ex <- expand.grid(unique(dfr[,n1[1]]),unique(dfr[,n1[2]]) )
	else
		grid.ex <- expand.grid(unique(dfr[,n1[1]]),unique(dfr[,n1[2]]),unique(dfr[,n1[3]]) )
	

	lp <- length(ipars)
	for(j in 1:lp)
		grid.ex <- cbind(grid.ex,1)
	
	names(grid.ex) <- c(groups,parname[1:lp])

	N <- dim(grid.ex)[1]

	for(i in 1:N){
		if(M==1)
			id <- dfr[,n1]==grid.ex[i,1]
		else if(M==2)
			id <- dfr[,n1[1]]==grid.ex[i,1] & dfr[,n1[2]]==grid.ex[i,2]
		else
			id <- dfr[,n1[1]]==grid.ex[i,1] & dfr[,n1[2]]==grid.ex[i,2] & dfr[,n1[3]]==grid.ex[i,3]
	
		a <- dfr[id,]
		f1 <- match(vars,names(dfr))
		if(is.null(list(...)$nobs)==FALSE){
			nob <- list(...)$nobs[id]
			fit <- nlm(ffunc,ipars,x=a[,f1[1]],y=a[,f1[2]],nobs=nob,...)
		}else
			fit <- nlm(ffunc,ipars,x=a[,f1[1]],y=a[,f1[2]],...)
		grid.ex[i,(M+1):(M+lp)] <- fit$estimate
	}
	grid.ex
}	

df.fit <- function(df,vars=NULL,ffunc=cgauss.lms,dfunc=cgauss,ipars=c(1,1),k=NULL,...) {
	
	parname <- c("par1","par2","par3","par4","par5")
	res <- NULL
	
	if(is.null(vars))
		stop("vars is null")
	
	f1 <- match(vars,names(df))
#print(f1)
	if(length(f1)==2){
		fit <- nlm(ffunc,ipars,x=df[,f1[1]],y=df[,f1[2]],weight=1,k=k,...)$estimate
	#	print(ffunc)
	}else{
#		print(df[,f1[3]])
		fit <- nlm(ffunc,ipars,x=df[,f1[1]],y=df[,f1[2]],weight=df[,f1[3]],k=k,...)$estimate
	}
	np <- length(fit)
	res <- data.frame(cbind(t(rep(0,np))))
	names(res) <- parname[1:np]
	for(i in 1:np)
		res[1,i] <- fit[i]

	res	
}



#Ellipse fit
ellipse.fit <- function(xi,yi,ci=qchisq(0.95,2),nx=200,draw=TRUE,...)
{

	v <- matrix(c(var(xi),cov(xi,yi),cov(xi,yi),var(yi)),ncol=2)
	ctr <- c(mean(xi),mean(yi))
  h <- solve(v)
  eps <- 1e-7
 
  xrad <- sqrt(-h[2,2]*ci/(h[1,2]^2-h[1,1]*h[2,2]))-eps  # x range
  x1 <- seq(-xrad,xrad,length=round(nx/2))
  det <- sqrt((h[1,2]^2-h[1,1]*h[2,2])*x1^2+h[2,2]*ci)/h[2,2]
  # calculate "top" and "bottom" halves (plus/minus solutions to quadratic)
  y1 <- -h[1,2]/h[2,2]*x1 + det 
  x2 <- rev(x1)
  y2 <- -h[1,2]/h[2,2]*x2 - det
  # translate ellipse to center
  x <- c(x1,x2)+ctr[1]
  y <- c(y1,y2)+ctr[2]
  x <- c(x,x[1])   # complete ellipse
  y <- c(y,y[1])
  if (draw){
    plot(xi,yi,xlab="X",ylab="Y",...)
    lines(x,y)
	}	
	list(x=x,y=y,xc=ctr[1],yc=ctr[2],xsd=sd(xi),ysd=sd(yi),cov=v)
}


# 
# pieron.fit <- function(Pieron, vars=NULL,ffunc=pieron.lms,dfunc=pieron,ipars=c(1,1,1),k=NULL,... )
# {
#    parname <- c("par1","par2","par3","par4","par5")
# 	res <- NULL
# 	
# 	if(is.null(vars))
# 		stop("vars is null")
# 	
#   f1 <- match(vars,names(Pieron))
#   
#  	fit <- nlm(ffunc,ipars,x=Pieron[,f1[1]],y=Pieron[,f1[2]],weight=[,f1[3]],k=k,...)$estimate
# 	
#   np <- length(fit)
# 	res <- data.frame(cbind(t(rep(0,np))))
# 	names(res) <- parname[1:np]
# 	for(i in 1:np)
# 		res[1,i] <- fit[i]
# 
# 	res	
# 
# }
# 


pieron.optim.fit <- function(Pieron, vars=NULL,ffunc=pieron.lms,ipars=c(1,1,1),dfunc=pieron,k=NULL,... )
{
   parname <- c("par1","par2","par3","par4","par5")
	res <- NULL
	
	if(is.null(vars))
		stop("vars is null")
	
  f1 <- match(vars,names(Pieron))
  
 	fit <- optim(ipars,ffunc,x=Pieron[,f1[1]],y=Pieron[,f1[2]],weight=1/Pieron[,f1[3]],k=k,...)$par
  fit <- optim(fit,ffunc,x=Pieron[,f1[1]],y=Pieron[,f1[2]],weight=1/Pieron[,f1[3]],k=k,...)$par          #pasamos los primeros parametros estimados otra vez a optim, utilizamos 1/weight para ponderar en función del error de cada punto

	 
  np <- length(fit)
	res <- data.frame(cbind(t(rep(0,np))))
	names(res) <- parname[1:np]
	for(i in 1:np)
		res[1,i] <- fit[i]

	res	

}

ddprime.fit <- function(Dprime, vars=NULL,ffunc=cdprime.mle,dfunc=dprime,ipars=c(1,1),k=NULL,.. )
{
   parname <- c("par1","par2","par3","par4","par5")
	res <- NULL
	
	if(is.null(vars))
		stop("vars is null")
	
  f1 <- match(vars,names(Dprime))
  
 	fit <- nlm(ffunc,ipars,x=Dprime[,f1[1]],y=Dprime[,f1[2]],weight=Dprime$nobs,..)$estimate
	
  np <- length(fit)
	res <- data.frame(cbind(t(rep(0,np))))
	names(res) <- parname[1:np]
	for(i in 1:np)
		res[1,i] <- fit[i]

	res	

}




