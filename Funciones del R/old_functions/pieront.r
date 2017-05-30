
pieron.lms <- function(p,x,y,weight=1,k=NULL)
{
# to fit the y = b*x^(-a) + T	
	
# p1: b
# p2: a
# p3: T

	yhat <- p[1]*x^(-p[2])+p[3] 
	 res <- 	sum((y-yhat)^2 / weight^2)
	return(res)
} 

#to plot
pieron <- function(x,p,k=NULL){
	
	(p[1]*x^(-p[2])+p[3])
} 

pieron2.lms <- function(p,x,y,weight=1,k=NULL)
{
# to fit the y = b*x^(-0.5) + T	
	
# p1: b
# p2: a
# p3: T

	yhat <- p[1]*x^(-p[2]) 
	res <- 	sum((y-yhat)^2 / weight^2 )
	return(res)
} 

pieron2 <- function(x,p){
	
	(p[1]*x^(-p[2]))
} 


powerf.lms <- function(p,x,y,k=1,weight=0.5)
{
	yhat <- p[1]*x^(k)+p[2] 

	   res <- 	sum((y-yhat)^2 / weight^2)
	return(res)
}

powerf <- function(x,p,k=1)
{
	(p[1]*x^(k)+p[2])
}


pieron.fit <- function(Pieron, vars=NULL,ffunc=pieron.lms,dfunc=pieron,ipars=c(1,1,1),k=NULL,... )
{
   parname <- c("par1","par2","par3","par4","par5")
	res <- NULL
	
	if(is.null(vars))
		stop("vars is null")
	
  f1 <- match(vars,names(Pieron))
  
 	fit <- nlm(ffunc,ipars,x=Pieron[,f1[1]],y=Pieron[,f1[2]],weight=1,k=k,...)$estimate
	
  np <- length(fit)
	res <- data.frame(cbind(t(rep(0,np))))
	names(res) <- parname[1:np]
	for(i in 1:np)
		res[1,i] <- fit[i]

	res	
}


pieron.optim.fit <- function(Pieron, vars=NULL,ffunc=pieron.lms,ipars=c(1,1,1),dfunc=pieron,k=NULL,... )
{
   parname <- c("par1","par2","par3","par4","par5")
	res <- NULL
	
	if(is.null(vars))
		stop("vars is null")
	
  f1 <- match(vars,names(Pieron))
  
 	fit <- optim(ipars,ffunc,x=Pieron[,f1[1]],y=Pieron[,f1[2]],weight=1/Pieron[,f1[3]],k=k,...)$par
  fit <- optim(fit,ffunc,x=Pieron[,f1[1]],y=Pieron[,f1[2]],weight=1/Pieron[,f1[3]],k=k,hessian=TRUE,...)          #pasamos los primeros parametros estimados otra vez a optim, utilizamos 1/weight para ponderar en función del error de cada punto
#  ste <- sqrt(diag(3*fit$value/(length(6) - 3) * solve(fit$hessian)))
  
  np <- length(fit$par)
	res <- data.frame(cbind(t(rep(0,np))))
	names(res) <- parname[1:np]
	for(i in 1:(np))
		res[1,i] <- fit$par[i]
# 	for(i in 4:(np*2))
#		res[1,i] <- ste[i]


	res	

}



powerf.fit <- function(Pieron, vars=NULL,ffunc=powerf.lms,dfunc=powerf,ipars=c(1,1),k=-1,...)
{
   parname <- c("par1","par2","par3","par4","par5")
	res <- NULL
	
	if(is.null(vars))
		stop("vars is null")
	
  f1 <- match(vars,names(Pieron))
  
 	fit <- nlm(ffunc,ipars,x=Pieron[,f1[1]],y=Pieron[,f1[2]],weight=1,k=-1,...)$estimate  #estimate 
	
  np <- length(fit)
	res <- data.frame(cbind(t(rep(0,np))))
	names(res) <- parname[1:np]
	for(i in 1:np)
		res[1,i] <- fit[i]

	res	

}


linealF.fit <- function(Lineal, vars=NULL,ffunc=linealf.lms,dfunc=linealf,ipars=c(1,1),k=1.0,...)
{
   parname <- c("par1","par2","par3","par4","par5")
	res <- NULL
	
	if(is.null(vars))
		stop("vars is null")
	
  f1 <- match(vars,names(Lineal))
  
 	fit <- nlm(ffunc,ipars,x=(Lineal[,f1[1]]),y=(Lineal[,f1[2]]), hessian = TRUE,weight=1,k=1.0,...)$estimate    
	
  np <- length(fit)
	res <- data.frame(cbind(t(rep(0,np))))
	names(res) <- parname[1:np]
	for(i in 1:np)
		res[1,i] <- fit[i]
    
	res	

}



linealf <- function(x,p,k=0){
	
	((k)*x +p[1])
} 

linealf.lms <- function(p,x,y,k=1,weight=0)
{
	yhat <- ((k)*x +p[1]) 

	   res <- 	sum((y-yhat)^2 / weight^2)
	return(res)
}




lineal <- function(x,p,k=NULL){
	
	(p[2]*(x) +p[1])
} 

lineal.lms <- function(p,x,y,k=NULL,weight=0)
{
	yhat <- (p[2]*(x) +p[1]) 

	   res <- 	sum((y-yhat)^2 / weight^2)
	return(res)
}



lineal.fit <- function(Lineal, vars=NULL,ffunc=lineal.lms,dfunc=lineal,ipars=c(1,1),...)
{
   parname <- c("par1","par2","par3","par4","par5")
	res <- NULL
	
	if(is.null(vars))
		stop("vars is null")
	
  f1 <- match(vars,names(Lineal))
  
 	fit <- nlm(ffunc,ipars,x=log(Lineal[,f1[1]]),y=log(Lineal[,f1[2]]),weight=Lineal[,f1[3]],...)$estimate    # puntuaciones colocadas en escala logaritmica
	
  np <- length(fit)
	res <- data.frame(cbind(t(rep(0,np))))
	names(res) <- parname[1:np]
	for(i in 1:np)
		res[1,i] <- fit[i]

	res	

}



linealfb.lms <- function(p,x,y,k=1,weight=0)
{
	yhat <- ((p[1])*x +(k)) 

	   res <- 	sum((y-yhat)^2 / weight^2)
	return(res)
}


linealfb.fit <- function(Lineal, vars=NULL,ffunc=linealfb.lms,dfunc=lineal,ipars=c(1,1),...)
{
   parname <- c("par1","par2","par3","par4","par5")
	res <- NULL
	
	if(is.null(vars))
		stop("vars is null")
	
  f1 <- match(vars,names(Lineal))
  
 	fit <- nlm(ffunc,ipars,x=(Lineal[,f1[1]]),y=(Lineal[,f1[2]]),weight=Lineal[,f1[3]],...)$estimate    # puntuaciones colocadas en escala logaritmica
	
  np <- length(fit)
	res <- data.frame(cbind(t(rep(0,np))))
	names(res) <- parname[1:np]
	for(i in 1:np)
		res[1,i] <- fit[i]

	res	

}

orthoreg <- function(p,y,x,sdy,sdx)
	{
	
	
		yhat <- p[1]*x + p[2]

	# yhat <- x 
		
		res <- sum( (yhat-y)^2 /(sdy^2 + p[1]^2*sdx^2))
		res
		
	}
	
	
cboot.fit <- function(Lineal, vars=NULL,ffunc=pieron.lms,dfunc=pieron,ipars=c(1,1,1),...)
{
   parname <- c("par1","par2","par3","par4","par5","par6","par7","par8","par9")
	res <- NULL

	if(is.null(vars))
		stop("vars is null")

  f1 <- match(vars,names(Lineal))

pars <- nlm(ffunc,ipars,x=Lineal[,f1[1]],y=Lineal[,f1[2]],weight=Lineal[,f1[3]])
pars <- nlm(ffunc,c( pars$estimate[1], pars$estimate[2], pars$estimate[3]),x=Lineal[,f1[1]],y=Lineal[,f1[2]],weight=Lineal[,f1[3]])

#  fit <- cboot( Lineal[,f1[1]],Lineal[,f1[2]], Lineal[,f1[3]],pfn=ffunc, ipars=pars$estimate,predfn=dfunc)
  fit <- cboot( Lineal[,f1],pfn=ffunc, ipars=pars$estimate,predfn=dfunc)
	np <- length(fit$param)

	res <- data.frame(cbind(t(rep(0,(np*3)))))

	names(res) <- parname[1:(np*3)]
	n <- 1
	for(i in seq(1,(np*3),by=3)){

		res[1,i:(i+2)] <- c(fit$param[n],fit$ci[,n])
		n <- n+1
	}
	res


}



stepfun.lms <- function(p,x,y,weight=1,sf=2.5)
{                         

  w <- weight/sum(weight)
  ym <- mean(y)
  
  yhat <- y
  yhat[x<sf] <- ym-(p/2)
  yhat[x>sf] <- ym+(p/2)
   res <- sum(((y-yhat)^2)*w)
        return(res)
}

stepfun.fit <- function(Stepfun, vars=NULL,ffunc=stepfun.lms,dfunc=stepdrawfun,ipars=c(1),k=NULL,. )
{
   parname <- c("par1","par2","par3","par4","par5")
	res <- NULL
	
	if(is.null(vars))
		stop("vars is null")
	
  f1 <- match(vars,names(Stepfun))
  
 	fit <- nlm(ffunc,ipars,x=Stepfun[,f1[1]],y=Stepfun[,f1[2]],weight=Stepfun$nobs,..)$estimate
	
  np <- length(fit)
	res <- data.frame(cbind(t(rep(0,np))))
	names(res) <- parname[1:np]
	for(i in 1:np)
		res[1,i] <- fit[i]

	res	

}

 stepdrawfun <- function(p,x,y,sf=2.5)
{
  ym <- mean(y)
  
  yhat <- y
  yhat[x<sf] <- ym-(p/2)
  yhat[x>sf] <- ym+(p/2)
  yhat
}



	

  
lmode.fit <- function(Lineal, vars=NULL)
{
   parname <- c("par1","par2","par3","par4","par5")
	res <- NULL
	
	if(is.null(vars))
		stop("vars is null")
	
  f1 <- match(vars,names(Lineal))
  
 	fit <- summary(lm(log(Lineal[,f1[1]])~log(Lineal[,f1[2]])))$coefficients[2]  # puntuaciones colocadas en escala logaritmica
 	 
  np <- length(fit)                     
	res <- data.frame(cbind(t(rep(0,np))))  
	names(res) <- parname[1:np]
	for(i in 1:np)
	res[1,i] <- fit[i]

	res	

}


	
# 
# lmodes.fit <- function(Lineal, vars=NULL)
# {
#    parname <- c("par1","par2","par3","par4","par5")
# 	res <- NULL
# 	
# 	if(is.null(vars))
# 		stop("vars is null")
# 	
#   f1 <- match(vars,names(Lineal))
#   
#   fits <- summary(lm(Lineal[,f1[2]]~1/Lineal[,f1[1]])$coefficients[2]
#   fit <- summary(lm(Lineal[,f1[2]]~(1/Lineal[,f1[1]]))$coefficients[4]# puntuaciones colocadas en escala logaritmica
#  	fit2 <- cbind(A=fits-1.96*fit, B=fits+1.96*fit)                  
# 	res <- data.frame(cbind(fits,fit2))  
# 	res	
# 
# }
	