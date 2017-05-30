#test set                    #/ nS, nFa, sS, nY 
                             #c(correct rejections(00),false alarms(01), misses(10), hits(11))
                             
sdt.dat <- data.frame(obs=c(167,33,119,81,145,55,79,121,114,86,73,127,85,115,50,150),cond=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4) )
nh <- c(81,121,127,150)
ns <- c(200,200,200,200)
nf <- c(33,55,86,115)
nn <- c(200,200,200,200)

sdt <- function(obs,pred=NULL,condition=rep(1,length(obs)),roc=FALSE,alternative="great",type.obs="binary",add=FALSE,...)
{
 
#if pred is null obs is a contingency table
# c(correct rejections(00),false alarms(01), misses(10), hits(11))

	getstats <- function(x){
		#x=correct rejections,fa,misses,hits 
		total0 <- x[1] + x[2] 
		total1 <- x[3] + x[4]
#		if(x[2]==0)
#			pfa <- 1/(2*total0)
#		else if(x[2]==total0)
#			pfa <- 1 - 1/(2*total0)
#		else
#			pfa <- x[2]/total0 
#		if(x[4]==0)
#			phit <- 1/(2*total1)
#		else if (x[4]==total1)
#			phit <- 1 - 1/(2*total1)
#		else
#			phit <-  x[4]/total1
		h_f <- get_hits_fa(x[4],total1,x[2],total0)
		dprime <- qnorm(h_f$h) - qnorm(h_f$f)
		criterium <- -(qnorm(h_f$h) + qnorm(h_f$f))/2.0
		(list(nh=x[4],ns=total1,nf=x[2],nn=total0,h=h_f$h,f=h_f$f,dprime=dprime,c=criterium))
	}
	make_tables <- function(x){
		list(a=sum(x==1),b=sum(x==2),c=sum(x==3),d=sum(x==4)) 
	}


	if(is.null(pred)==FALSE & type.obs=="binary"){ 	#individual observations are passed
		ct <- rep(0,length(obs))
		ct[obs==0 & pred==0] <- 1
		ct[obs==1 & pred==0] <- 2
		ct[obs==0 & pred==1] <- 3
		ct[obs==1 & pred==1] <- 4
		a <- unlist(tapply(ct,condition,make_tables))
		names(a) <- NULL
		o1 <- a
		c1 <- rep(unique(condition),each=4)
	}else if(is.null(pred) & type.obs=="binary"){   # contingency table is passed as argument through response
		o1 <- obs
		c1 <- condition
	}else{ # type.obs = "continuous"
		if(is.null(condition) | length(condition)==length(obs))
			stop("The condition parametre should be a criterium value or a vector of a length smaller than the length of obs")
		if(length(unique(pred))!=2){
			stop("For continuous observations two distributions are required. Vector pred should have two unique values: (0 noise; 1 signal)")
		}
		signal <- as.integer(pred!=0)
		ct <- matrix(0,length(obs),length(condition))
		for(b in 1:length(condition)){
			o1 <- as.integer(obs > condition[b])
			ct[o1==0 & pred==0,b] <- 1
			ct[o1==1 & pred==0,b] <- 2
			ct[o1==0 & pred==1,b] <- 3
			ct[o1==1 & pred==1,b] <- 4			
		}
		dim(ct) <- NULL
		c1 <- rep(seq(1,length(condition)),each=length(obs))
		a <- unlist(tapply(ct,c1,make_tables))
		names(a) <- NULL
		c1 <- rep(seq(1,length(condition)),each=4)
		o1 <- a

	}	

	if(length(o1)%%4!=0 & is.null(pred)){
		stop("Response should be a contingency table of length 4 or multiple of 4 if pred is null")
	}

	ra <- NA
	params <- NA
	W <- NA
	p.v <- NA
	a <- unlist(tapply(o1,c1,getstats))	
	dim(a) <- c(8,length(a)/8)
	a <- t(a)
	a <- as.data.frame(a)
	names(a) <- c("nh","ns","nf","nn","h","f","dprime","c")
	if(roc){
		res <- roc(a$nh,a$ns,a$nf,a$nn,alternative=alternative)
		ra <- res$Az
		p.v <- res$p.value
		W <- res$W
		params <- res$params
		if(add==FALSE)
    plot(a$f,a$h,xlim=c(0,1),ylim=c(0,1),xlab="prob FA",ylab="prob Hit",...)  #			plot(qnorm(a$f),qnorm(a$h),xlim=c(-1,1),ylim=c(-1,1),xlab="prob FA",ylab="prob Hit",...)      
		else
			points(a$f,a$h,...)
		lines(c(0,1),c(0,1),lty=2)
		
		zf <- seq(-5,5,len=100)
		zh <- params[1]*zf+params[2]
		lines(pnorm(zf),pnorm(zh),...)
		
	}
			
	return(list(frequencies=a[,1:4],stats=a[,5:8],Az=ra,W=W,p.value=p.v,params=params))

}

#Estimated variance of the Z(prob) following the error-propagation theory.

roc <-function(nh,ns,nf,nn,alternative="great")
{
	varhatz.std <- function(x,N)
	{
		p <- x/N
		p[x==0] <- 1/(2*N[x==0])
		p[x==N] <- 1/(2*N[x==N])

		varp <- (p*(1-p))/N
		(varp/dnorm(qnorm(p))^2)
	}
	rocfit <- function(p,zh,zf,varzh,varzf)
	{
	
	# S = sum((yi-a-b*xi).^2/(stdy^2 + b^2*stdx^2))
		yhat <- p[1]*zf + p[2]
		res <- sum( (yhat-zh)^2 /(varzh^2 + p[1]^2*varzf^2)) #varzh i varzf son sd, no var
		res
	}

#	n <- length(nh)
#	f <- rep(0,n)
#	h <- rep(0,n)
#	for(i in 1:n){	
#		hf <- get_hits_fa(nh[i],ns[i],nf[i],nn[i])	
#		h[i] <- hf$h
#		f[i] <- hf$f
#	}
	hf <- get_hits_fa(nh,ns,nf,nn)
	h <- hf$h
	f <- hf$f

	vh <- varhatz.std(nh,ns)
	vf <- varhatz.std(nf,nn)

	
	fit<-optim(c(1,1),rocfit,zh=qnorm(h),zf=qnorm(f),varzh=vh,varzf=vf)$par
	
#	fit1 <- coefficients(lsfit(qnorm(f),qnorm(h)))
	a<- pnorm(fit[2]/sqrt(1+fit[1]*fit[1]))
	names(a) <- NULL

	wt <- wilcox.test(h,f,alternative=alternative)
	w <- wt$statistic
	names(w) <- NULL
	(list(Az=a,params=fit,W=w,p.value=wt$p.value))
}

get_hits_fa <- function(nh,ns,nf,nn)
{
	
	h <- nh/ns
	f <- nf/nn
	
	h[nh==0] <- 1/(2*ns[nh==0])
	h[nh==ns] <- 1- 1/(2*ns[nh==ns])

	f[nf==0] <- 1/(2*nn[nf==0])
	f[nf==nn] <- 1- 1/(2*nn[nf==nn])
	
#	if(nf==0)
#		f <- 1/(2*nn)
#	else if(nf==nn)
#		f <- 1 - 1/(2*nn)
#	else
#		f <- nf/nn
#		
#	if(nh==0)
#		h <- 1/(2*ns)
#	else if(nh==ns)
#		h <- 1 - 1/(2*ns)
#	else
#		h <-  nh/ns
	list(h=h,f=f)
	
}

#if(binormal){
#  dat  <- as.data.frame( DAT[,,j] )
#  names(dat) <- c("thres", "proby", "probn", "zH", "zF")
#  dat <- dat[is.finite(dat$zH) & is.finite(dat$zF), ] ## reduce dat, get rid of nans and inf   
#  new <-  as.data.frame( matrix(qnorm(seq(0.005, 0.995, 0.005 ) ), ncol = 1) )
#  names(new) <- "zF"
#  A <- lm(zH ~ zF, data = dat)$fitted.values
#  B <- predict(lm(zH ~ zF, data = dat), newdata = new)
#
#binormal.pltpts[[j]]<- data.frame( t = new$zF, x = pnorm(new$zF), y = pnorm(B) )
#binormal.area  <- sum(0.005*pnorm(B) , na.rm = TRUE) } else {binormal.area <- NA}
#}
#



