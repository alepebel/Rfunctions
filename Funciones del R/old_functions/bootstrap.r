x <- seq(1,10,len=9)
y <- pnorm(x,5,2.5)
y <- y + runif(9,-0.03,0.03)
d1 <- data.frame(a=x,b=y,c=30)
#y <- pnorm(x,5,2.5)
#y <- y + runif(9,-0.03,0.03)
#d2 <- data.frame(a=x,b=y,c=30)


# pf_boot
# funcio generica per bootstrap parametric
# autor: Joan LM
pfboot <- function(data1,data2=NULL,pfn=cgauss.lms,ipars=NULL,predfn=cgauss,runs=999,ci=0.95,sim="parametric",...)
{
	#pfn <- match.arg(pfn)
	
	if(is.data.frame(data1)==FALSE){
		stop("data1 should be a data frame with 3 numeric columns")
	}

	if (is.null(ipars)){ # comparing two 
		stop("Error in arg ipars: vector of initial params should be supplied")
	}

	
	names(data1) <- c("x","y","n")
	
	require(boot,quietly=TRUE)

	if(is.null(data2)){
		fun.boot <- function(bdata,...){ # bootstrap function
			res <- nlm(pfn,ipars,x=bdata$x,y=bdata$y,weight=bdata$n)	
			c(res$estimate)
		}
		
		fake.rg <- function(bdata,mle){ # simulation function   
			out <- bdata
			  out$y <- rbinom(nrow(bdata),out$n,mle)
			  out$y <- out$y/out$n
			  out
		 }
		 
		 
		fit1 <-  fun.boot(data1) 
		pred1 <- predfn(data1$x,fit1)		
		res <- list()
		npar <- length(ipars)
		param.ci<-matrix(0,nrow=2,ncol=npar)	
		if(sim=="parametric"){
			p.boot<-boot(data1,fun.boot,R=runs,sim=sim,ran.gen=fake.rg,mle=pred1)
			res$param <- p.boot$t0
			for(i in 1:npar){
				param.ci[,i]<-norm.ci(boot.out = p.boot, conf = ci, index = i)[2:3]	
			}
			res$ci <- param.ci	
		}
		else{
			res$param <- fun.boot(data1)
			p1<-matrix(0,nrow=runs,ncol=npar)
			d1 <- data1
			for(i in 1:runs){
				d1$y <- resample(data1)	
				p1[i,]<-fun.boot(d1)				
			}
			for(i in 1:npar){
				param.ci[,i] <- quantile(p1[,i],c(0.05,0.95))
			}			
			res$ci <- param.ci
		}
		res
	}
	else{
		names(data2) <- c("x","y","n")
			
		fun.boot <- function(bdata){ # bootstrap function
			res1 <- nlm(pfn,ipars,x=bdata$d1.x,y=bdata$d1.y)	
			res2 <- nlm(pfn,ipars,x=bdata$d2.x,y=bdata$d2.y)
			c(res1$estimate - res2$estimate)
		}	
		fake.rg <- function(bdata,mle){    # simulation function
			out <- bdata
			out$d1.y <- rbinom(nrow(bdata),out$d1.n,mle[,1])
			out$d1.y <- out$d1.y/out$d1.n
			out$d2.y <- rbinom(nrow(bdata),out$d2.n,mle[,2])
			out$d2.y <- out$d2.y/out$d2.n			
			out
		 }
	
		#comb<-rbind(cbind(data1$x,data1$y),cbind(data2$x,data2$y))
		comb <- rbind(data1,data2)
		#comb<-data.frame(comb)
		comb$x <- as.numeric(comb$x) 
		comb$y <- as.numeric(comb$y)
		comb$n <- as.numeric(comb$n)

		fitcomb <-  nlm(pfn,ipars,x=comb$x,y=comb$y,weight=comb$n)$estimate

		pred1 <- predfn(data1$x,fitcomb)
		pred2 <- predfn(data2$x,fitcomb)

		pred1i2<-cbind(pred1,pred2)
		comb<-data.frame(d1=data1,d2=data2)
		p.boot<-boot(comb,fun.boot,R=runs,sim="parametric",ran.gen=fake.rg,mle=pred1i2)
		require(MASS)
		kde2d(p.boot$t[,1],p.boot$t[,2],n=100)->z
		image(z,xlab="diff(pse)",ylab="diff(slope)",main="Bivariate distribution of differences",col = terrain.colors(100))
		points(p.boot$t0[1],p.boot$t0[2],pch=15,col="red")
		res <- list()
		res$param <- p.boot$t0
		p.value<-c(0,0)

		ecdf.pse<-ecdf(p.boot$t[,1])
		ecdf.slp<-ecdf(p.boot$t[,2])
		p.value[1]<-ecdf.pse(p.boot$t0[1])
		p.value[1]<-min(p.value[1],1-p.value[1])*2
		
		p.value[2]<-ecdf.slp(p.boot$t0[2])
		p.value[2]<-min(p.value[2],1-p.value[2])*2
		res$p.value <- p.value
	} # end of else
	
	res
}

boot.df <- function(dfr,ind=c(1,2,3),...)
{
	parname <- c("par1","par1inf","par1sup","par2","par2inf","par2sup","par3","par3inf","par3sup","par4","par4inf","par4sup")

	res <- NULL

	fit <- pfboot(data1=dfr[,ind],data2=NULL,...)	
	
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

boot.help <- function()
{
	print("ddply(dframe,.(cond,subj),boot.df,ind=c(1,2,3),pfn=pfun.lms,ipars=c(1,1,1),predfn=predfun)")
	print("pfboot <- function(data1,data2=NULL,pfn=cgauss.lms,ipars=NULL,predfn=cgauss,runs=999,ci=0.95,sim='ordinary',...)")
}

resample <- function(dfr)
{
	n <- dfr$y*dfr$n
	n1 <- rep(0,length(n))
	for(i in 1:length(n)){
		x <- rep(c(0,1),c(dfr$n[i]-n[i],n[i])) # 0,1 array
		n1[i] <- mean(sample(x,dfr$n[i],replace=T))
	}
	n1
}


