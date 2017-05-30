#x <- seq(1,10,len=9)
#y <- pnorm(x,5,2.5)
#y <- y + runif(9,-0.03,0.03)
#d1 <- data.frame(a=x,b=y,c=30)
#y <- pnorm(x,5,2.5)
#y <- y + runif(9,-0.03,0.03)
#d2 <- data.frame(a=x,b=y,c=30)


# pf_boot
# funcio generica per bootstrap parametric
# autor: Joan LM

cbooth <- function(data1,pfn=power.lms,ipars=NULL,...)
{

	names(data1) <- c("x","y")
	out <- nlm(pfn,ipars,hessian=TRUE,x=data1$x,y=data1$y,...)
	print(sqrt(diag(2*out$minimum/(length(data1$y) - 2) * solve(out$hessian))))
	print(out$estimate)
	
}


cboot <- function(data1,data2=NULL,pfn=powerf.lms,ipars=NULL,predfn=powerf,runs=999,ci=0.95,...)
{
	#pfn <- match.arg(pfn)
	
	if(is.data.frame(data1)==FALSE){
		stop("data1 should be a data frame with 3 numeric columns")
	}

	if (is.null(ipars)){ # comparing two 
		stop("Error in arg ipars: vector of initial params should be supplied")
	}

	
	names(data1) <- c("x","y","dy")
	
	require(boot,quietly=TRUE)

	if(is.null(data2)){
		fun.boot <- function(bdata,...){ # bootstrap function
			res <- nlm(pfn,ipars,x=bdata$x,y=bdata$y)	
			c(res$estimate)
		}
		fake.rg <- function(bdata,mle){ # simulation function   
			out <- bdata
#			  out$y <- rbinom(nrow(bdata),out$n,mle)
#			  out$y <- out$y/out$n
			out$y <- rnorm(nrow(bdata),out$y,out$dy)
			out
		 }
		
		fit1 <-  fun.boot(data1) 

		pred1 <- predfn(data1$x,fit1)
		p.boot<-boot(data1,fun.boot,R=runs,sim="parametric",ran.gen=fake.rg,mle=pred1)
		
		res <- list()
		res$param <- p.boot$t0
		npar <- len(p.boot$t0)
		param.ci<-matrix(0,nrow=2,ncol=npar)	
		
		for(i in 1:npar)
			param.ci[,i]<-norm.ci(boot.out = p.boot, conf = ci, index = i)[2:3]
	
		res$ci <- param.ci
		res
	}
	else{
		names(data2) <- c("x","y","dy")
			
		fun.boot <- function(bdata){ # bootstrap function
			res1 <- nlm(pfn,ipars,x=bdata$d1.x,y=bdata$d1.y)	
			res2 <- nlm(pfn,ipars,x=bdata$d2.x,y=bdata$d2.y)
			c(res1$estimate - res2$estimate)
		}	
		fake.rg <- function(bdata,mle){    # simulation function
			out <- bdata
			out$d1.y <-rnorm(nrow(bdata),out$d1.y,out$d1.dy)
			#out$d1.y <- out$d1.y/out$d1.n
			out$d2.y <- rnorm(nrow(bdata),out$d2.y,out$d2.dy)
			#out$d2.y <- out$d2.y/out$d2.n			
			out
		 }
	
		#comb<-rbind(cbind(data1$x,data1$y),cbind(data2$x,data2$y))
		comb <- rbind(data1,data2)
		#comb<-data.frame(comb)
		comb$x <- as.numeric(comb$x) 
		comb$y <- as.numeric(comb$y)
		comb$n <- as.numeric(comb$n)

		fitcomb <-  nlm(pfn,ipars,x=comb$x,y=comb$y,nobs=comb$n)$estimate

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
