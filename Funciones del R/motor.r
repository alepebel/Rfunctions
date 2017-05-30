 ## Motor control analysis Functions ##
 

 
tdata.stats <- function(t,x,y,z=rep(0,length(t)),vthres=0.5,freq=10.0,rm.frame=7,pbwidth=17,vbwidth=15,abwidth=9,draw=FALSE,jerk=FALSE,bwFilter=FALSE,add=FALSE,col=1,...)
 {
 # get trajectory stats
	 
#	t <- bwf(t,ff=freq);
	t1 <- t[-1] # resize dim
	if(bwFilter==TRUE){
		bwx <- bwfilter(x,freq=30)
		bwy <- bwfilter(y,freq=30)
		bwz <- bwfilter(z,freq=30)
		vel <- vel.in(t,bwx$trend,bwy$trend,bwz$trend)
 		 	#acceleration profile
 		acc <- diff(vel)
 		bwacc <- acc
	} else{

		bwx <- filter(x,filter=rep(1/pbwidth,pbwidth))
		bwy <- filter(y,filter=rep(1/pbwidth,pbwidth))
		bwz <- filter(z,filter=rep(1/pbwidth,pbwidth))
		vel <- vel.in(t,bwx,bwy,bwz)
 		vel <- filter(vel,filter=rep(1/vbwidth,vbwidth)) # 9 per vels alta
 		acc <- diff(vel)
 		bwacc <- filter(acc,filter=rep(1/abwidth,abwidth))	
 		
	}
#	te1 <- Sys.time() 	
 	rtime <- t1[which(vel>vthres)[1]]
 	seg <- 0
 	vmax <- 0
 	tzero2 <- 0
 	t2 <- t1[-1] # resize dim

# 	id <- which(t2 > rtime+addtime & bwacc < athres & bwacc > -athres)
# 	id2 <- which(diff(t1[id])>addtime)+1 	
# 	tzero1 <- c(mean((t1[id])[1:(id2[1]-1)]),(t1[id])[id2])
	id <- which(diff(sign(bwacc))!=0) 
	id2 <- which(diff(id)<rm.frame)+1
	id <- id[-id2]

	tzero1 <- t2[id] 
 	tzero1 <- tzero1[which(tzero1>rtime)]
 #	id <- which(diff(v1)<addtime) +1
# 	id <- id[-len(id)]
# 	tzero1 <- tzero1[-id]

 	lz <- len(tzero1)
#print(tzero1)
 	if(lz!=0){
		#& vel[id] > vthres*10
		seg <- len(tzero1[seq(1,len(tzero1),2)])
		id <- match(t1,tzero1[seq(1,len(tzero1),2)],nomatch=0) > 0	
		vmax <- vel[id]		
	}
	#print(Sys.time()-te1)
	if(jerk==TRUE & lz!=0){
		jk <- diff(bwacc)
		t3 <- t2[-1]
		fjk <- filter(jk,filter=rep(1/10,10))

#		id <- which(t3 > rtime+addtime & fjk < jthres & fjk > -jthres)
#		id2 <- which(diff(t2[id])>addtime)+1 
#		tzero2 <- c(mean((t2[id])[1:(id2[1]-1)]),(t2[id])[id2])

		id <- diff(sign(fjk))!=0
		tzero2 <- t3[id]
		tzero2 <- tzero2[which(tzero2>rtime)]
		id2 <- diff(tzero2) > 0.03
		tzero2 <- tzero2[id2]
	}
	
 	if(draw==TRUE){
 		if(add==FALSE)
 	 		plot(t1,vel,t='l',col=col,...)
 	 	else
 	 		lines(t1,vel,col=col,...)
 		lines(t2,bwacc,col=col+1)
 		abline(h=0.0,lty=2)
 		abline(v=tzero1,lty=3,col=col)
 		abline(v=tzero2,lty=4,col="gray")
 		abline(v=rtime,lty=1,col=col+2)
 	}
 	
 	(list(nsegs=seg,rtime=rtime,vmax=vmax,t2peakv=tzero1[1]-rtime,zerocross_acc=tzero1,zerocross_jerk=tzero2))
 
 }

#intern
vel.in <- function(t,x,y=rep(0,length(t)),z=rep(0,length(t))){
 		dxdt <- diff(x)/diff(t)
 		dydt <- diff(y)/diff(t)
 		dzdt <- diff(z)/diff(t)
 		(vel <- sqrt(dxdt^2+dydt^2+dzdt^2))
 }

larger.th <- function(x,th,yr=0.1){	
	id <- x>th & c(0,diff(x)) >1.0 & x < th + (th*yr) 
	id <- id & c(0,diff(id))==1
	id
}

itime <- function(th,x,deltay=0.08,increase=1,span=2){
	
	dx <- c(0,diff(x))
	if(increase==1)
		id <- which(x>th & x < th+(th*deltay) & dx > 0.0 )
	else{
		id <- which(x<th & x > th-(th*deltay) & dx < 0.0 )
		span=10 
	}
			id2 <- which(diff(id)>span) 
	#c(id[1],id[id2])	
	id[id2] 
	}

#optimal as it calls .C 
 tdata.vel <- function(t,x,y,freq=10.0)
 {
 		t <- bwf(t,ff=5.0);
 		bwx <- bwf(x,ff=freq)
		bwy <- bwf(y,ff=freq)
 		dxdt <- diff(bwx)/diff(t)
 		dydt <- diff(bwy)/diff(t) 
 		(vel <- sqrt(dxdt^2+dydt^2))
 }
 
# tdata2.vel <- function(t,x,y)
# {
#	if(is.loaded('_deriv1')==FALSE)
#		dyn.load('~/R/c_interface/c_utils.so')
#			o <- rep(0,length(x)-1)
#	.C("_deriv1",as.integer(length(t)),as.double(t),as.double(x),as.double(y),v=double(length(o)),NAOK=TRUE)$v
#
# }



#use this instead of tablet2screen 
tdata.scoord <- function(x,y) 
{
	if(is.loaded('_screencoord')==FALSE)
		dyn.load('~/R/c_interface/motor.so')

	o <- rep(0,length(x))
	a <- .C("_screencoord",as.integer(length(x)),as.double(x),as.double(y),sx=double(length(o)),sy=double(length(o)))
	list(x=a$sx,y=a$sy)
}

#tablet2screenXY <- function(x,y)
#{
#    
#    xmin <- 0.5 *(tablet_x[3]+tablet_x[4]);
#    ymin <- 0.5*(tablet_y[2]+tablet_y[3]);
#    
#    xrange <- 0.5*(tablet_x[1]+tablet_x[2]) - xmin;    
#    yrange <- 0.5*(tablet_y[1]+tablet_y[4]) - ymin;
#    
#    sx = -(-(SCREEN_TABLET_W/2) + ((x - xmin) /xrange)*SCREEN_TABLET_W);
#    sy = -(SCREEN_TABLET_H/2) + ((y - ymin) /yrange)*SCREEN_TABLET_H;
# 	
# 	list(x=sx,y=sy) 
#}

#seltrial <- function(dat,trial){
#	id <- which(dat$trial==trial)
#	(dat[id,])
#
#}

tdata.frame <- function(filename,subj=1,con=1)
{
	zz <- file(filename, "rb")
	ntrials <- readBin(zz, integer(), 1)
	dd <- data.frame(temps=NULL,x=NULL,y=NULL,subj=NULL,con=NULL)
	dd <- NULL;
	for(i in 1:ntrials){
		nsamples <- readBin(zz, integer(), 1)
		a<-readBin(zz, numeric(),n=nsamples, size=4)
		x<-readBin(zz, numeric(),n=nsamples, size=4)
		y<-readBin(zz, numeric(),n=nsamples, size=4)
		dd <- rbind(dd,data.frame(temps=a,x=x,y=y,trial=i,subj=subj,con=con))
	}
	close(zz)
	(dd)
}

tdata.open <- function(filename)
{
	(file(filename, "rb"))
}
#
#
tdata.info <- function(conn)
{

	isOpen(conn, "r")

	seek(conn, 0, rw="r") # to start
	ntrials <- readBin(conn, integer(), 1)
	
	nsamples <- rep(0,ntrials)
	for(i in 1:ntrials){
		nsamples[i] <- readBin(conn, integer(), 1)
		seek(conn,nsamples[i]*3*4,origin="current")
	}
	nsamples
}
#
tdata.trial <- function(conn,trial,tinfo=NULL)
{
	isOpen(conn, "r")

	if(is.null(tinfo))
		stop("tinfo not provided")
	if(trial==1)
		offs <- 4+4
	else
		offs <- sum((tinfo[1:trial-1]*3*4))+((trial)*4)+4
	seek(conn, 0, rw="r") # to start
	seek(conn, offs, rw="r") # to start
	temps<-readBin(conn, numeric(),n=tinfo[trial], size=4)
	x<-readBin(conn, numeric(),n=tinfo[trial], size=4)
	y<-readBin(conn, numeric(),n=tinfo[trial], size=4)	
	data.frame(temps=temps,x=x,y=y)
	
}

tdata.ave <- function(conn,skip=NULL,tinfo=NULL,tmin=0, tmax=2.0,freq=200)
{
	isOpen(conn, "r")

	if(is.null(tinfo))
		stop("tinfo not provided")
	n <- 1
	tz <- seq(tmin,tmax,1/freq)
	tx <- rep(0,len(tz)) 
	ty <- rep(0,len(tz)) 
	for(trial in 1:len(tinfo)){
		if((trial %in% skip)==FALSE){
			dd <- tdata.trial(conn,trial,tinfo)
			tx <- tx + quantile(dd$x,seq(0,1,length=length(tz)))
			ty <- ty + quantile(dd$y,seq(0,1,length=length(tz)))
			n <- n+1
		}
	}
	(dd <- data.frame(temps=tz,x=tx/n,y=ty/n))

}

aveT <- function(sdata,skip=NULL,tmin=0, tmax=2.0,freq=200)
{
	tz <- seq(tmin,tmax,1/freq)
	
	if(!is.null(skip)){
		sdata <- sdata[-skip,]
	}
	
	tindex <- findInterval(sdata$temps,tz)
	sdata$temps2 <- tz[tindex]
	#sdata$tindex <- tindex
	#a <- frameApply(sdata,by="temps2",on=c("x","y"),mean)
	a <- ag(list(x=sdata$x,y=sdata$y),list(sdata$temps2),mean)
	
	(data.frame(tnorm=tz,x=a$x,y=a$y))
}

#tdata.open <- function(fname)
#{
#	if(is.loaded('_bopen')==FALSE)
#		dyn.load('~/R/c_interface/motor.so')
#	n <- 0
#	.C("_bopen",as.character(fname),out=as.integer(n))$out
#}
#
#tdata.close <- function()
#{
#	if(is.loaded('_bclose')==FALSE)
#		dyn.load('~/R/c_interface/motor.so')
#	n <- 1
#	.C("_bclose",out=as.integer(n))$out
#}
#tdata.info <- function(ntrials)
#{
#	if(is.loaded('_tdata_info')==FALSE)
#		dyn.load('~/R/c_interface/motor.so')
#	o <- rep(0,ntrials)
#	.C("_tdata_info",as.integer(ntrials),out=integer(length(o)))$out
#}
#
#tdata.trial <- function(ntrial,info)
#{
#	if(ntrial>length(info))
#		stop("ntrial too large")
#	if(is.loaded('_tdata_trial')==FALSE)
#		dyn.load('~/R/c_interface/motor.so')
#	o <- rep(0,info[ntrial])
#	
#	as.data.frame(.C("_tdata_trial",as.integer(ntrial),as.integer(info),temps=single(length(o)),x=single(length(o)),y=single(length(o)))[3:5])
#
#	#(data.frame(temps=out1,x=out2,y=out3))
#}

#_bwfilter(float *signaal, float ff, float sf, int start, int stop, int on, float *out )
bwf <- function(input,ff=5.0,sf=200,on=4,from=0,to=length(input)-1,naok=TRUE){

	if(is.loaded('_bwfilter')==FALSE)
		dyn.load('D:/Funciones del R/bwfilter.so')

	o <- rep(0,length(input))
	as.double(.C("_bwfilter",as.single(input),as.single(ff),as.single(sf),as.integer(from),as.integer(to),as.integer(on),out=single(length(o)),NAOK=naok)$out)
#	dyn.unload('/Users/joan1/R/c_interface/bwfilter.so')
#	y
}


tablet_x <- c(45.076,45.2,4.3,4.4,24.492001)
tablet_y <- c(34.430,8.63,8.24,34.1,21.577999)
SCREEN_TABLET_W <- (52.0)
SCREEN_TABLET_H <- (33.0)

