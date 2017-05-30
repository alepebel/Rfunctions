#home <-"/Users/joan1"

# exp(seq(log(from), log(to), length.out = length.out))
# 
# exp(seq(log(100), log(200), length.out = 5))

#clear <- rm(list=ls())
  
  

revers <- function(x)
{
	as.logical(1-x)
}

colMedians <- function(x,...)
{
	(apply(x,2,median,...))
}

rowMedians <- function(x,...)
{
	(apply(x,1,median,...))
}

colMeans2 <- function(x,...)
{
	(apply(x,2,mean,...))
}

rowMeans2 <- function(x,...)
{
	(apply(x,1,mean,...))
}

colSdev <- function(x,...)
{
	(apply(x,2,sd,...))
}

rowSdev <- function(x,...)
{
	(apply(x,1,sd,...))
}

sortfiles <- function(fnames)
{
	n <- length(fnames)
	ii <- sort(as.character(seq(1,n)),index.return=T)
	(fnames[sort(ii$ix,index.return=T)$ix])
}

findex <- function(f)
{
	if(is.factor(f)==FALSE)
		stop("f not a factor")
	lb <- levels(f)
	match(f,lb)
}


binvar <- function(x,n=10,breaks=NULL)
{
#	prob <- seq(0.0,1.0,len=n+1)	
#	vec <- quantile(x,probs=seq(0.0,1.0,len=n),type=2)
#	names(vec) <- NULL

#algo 1
#	h <- hist(x,breaks=n,plot=FALSE)
#	id <- findInterval(x,h$breaks)
#	h$mids[id]
#algo 2	
	cen <- stats.bin(x,x,N=n,breaks=breaks)$centers
	(cen[closer(cen,x)])
}

ag <- function(dat=xd,...)
{
	attach(dat)
	res<-try(x <- aggregate(...))
	detach(dat)
	if(is(res,"try-error")==FALSE)
		(x)
}

xq <- function(width=5,height=5,...){
  #x11(":0.0",width=width,height=height)
  quartz(width=width,height=height,...)
}

med <- function(x,na.rm=FALSE)
{

	y <- sort(x[[1]])	# to get it atomic
	l <- length(x)
	if((l %% 2)==0){
		return (mean(c(y[l/2],y[l/2+1]),na.rm=na.rm))	
		}else{
		return (y[ceiling(l/2)])
		}
}

flip <- function(x)
{
	(x[seq(length(x),1,by=-1)])
}


#s<-function(f) source(f)

toDeg <- function(x)
{
	(x*180/pi)
}

toRad <- function(x) 
{
	(x*pi/180)
}

norm01 <- function(x)
{
    minx <- min(min(x))
    maxx<-max(max(x))
    
    xrange<-maxx-minx
    
    y<-(x-minx)/xrange
    y
}

dirR <- function()
{
	dir(path="/Users/joan1/R")
}

last <- function(x)
{
	(x[length(x)])
}

topdf <- function(pl,width=5,height=5,color="nosolid")
  {
    pdf(file="quartz.pdf",width=width,height=height)
    if(color=="nosolid")
      nosolid()
    else if(color=="incolor")
      incolor()
      else
      	bw()
    print(pl)
    dev.off()
    
  }

def <- function(func,dir=home)
{
	source(file.path(dir,"R",paste(func,".R",sep="")))				
}

extend_data_frame <- function(x,y,df1)
{
	nr <- nrow(df1)
	l <- length(x)
	if(l<nr)
		stop("incrementa longitud de x")
	if(l>nr){
		for(i in 1:(l-nr)){
			df1 <- rbind(df1,df1[1,])
		}
	}
	df1$x <- x
	df1$y <- y
	(df1)
}


len<-function(x,...)
{
    length(x)
}



sem <- function(x) 
{
	sqrt( var(x,na.rm=TRUE) / len(x) )
}

# 
# f2n <- function(fac)
# {
# 	if(nargs()==0){
# 		print("f2n(factor)")
# 		return("(c) Joan LM")
# 	}
#     as.numeric(fac)->id
# (as.numeric(levels(fac)[id]))
# 
# }
# 
# 
# ff <- function(fname)
# {
# 
# 	(system(paste("cat /Users/joan1/R/*.R | /Users/joan1/R/c_interface/findRfunc ",fname,">kk")))
# 	source("kk")
# 	a <- system("cat kk")
# 	system("rm kk")
# 	a
# }
# 
# ul <- function(lib='/Users/joan1/R/c_interface/motor.so')
# {
# 	dyn.unload(lib)
# }
# 
# 
# closer <- function(x,tomatch)
# {
# 	ind <- rep(0,length(tomatch))
# 
# 	for(i in 1:length(tomatch)){
# 		ind[i] <- which.min(abs(x-tomatch[i]))[1]
# 	}
# 	ind
# }    


            
dfilter <- function(dfr)   #para filtrar RT en varias condiciones simult?neas
{
	m <- mean(dfr$RT)
	sds <- sd(dfr$RT)
	Ub <- m+3*sds
	Lb <- m-3*sds
	SFR <- subset(dfr, RT>Lb & RT<Ub)
	SFR
	}

corr.chisq <- function(m,corrected=0)
{
	if(nargs()==0){
		a <- matrix(c(2,12,6,2),nr=2)
		colnames(a) <- c("v","av")
		rownames(a) <- c("+-","-+")
		print(a)
		print(corr.chisq(a))
	}

	csum <- colSums(m)
	if(csum[1]< 7 | csum[2] < 7)
		corrected=1/ncol(m)
	
	ch <- sum(((abs(m[1,]-colSums(m)/2)-corrected)^2) / (colSums(m)/2))
	
	list(chisq=ch,p=1-pchisq(ch,1))

}
 

reciprobit <- function(dd)
{
  
  e1 <- ecdf(dd$RT)
  p1 <- e1(dd$RT)
  qp1 <- qnorm(p1)	
  (data.frame(rRT=-1/dd$RT,qp=qp1))
}     


ecdfs <- function(dd)   #transforms the data to the empirical distribution function
{
  
  e1 <- ecdf(dd$RT)
  p1 <- e1(dd$RT)
  qp1 <-p1 	
  (data.frame(rRT=dd$RT,qp=qp1))
}


probit.par <- function(probitfit) # This function outputs the intercept and slope of the psychometric GLM fit
{
  
  p <- (as.numeric(coef(probitfit)))
  list(mean=p[1]/(-p[2]),sd=1/-p[2])
  
}



S_Kmethod <- function(independent_variable, proportion_of_responces){
  
  # % Calculates the mean of the distribution of responses obtained for example in a 2AFC task. It 
  # % makes a weighted sum of the independent values (proportion of responses).
  # %NOTE: the range of the independent variable (i.e., asynchronies) to be
  # %tested has to be large enough so that the proportion of responses reaches
  # %0 at the two extremes
  # %Usage:
  #   % mean_cumulative=mean_data_s(independent_variable,proportion_of_responces,number_of_responces);
  # %  independent_variable:
  #   %  proportion_of_responces:are two vectors with the same number of elements
  # %  mean_cumulative:         is the estimate of the mean of the underlying distribution
  # %
  # independent_variable <- c(240,-200, -160, -120,  -80,  -40, 0,  40,  80, 120, 160, 200, 240);
  # proportion_of_responces<- c(   0  ,  0 ,   0  , .1 ,  .5  , .7 ,  .8,  .6,  .4,  .2,  .1  , 0  , 0 );
  # % plot(independent_variable, proportion_of_responces)
  
  # % x=-200:1:200;y=normpdf(x,25,44);plot(x,y);[mu,sigma]=mean_data_s(x,y)
  # %
  # % Created by Massimiliano Di Luca m.diluca@bham.ac.uk, Birmingham 17/05/2012 from mean_data which is used for cumulative psychometric functions 
  #Adapted to R by Alexis Perez Bellido 10/01/2015
  
  matrix = as.data.frame(cbind(independent_variable , proportion_of_responces ))
  
  sorted_matrix =  matrix[order(matrix$independent_variable ),]
  
  #normalizes the probability (area under the graph) to one
  proportion_of_responces=proportion_of_responces/sum(proportion_of_responces);
  
  #weight each independent_variable at half point for the corresponding difference in responces
  
  mu=sum(proportion_of_responces*independent_variable);
  #plot(independent_variable_middle-mean_cumulative,prop)
  
  #sigma=sqrt(sum(proportion_of_responces*(independent_variable-mu)^2));
  sigma=sqrt(sum(proportion_of_responces*(independent_variable-mu)^2));
  c(mu , sigma)
  
}

binner <- function(data,bin_size)   #para binnear varias condiciones simult?neas (alexis perez, 2016) generates a vector with the labels binned
{
  bin_breaks = seq(floor(min(data)),ceiling(max(data)),by = bin_size)
  bins <- NULL
  
  for (bin in 1 : ceiling(max(bin_breaks))){
    bins[data>= bin_breaks[bin] & data < bin_breaks[bin+1]] <- bin_breaks[bin]    
  }
  bins
  # binned_vector <- binner(vector2bin,1) # attach vector to dataframe
}


# To generate within-subject SE
# (see http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper functions)


## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}
## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

