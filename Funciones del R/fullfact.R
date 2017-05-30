
fullfact<-function(lev){

	n<-length(lev);
	 
	 
	
	if (any(lev==0)==TRUE){
	  stop('The input values must be integers greater than one.')
	}
	 
	 
	
	ssize = prod(lev);
	ncycles = ssize;
	cols = n;
	 
	#design = zeros(ssize,cols);
	 
	design<-matrix(0,nrow=ssize,ncol=cols) 
	 
	 
	for(k in 1:cols){
	 #set1 in c
	  settings <- 1:lev[k]
	#  if(k==2) print(settings)
	  ncycles <- ncycles / lev[k]
	  nreps <- ssize / (ncycles*lev[k])
	  # set2 in c
	  settings<-rep(settings,1,each=nreps)
	#    if(k==2) print(settings)
	  dim(settings)<-c(length(settings),1)
	  settings <- rep(settings,ncycles)
#	    if(k==2) print(settings)
	  dim(settings)<-c(length(settings),1)  
	  design[,k] <- settings;
	}
	design

}
