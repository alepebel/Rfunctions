#PAPER'S PLOTS

#CANVI DE DIRECTORI
#setwd("/Users/joan1/projects/borja")


#def("motor")
#def("ellipses")

panel.ng <- function(...)
{
      x <- list(...)$x
    y <- list(...)$y

	  	fit<-lm( y ~x)
	    panel.abline(fit,col="darkgreen")
	   	print(coefficients(fit))
	    print(confint(fit))

    panel.xyplot(...)
}


panel.g <- function(...)
{

  col <- rep(trellis.par.get()$superpose.symbol$col,3)
   g <- sort(unique(list(...)$groups[list(...)$subscripts]))
    sym <- rep(trellis.par.get()$superpose.symbol$pch,3)

    x <- list(...)$x
    y <- list(...)$y

	pn <- panel.number()
    for(i in 1:length(g)){
	    id <- which(list(...)$groups[list(...)$subscripts]==g[i])
	  	fit<-lm( y[id] ~x[id])
	    panel.abline(fit,col=col[i])
	   	print(coefficients(fit))
	    print(confint(fit))
    }
    panel.superpose(...)
}


panel.ellipse <- function(...)
{

  col <- rep(trellis.par.get()$superpose.symbol$col,3)
   g <- sort(unique(list(...)$groups[list(...)$subscripts]))
  pch <- rep(trellis.par.get()$superpose.symbol$pch,3)
  fill <- rep(trellis.par.get()$superpose.symbol$fill,3)
    x <- list(...)$x
    y <- list(...)$y

	xc <- rep(0,length(g))
	yc <- rep(0,length(g))
	pn <- panel.number()
    for(i in 1:length(g)){
	    id <- which(list(...)$groups[list(...)$subscripts]==g[i])
	  	fit<-ellipse.fit(x[id] ,y[id],draw=FALSE )
	    panel.lines(fit$x,fit$y,col=col[i])
	   	print(c(fit$xc,fit$yc))
	   	xc[i] <- fit$xc
	   	yc[i] <- fit$yc
    }
    panel.superpose(...)
    panel.points(xc,yc,col="black",pch=pch,fill="black",cex=2.0)
}


panel.ng <- function(...)
{
      x <- list(...)$x
    y <- list(...)$y

	  	fit<-lm( y ~x)
	    panel.abline(fit,col="darkgreen")
	   	print(coefficients(fit))
	    print(confint(fit))

    panel.xyplot(...)
}


panel.g <- function(...)
{
  col <- c("red","darkgreen","blue","black")
    g <- sort(unique(list(...)$groups[list(...)$subscripts]))

    x <- list(...)$x
    y <- list(...)$y

	pn <- panel.number()
    for(i in 1:length(g)){
	    id <- which(list(...)$groups[list(...)$subscripts]==g[i])
	  	fit<-lm( y[id] ~x[id])
	    panel.abline(fit,col=col[i])
	   	print(coefficients(fit))
	    print(confint(fit))
    }
    panel.superpose(...)
}

panel.ellipse <- function(...)
{

  col <- rep(trellis.par.get()$superpose.symbol$col,3)
   g <- sort(unique(list(...)$groups[list(...)$subscripts]))
  pch <- rep(trellis.par.get()$superpose.symbol$pch,3)
  fill <- rep(trellis.par.get()$superpose.symbol$fill,3)
    x <- list(...)$x
    y <- list(...)$y

	xc <- rep(0,length(g))
	yc <- rep(0,length(g))
	pn <- panel.number()
    for(i in 1:length(g)){
	    id <- which(list(...)$groups[list(...)$subscripts]==g[i])
	  	fit<-ellipse.fit(x[id] ,y[id],draw=FALSE )
	    panel.lines(fit$x,fit$y,col=col[i])
	   	print(c(fit$xc,fit$yc))
	   	xc[i] <- fit$xc
	   	yc[i] <- fit$yc
    }
    panel.superpose(...)
  #  panel.points(xc,yc,col="black",pch=pch,fill="black",cex=2.0)
    panel.abline(h=0,lty=2)
	panel.abline(v=0,lty=2)
}



panel.ellipse2 <- function(...)
{

  col <- rep(trellis.par.get()$superpose.symbol$col,3)
   g <- sort(unique(list(...)$groups[list(...)$subscripts]))
  pch <- rep(trellis.par.get()$superpose.symbol$pch,3)
  fill <- rep(trellis.par.get()$superpose.symbol$fill,3)
    x <- list(...)$x
    y <- list(...)$y

	xc <- rep(0,length(g))
	yc <- rep(0,length(g))
	pn <- panel.number()
    for(i in 1:length(g)){
	    id <- which(list(...)$groups[list(...)$subscripts]==g[i])
	  	fit<-ellipse.fit(x[id] ,y[id],draw=FALSE )
	    panel.lines(fit$x,fit$y,col=col[i])
	   	print(c(fit$xc,fit$yc))
	   	xc[i] <- fit$xc
	   	yc[i] <- fit$yc
    }
    #panel.superpose(...)
    #panel.points(xc,yc,col="black",pch=pch,fill="black",cex=2.0)
}

getstatsby<-function(df,varnames,dv="Response",fun=mean){

	if(nargs()==0){
		print("getstatsby(df,varnames,dv='Response',fun=mean)")
		print("fun usually is mean(default), sd, or len")
		return("Author: Joan LM");
	}

    suppressWarnings(aux<-aggregate(df,as.list(subset(df,select=varnames)),fun,na.rm=TRUE))

	names(aux)[1:length(varnames)]<-varnames
	aux<-subset(aux,select=c(varnames,dv))
    aux
}

toNum<-function(fac)
{
	if(nargs()==0){
		print("toNum(factor)")
		return("Author: Joan LM")
	}


    as.numeric(fac)->id
   (as.numeric(levels(fac)[id]))

}

len<-function(x,...)
{
    length(x)
}


logis.op <- function(p,x,y) {


  ypred <- 1.0 / (1.0 + exp((p[1] - x) / p[2]));


	res <- sum((y-ypred)^2)
    	return(res)
}

cgauss.op <- function(p,x,y) {

 	ypred<-pnorm(x,p[1],p[2])

	res <- sum((y-ypred)^2)
    	return(res)
}




#load("data.rda")
#load("posx_.rda")
#load("posy_.rda")

require(lattice)




