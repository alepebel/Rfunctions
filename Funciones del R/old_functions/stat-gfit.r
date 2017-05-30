StatFit <- proto(Stat, {
  
  calculate <- function(., data, scales, n=101, args = list(), ...) {
    range <- scales$x$output_set()
    xseq <- seq(range[1], range[2], length=n)
    gfit <- function(dfr,xseq,fun,dfun,ipars,...)
	{

		fit <- nlm(fun,ipars,x=dfr$x,y=dfr$y,weight=dfr$weight,...)$estimate
		
		print(fit)
#		write(fit,file="~/projects/_lastfitparams.log",append=TRUE)
		y <- dfun(xseq,fit,...)
		y
	}
   
    if (is.null(data$weight)) data$weight <- 1
      
    data.frame(
      x = xseq,
      y = do.call(gfit, c(list(dfr=data),list(xseq), args))
    )
  }


  objname <- "fit" 
  desc <- "Fit a function "

  desc_params <- list(
    fun = "function to use",
    n = "number of points to interpolate along",
    args = "list of additional arguments to pass to fun"
  )
  
  desc_outputs <- list(
    x = "x's along a grid",
    y = "value of function evaluated at corresponding x"
  )

  default_geom <- function(.) GeomPath
  
  examples <- function(.) {
    x <- rnorm(100)
    qplot(x, geom="density") + stat_gfit(fun = dnorm, colour="red")
  }
  
})
