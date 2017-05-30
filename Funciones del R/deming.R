x se.x y se.y
871 192 735 207
701 156 792 223
328 76 340 096 
560 126 544 153
155 39 207 59 
175 43 229 65 
73 22 66 19
366 84 343 97
90 25 125 36
939 207 658 185
439 100 331 93
369 84 272 77
34 13 232 66
194 47 150 43
207 50 350 99
138 36 117 33
181 45 231 66
127 33 188 54
82 23 44 13
188 46 137 40
566 127 704 198
0 06 0 1
0 6 49 15
40 15 129 37
0 6 37 12
198 48 216 62
1021 224 1253 351
464 105 390 110
566 127 466 131
1925 418 1586 445
\name{deming}
\alias{deming}
\title{Fit a generalized Deming regression}
\description{
 Find the MLE line relating x and y when both are measured with
error.  When the variances are constant and equal, this is the
special case of Deming regression.
For laboratory analytes this is rarely true, however.
}
\usage{
ripleyfit(x, y, xstd, ystd, jackknife = TRUE, dfbeta = FALSE, scale=T)
}
\arguments{
  \item{x}{A numeric vector}
  \item{y}{A numeric vector}
  \item{xstd}{Either a numeric vector of the same length as \code{x}
giving the standard error for each of the elements, or a vector of
length 2 giving the error formula.}
  \item{ystd}{Either a numeric vector of the same length as \code{y}
giving the standard error for each of the elements, or a vector of
length 2 giving the error formula.}
  \item{jackknife}{Produce jackknife estimates of standard error.}
  \item{dfbeta}{Return the dfbeta matrix}
  \item{scale}{Compute an estimate of residual variance or scale.  
  If FALSE, the estimates of variance \code{xstd} and \code{ystd} are
  assumed to be perfectly calibrated.}
}
\details{
The \code{xstd} specification can be a pair of
values a, b; if so then the standard deviation of \code{x} is assumed
to be \code{a + b*x}; similarly for \code{ystd}.
An assumption of constant variance (homoscedasticity) correponds to \code{b=0}.
If \code{b} is 0 for both \code{x} and \code{y}, 
then the result depends only on the 
ratio of the \code{a} values, which is the ratio of the variances.
To fit a Deming regression for instance use \code{c(1,0)} for 
both specifications.  (Use of (k,0) for both would give the same answer
for any value k).

When \code{a} is zero this is a model assuming constant coefficient of
variation.  
Values of stdx= (0,k) and stdy = (0,1) correspond to the case of contant
proportional errors discussed by Linnet.

The most realistic case is where both \code{a} and \code{b} are
non-zero and have been estimated from prior data.
}
\value{
  If \code{jackknife} is FALSE the result is a vector containing the
intercept and the slope, otherwise it is a list with components:
  \item{coefficient}{The coefficient vector, containing the intercept and 
slope.}
  \item{variance}{The jackknife estimate of variance}
  \item{dfbeta}{Optionally, the dfbeta residuals.  A 2 column matrix,
each row is the change in the coefficient vector if that observation is
removed from the data.}
}
\details{
  The standard printout includes test of intercept=0 and of slope=1.}
\references{
 BD Ripley and M Thompson, Regression techniques for the detection
of analytical bias, Analyst 112:377-383, 1987.

 K Linnet, Estimation of the linear relationship between the
measurements of two methods with proportional errors.  
Statistics in Medicine 9:1463-1473, 1990.
}
\author{Terry Therneau}
\examples{
# Data from Ripley
arsenic <- data.frame( 
      x=c(871, 701, 328, 560, 155, 175, 73, 366, 90, 939, 439, 369, 34, 194, 
          207, 138, 181, 127, 82, 188, 566, 0, 0, 40, 0, 198, 1021, 464, 566, 
          1925)/100,
      y=c(735, 792, 340, 544, 207, 229, 66, 343, 125, 658, 331, 272, 232, 150, 
          350, 117, 231, 188, 44, 137, 704, 0, 49, 129, 37, 216, 1253, 390, 
          466, 1586)/100,
      se.x=c(192, 156, 76, 126, 39, 43, 22, 84, 25, 207, 100, 84, 13, 47, 
              50, 36, 45, 33, 23, 46, 127, 6, 6, 15, 6, 48, 224, 105, 127, 418)
              /100,
      se.y=c(207, 223, 96, 153, 59, 65, 19, 97, 36, 185, 93, 77, 66, 43, 99, 
             33, 66, 54, 13, 40, 198, 1, 15, 37, 12, 62, 351, 110, 131, 445)/
             100)

fit <- deming(arsenic$x, arsenic$y, arsenic$se.x, arsenic$se.y, dfbeta=T)
print(fit)
\dontrun{
               Coef  se(coef)           z         p
Intercept 0.1064478 0.2477054  0.54552512 0.3101551
Slope     0.9729999 0.1429776 -0.07341776 0.3874562

   Scale= 1.358379 
   }
plot(1:30, fit$dfbeta[,1]) #subject 22 has a major effect on the fit

# Standard Deming regression.  The plot is not at all horizontal, which shows
#  that this is an inappropriate model, however
plot(arsenic$x, arsenic$se.x)
fit <- deming(arsenic$x, arsenic$y, xstd=c(1,0), ystd=c(1,0))
 }
}
\keyword{regression}

# Generalized Deming regression, based on Ripley, Analyst, 1987:377-383.
#
deming <- function(x, y, xstd, ystd, jackknife=TRUE, dfbeta=FALSE,
                   scale=TRUE) {
    Call <- match.call()
    n <- length(x)
    if (length(y) !=n) stop("x and y must be the same length")
    if (length(xstd) != length(ystd)) 
        stop("xstd and ystd must be the same length") 

    # Do missing value processing
    nafun <- get(options()$na.action)
    if (length(xstd)==n) {
        tdata <- nafun(data.frame(x=x, y=y, xstd=xstd, ystd=ystd))
        x <- tdata$x
        y <- tdata$y
        xstd <- tdata$xstd
        ystd <- tdata$ystd
        }
    else {
        tdata <- nafun(data.frame(x=x, y=y))
        x <- tdata$x
        y <- tdata$y
        if (length(xstd) !=2) stop("Wrong length for std specification")
        xstd <- xstd[1] + xstd[2]*x
        ystd <- ystd[1] + ystd[2] * y
        }

    if (any(xstd <=0) || any(ystd <=0)) stop("Std must be positive")

    minfun <- function(beta, x, y, xv, yv) {
        w <- 1/(yv + beta^2*xv)
        alphahat <- sum(w * (y - beta*x))/ sum(w)
        sum(w*(y-(alphahat + beta*x))^2)
        }

    minfun0 <- function(beta, x, y, xv, yv) {
        w <- 1/(yv + beta^2*xv)
        alphahat <- 0  #constrain to zero
        sum(w*(y-(alphahat + beta*x))^2)
        }

    afun <-function(beta, x, y, xv, yv) {
        w <- 1/(yv + beta^2*xv)
        sum(w * (y - beta*x))/ sum(w)
        }

    fit <- optimize(minfun, c(.1, 10), x=x, y=y, xv=xstd^2, yv=ystd^2)
    coef = c(intercept=afun(fit$minimum, x, y, xstd^2, ystd^2), 
               slope=fit$minimum)
    fit0 <- optimize(minfun0, coef[2]*c(.5, 1.5), x=x, y=y, 
                     xv=xstd^2, yv=ystd^2)

    w <- 1/(ystd^2 + (coef[2]*xstd)^2) #weights
    u <- w*(ystd^2*x + xstd^2*coef[2]*(y-coef[1])) #imputed "true" value
    if (is.logical(scale) && scale) {
        err1 <- (x-u)/ xstd
        err2 <- (y - (coef[1] + coef[2]*u))/ystd
        sigma <- sum(err1^2 + err2^2)/(n-2)
        # Ripley's paper has err = [y - (a + b*x)] * sqrt(w); gives the same SS
        }
    else sigma <- scale^2
    
    test1 <- (coef[2] -1)*sqrt(sum(w *(x-u)^2)/sigma) #test for beta=1
    test2 <- coef[1]*sqrt(sum(w*x^2)/sum(w*(x-u)^2) /sigma) #test for a=0
                      
    rlist <- list(coefficient=coef, test1=test1, test0=test2, scale=sigma,
                  err1=err1, err2=err2, u=u)

    if (jackknife) {
        delta <- matrix(0., nrow=n, ncol=2)
        for (i in 1:n) {
            fit <- optimize(minfun, c(.5, 1.5)*coef[2], 
                            x=x[-i], y=y[-i], xv=xstd[-i]^2, yv=ystd[-i]^2)
            ahat <- afun(fit$minimum, x[-i], y[-i], xstd[-i]^2, ystd[-i]^2)
            delta[i,] <- coef - c(ahat, fit$minimum)
            }
        rlist$variance <- t(delta) %*% delta
        if (dfbeta) rlist$dfbeta <- delta
        }

    rlist$call <- Call
    class(rlist) <- 'deming'
    rlist
    }

print.deming <- function(x, ...) {
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    if (is.null(x$variance)) {
        table <- matrix(0., nrow=2, ncol=3)
        table[,1] <- x$coefficient
        table[,2] <- c(x$test0, x$test1)
        table[,3] <- pnorm(-2*abs(table[,2]))
        dimnames(table) <- list(c("Intercept", "Slope"),
                                c("Coef", "z", "p"))
        }
    else {
        table <- matrix(0., nrow=2, ncol=4)
        table[,1] <- x$coefficient
        table[,2] <- sqrt(diag(x$variance))
        table[,3] <- c(x$test0, x$test1)
        table[,4] <- pnorm(-2*abs(table[,3]))
        dimnames(table) <- list(c("Intercept", "Slope"),
                                c("Coef", "se(coef)", "z", "p"))
        }
    print(table, ...)
    cat("\n   Scale=", format(x$scale, ...), "\n")
    invisible(x)
    }

