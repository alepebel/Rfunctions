
#matrix(c(var(x),cov(x,y),cov(x,y),var(y)),ncol=2)

ellipse.fit <- function(xi,yi,ci=qchisq(0.95,2),nx=200,draw=TRUE,...)
{

	v <- matrix(c(var(xi),cov(xi,yi),cov(xi,yi),var(yi)),ncol=2)
	ctr <- c(mean(xi),mean(yi))
  h <- solve(v)
  eps <- 1e-7
 
  xrad <- sqrt(-h[2,2]*ci/(h[1,2]^2-h[1,1]*h[2,2]))-eps  # x range
  x1 <- seq(-xrad,xrad,length=round(nx/2))
  det <- sqrt((h[1,2]^2-h[1,1]*h[2,2])*x1^2+h[2,2]*ci)/h[2,2]
  # calculate "top" and "bottom" halves (plus/minus solutions to quadratic)
  y1 <- -h[1,2]/h[2,2]*x1 + det 
  x2 <- rev(x1)
  y2 <- -h[1,2]/h[2,2]*x2 - det
  # translate ellipse to center
  x <- c(x1,x2)+ctr[1]
  y <- c(y1,y2)+ctr[2]
  x <- c(x,x[1])   # complete ellipse
  y <- c(y,y[1])
  if (draw){
    plot(xi,yi,xlab="X",ylab="Y",...)
    lines(x,y)
	}	
	list(x=x,y=y,xc=ctr[1],yc=ctr[2],xsd=sd(xi),ysd=sd(yi),cov=v)
}


# Here are two ellipse() functions I ve written in the past, probably very
#inefficient but they might give you something to start with.  One takes
#the center, semiminor and major axes, and rotation, the other takes a
#variance-covariance matrix (it was designed for confidence ellipses using
#the likelihood ratio test).
#   Hope they give you a start.


ellipse <- function(x=1,y=1,r1=1,r2=1,ang=0,narc=200,...)
{
  rmat <- c(cos(ang),-sin(ang),sin(ang),cos(ang))
  calcpt <- function(x,y,r1,r2,ang1) {
    r <- rmat %*% c(r1*cos(ang1),r2*sin(ang1))
    c(x+r[1],y+r[2])
    }
  arcs <- matrix(sapply(seq(0,2*pi,len=narc),
                       function(q)calcpt(x,y,r1,r2,q)),byrow=TRUE,ncol=2)
  lines(arcs,...)
}


vcellipse <- function(ctr,varcov,correlation=NULL,
                      critval=qchisq(0.95,2),nx=200,
                    fill=FALSE,...) {
  # draw the ellipse defined by the center ctr, and either a
  # variance vector c(sigma^2(x),sigma^2(y)) and a correlation,
  # OR a variance-covariance matrix, plus an optional critical value.
  # nx is the number of separate arcs used to draw the ellipse
  # plot solution for y of equation: h11*x^2+2*h12*x*y+h22*y^2=critval
  #
  # reconstruct hessian matrix
  if (!is.null(correlation) && (correlation>1 || correlation<(-1)))
    stop(paste("Correlation=",correlation," is out of bounds: parameter mixup?"))
  if (!is.null(correlation) && is.vector(varcov)) {
    v <- matrix(nrow=2,ncol=2)
    v[1,1] <- varcov[1]
    v[2,2] <- varcov[2]
    v[1,2] <- correlation*sqrt(v[1,1]*v[2,2])
    v[2,1] <- v[1,2]
  } else if (is.matrix(varcov)) {
    v <- varcov
  }
  h <- solve(v)
  eps <- 1e-7
  ##  xrad  <- sqrt(solve(hess)[1,1]*critval)-eps
  xrad <- sqrt(-h[2,2]*critval/(h[1,2]^2-h[1,1]*h[2,2]))-eps  # x range
  ##  segments(ctr[1],ctr[2],ctr[1]+xrad,ctr[2],...)
  ## det >= 0
  ## (h12^2-h11*h22)*x1^2 > -h22*critval
  ## x1^2 > -h22*critval/(h12^2-h11*h22)
  ## x1   > sqrt(-h22*critval/(h12^2-h11*h22)),"\n")
  ## same as inverting the matrix!
  x1 <- seq(-xrad,xrad,length=round(nx/2))
  det <- sqrt((h[1,2]^2-h[1,1]*h[2,2])*x1^2+h[2,2]*critval)/h[2,2]
  # calculate "top" and "bottom" halves (plus/minus solutions to quadratic)
  y1 <- -h[1,2]/h[2,2]*x1 + det 
  x2 <- rev(x1)
  y2 <- -h[1,2]/h[2,2]*x2 - det
  # translate ellipse to center
  x <- c(x1,x2)+ctr[1]
  y <- c(y1,y2)+ctr[2]
  x <- c(x,x[1])   # complete ellipse
  y <- c(y,y[1])
  if (fill)
    polygon(x,y,...)
  else
    plot(x,y,...)
}



#Below is some code which will plot an ellipse.  This code was written by
#Bernard Flury and Marco Bee originally in S+ for Flury's  book "A First
#Course in Multivariate Statistics" (Springer 1997).  You would have to
#plug in the mean and covariance matrix for m and A respectively for a
#density ellipse.
#
#
#  Plot points satisfying this equation of an ellipse:
#                   -1             2
#           (x-m)' A   (x-m)  <-  c  .
#
#*****************************  CUT HERE  ********************************


# First define all parameters 

A <- matrix(c(1,1,1,2), ncol = 2)		 # define matrix A
m <- c(3, 4)					 # define vector m
const <- 1			 # define constant 
k <- 1000		  # define number of points on the ellipse

# procedure ELLIPS
# procedure ELLIPS

ellips <- function(A, m, const, k)
{
# input:  A	positive definite symmetric matrix of dimension 2 by 2
#         m	column vector of length 2, center of ellipse
#         const	positive constant
#         k	number of points on ellipse (must be at least 2)
# output: x 	a (k by 2) matrix whose rows are the coordinates
#		of k points on the ellipse (y-m)'*A^{-1}*(y-m) = c^2

r <- A[1, 2]/sqrt(A[1, 1] * A[2, 2])
Q <- matrix(0, 2, 2)			  # construct matrix Q for
Q[1, 1] <- sqrt(A[1, 1] %*% (1+r)/2)	# transformation of circle
Q[1, 2] <- -sqrt(A[1, 1] %*% (1-r)/2)		      # to ellipse
Q[2, 1] <- sqrt(A[2, 2] %*% (1+r)/2)
Q[1, 1] <- sqrt(A[2, 2] %*% (1-r)/2)
alpha <- seq(0, by = (2 * pi)/k, length = k)	   # define angles
Z <- cbind(cos(alpha), sin(alpha)) 	   # points on unit circle
X <- t(m + const * Q %*% t(Z))  # coordinates of points on ellipse
X
}					 # end of procedure ellips

# call procedure ellips

X <- ellips(A, m, const, k)		   

# graph the results

#win.graph()
#plot(X[,1], X[,2])

