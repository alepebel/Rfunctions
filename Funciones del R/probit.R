

probit.par <- function(probitfit)
{
  
  p <- (as.numeric(coef(probitfit)))
  list(mean=p[1]/(-p[2]),sd=1/-p[2])
  
}
