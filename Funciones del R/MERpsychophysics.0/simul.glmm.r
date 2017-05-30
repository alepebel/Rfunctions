suff.GLMM <- function(models){
smp <- length(models)
psy.glmm <- matrix(rep(numeric(smp * 2), times = 4), 
    ncol = 4, dimnames = list(row <- NULL, statistics <- c("Estimate", 
        "Std. Error", "Inferior", "Superior")))
pse <- rep(c(1, 0), smp)                                                        
for (i in 1:smp) {
    j <- seq(from = 1, to = smp * 2, by = 2)[i]
    psy.glmm[j:(j+1),] <- delta.psy.probit(models[[i]], lme4 = T)               
}
psy.glmm <- data.frame(cbind(pse, psy.glmm))
return(psy.glmm)
}


