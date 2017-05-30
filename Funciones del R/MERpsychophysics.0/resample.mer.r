resample.mer <- function(model, sample.size){
                                                                                                                                                           
pps <- model@dims[2]/summary(model)@ngrps                                       
r.sd <- as.numeric(summary(model)@REmat[,4])                                    

if(length(r.sd) == 1){                                                          
r.interc <- matrix(data = numeric(), nrow = model@dims[2] * sample.size,
 ncol = 5, dimnames = list(c(NULL), c("Subj", "Mean", "Str.Dev", "Sample.Val",
  "Fix.Eff")) )
r.interc[,1] <- sort(rep(1: c(sample.size* summary(model)@ngrps), pps))         
r.interc[,2] <- rep(0, model@dims[2] * sample.size)
r.interc[,3] <- rep(r.sd[1], model@dims[2] * sample.size)

buffer <- numeric(length = sample.size* summary(model)@ngrps)                   
for(i in 1:c(sample.size* summary(model)@ngrps)){buffer[i] <- rnorm(n = 1,      
  mean = 0, sd = r.sd[1])}
r.interc[,4] <- rep(buffer, each = pps)                                         
r.interc[,5] <- rep(model@fixef[1], model@dims[2] * sample.size)
                                                                                
r.slope <- matrix(data = numeric(), nrow = model@dims[2] * sample.size,         
 ncol = 5, dimnames = list(c(NULL), c("Subj", "Mean", "Str.Dev",
  "Sample.Val", "Fix.Eff")) )
r.slope[,1] <- sort(rep(1: c(sample.size* summary(model)@ngrps), pps))
r.slope[,2] <- rep(0, model@dims[2] * sample.size)
r.slope[,3] <- rep(0, model@dims[2] * sample.size)
r.slope[,4] <- rep(0, model@dims[2] * sample.size)
r.slope[,5] <- rep(model@fixef[2], model@dims[2] * sample.size)}else{
                                                                                
r.interc <- matrix(data = numeric(), nrow = model@dims[2] * sample.size,
 ncol = 5, dimnames = list(c(NULL), c("Subj", "Mean", "Str.Dev", "Sample.Val",
  "Fix.Eff")) )
r.interc[,1] <- sort(rep(1: c(sample.size* summary(model)@ngrps), pps))
r.interc[,2] <- rep(0, model@dims[2] * sample.size)
r.interc[,3] <- rep(r.sd[1], model@dims[2] * sample.size)
r.interc[,5] <- rep(model@fixef[1], model@dims[2] * sample.size)

r.slope <- matrix(data = numeric(), nrow = model@dims[2] * sample.size,
 ncol = 5, dimnames = list(c(NULL), c("Subj", "Mean", "Str.Dev",
  "Sample.Val", "Fix.Eff")) )
r.slope[,1] <- sort(rep(1: c(sample.size* summary(model)@ngrps), pps))
r.slope[,2] <- rep(0, model@dims[2] * sample.size)
r.slope[,3] <- rep(r.sd[2], model@dims[2] * sample.size)
r.slope[,5] <- rep(model@fixef[2], model@dims[2] * sample.size)

buffer <- matrix(numeric(), nrow = sample.size* summary(model)@ngrps,
 ncol = 2)
 
mer.vcov = nearPD(VarCorr(model, type = "varcov")[[1]][1:2,1:2])	 			
for(i in 1:c(sample.size* summary(model)@ngrps)){buffer[i,1:2] <- rmnorm(n = 1, 
  mean = rep(0, 2), varcov = as.matrix(mer.vcov$mat))}                     					
r.interc[,4]<- rep(buffer[,1], each = pps)
r.slope[,4] <- rep(buffer[,2], each = pps)
}

GRID <- data.frame(cbind(apply(X = model@X, MARGIN = c(2), FUN = rep,
 times = sample.size), r.interc[,4] + r.interc[,5], r.slope[,4] + r.slope[,5]))
names(GRID) <- c(names(model@fixef), "alpha", "beta")                           

probab <- apply(GRID, MARGIN = 1,
 FUN = function(X){pnorm(q = X[2], mean = -X[3]/X[4], sd = 1/X[4])})
probab <- data.frame(cbind(rep(model@frame[,1][,1]+ model@frame[,1][,2],
  sample.size), probab))
names(probab) <- c("size", "prob")
risposte <- apply(probab, MARGIN = 1, FUN = function(X) rbinom(n = 1,
    prob = X[2], size = X[1]))
                                                                                
#campioni <- sort(rep(1:sample.size, model@dims[2]))
campioni <- rep(1:sample.size, each = model@dims[2])
condizioni <- rep(0,model@dims[2] * sample.size)

datafr <- data.frame(cbind(GRID[,2:4], risposte, probab[,1],
    campioni, condizioni, as.factor(r.interc[,1])))                             
names(datafr) <- c("indep", "alpha", "beta", "risposte", "totale", "campioni",
 "condizioni", "sogg")
return(datafr)
}

