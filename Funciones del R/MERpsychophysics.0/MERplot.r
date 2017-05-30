MERplot = function(model, randcol = 3, 
	x.label = "Stimulus Intensity", y.label = "Predicted Response",
 	p05line = F, x.ref = mean(model@X[,2]),
 	x.from = min(model@X[,2]), x.to = max(model@X[,2]), col = F){
	
	#number of subjects and points per subject in a balanced design
	nsubjects = summary(model)@ngrps
	pps <- model@dims[2]/summary(model)@ngrps
	
	#colors
	if(col == T){
		if(length(palette()) < nsubjects){palette(rainbow(nsubjects))}		
		col.code.curves = 1:nsubjects
		col.code.points = model@frame[ ,randcol]}else{
				 	col.code.curves = rep(1, nsubjects)
				 	col.code.points = rep(x = 1, times = nsubjects*pps)}
	                                    							
	#for plotting beta > 0 and beta < 0
	#----------------------------------------------------------------------------#	
	plot.cdf = function(X){
		BETAplus = which(X[,2] > 0) 
		Xplus = X[BETAplus,]
		Xminus = X[-BETAplus,]
		
		if(nrow(Xplus) > 0){	
		apply(X = Xplus, MARGIN = 1,
		 FUN = function(X) {curve(expr = pnorm(x, mean = -X[1]/X[2], sd = 1/X[2] ),
		 	 from = x.from, to = x.to,
		 	  #col = "black", 
		 	  col = X[3],
		 	  add = T)}
		 )}
		 
		if(nrow(Xminus) > 0){
		apply(X = Xminus, MARGIN = 1,
		 FUN = function(X) {curve(expr = 1 - pnorm(x, mean = -X[1]/X[2], sd = 1/abs(X[2] )),
		 	from = x.from, to = x.to,
		 		#col = "red", 
		 	col = X[3],
		 	add = T)}
		 )}
		return(BETAplus)
		}
	#----------------------------------------------------------------------------#
	
	#BLUPS (random modes)			
	if(dim(ranef(model)[[1]])[2] == 1){
		estimates = data.frame(matrix(numeric(), nrow = nrow(ranef(model)[[1]]), ncol = 3))
		estimates[,1] = ranef(model)[[1]] + fixef(model)[1]
		estimates[,2] = rep(fixef(model)[2], times = nrow(estimates))
		estimates[,3] = col.code.curves}else{
		estimates = t(apply(X = ranef(model)[[1]], MARGIN = 1, FUN = function(X){X + fixef(model)}))
		estimates = cbind(estimates, col.code.curves)
		}
		
	plot(y = model@y, x = model@X[,2],  xlim =c(x.from, x.to),
	 xlab = x.label, ylim = c(0,1), ylab = y.label,
	 col = col.code.points)
	
	if (p05line == T){
		abline(h = 0.5, col = "lightgrey", lty = "dashed")
		abline(v = x.ref, col = "lightgrey", lty = "dashed")}
 	
 	plot.cdf(estimates)
 	
	palette("default")
	return(estimates)
	}


