
MERboot = function(model, size = 600, confint = 0.95, fileout = F, filenamePSE = "MCMCpse.pdf",
 filenameJND = "MCMCjnd.pdf"){
	
	if(size < 100){
		size = 100
		print("Warning message: The sample size is too small! It has been set to 100")}
	
	sim.cond <- resample.mer(model, sample.size = size)
	#sim.cond <- resample.merCM(model, sample.size = size)
	  
	glmm.list <-by(data = sim.cond, INDICES = sim.cond$campioni, FUN = glmer,
  		formula = cbind(risposte, totale - risposte) ~ indep + (1 + indep|sogg),
   		family = binomial(link="probit"), nAGQ = 1, simplify = TRUE)                   
                                                                           
	psy.glmm <- suff.GLMM(glmm.list)
	alpha05 = (1 - confint)/2
	percentiles = c(alpha05, 0.5, 1-alpha05) 
	output = matrix(c(sort(psy.glmm[psy.glmm$pse == 1,2])[size*percentiles],
					sort(psy.glmm[psy.glmm$pse == 0,2])[size*percentiles]), byrow = T, nrow = 2)
	dimnames(output) = list(c("pse","jnd"), c("Inferior", "Median", "Superior"))			
	
	if(fileout == T){
	title = paste("Median and ", confint*100, "% Confidence Interval", sep ="")
	
	pdf(file = filenamePSE)
		hist(sort(psy.glmm[psy.glmm$pse == 1, 2]), breaks = size/10, main = "PSE: bootstrap-based estimate",
 			xlab = title)
		abline(v = output[1,], lty = c(2,2,2), col = c("black", "red", "black"))
				dev.off()
		
	pdf(file = filenameJND)
		hist(sort(psy.glmm[psy.glmm$pse == 0, 2]), breaks = size/10, main = "JND: bootstrap-based estimate",
 			xlab = title)
		abline(v = output[2,], lty = c(2,2,2), col = c("black", "red", "black"))
		dev.off()}
				
	return(output)
	}