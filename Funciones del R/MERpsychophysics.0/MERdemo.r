#------------------------------------------------------------------------------#
#                         MERpsychopysics 1.1 Demo
#------------------------------------------------------------------------------#
#This is a demo for the R code MERpsychophysics. We assume the user is familiar 
#with R programming (in particular with lme4 package), and has some knowledge of 
#the Generalized Linear Mixed Models. 
#Instal packages required in the user manual before running this script!

#Load MERpsychophysics (enter your path!)
#setwd("/mypath/MERpsychophysics.0")
source("MERpsychophysics.r")

#Go back to your working directory
#setwd("/mypath/myworkdirectory")

#------------------------------------------------------------------------------#
#1	Simulated data: Example 1
#------------------------------------------------------------------------------#
#1.1	Simulation of data and Inference
#We use the function MERsimulate to simulate a single dataframe in this demo.
#Warning: using arbitrary values of parameters, may generate unrealistic responses.
#We suggest to look at simulated row data befor running the analysis. 
#Modify the parameters if resposes are not plausible
#set.seed ensure that your output will match mine (unless you change the script)
set.seed(32787)																	#this ensure that your output will match mine
datafr <- MERsimulate(nsubject = 5, constant = T)                               #Simulate a dataset (default values)
datafr

#one random effect
mod1a <- glmer(cbind(Longer, Total - Longer) ~ X + (1 | Subject),				#fit the GLMM(probit link function)
			 family = binomial(link = "probit"), data = datafr, nAGQ = 10)
summary(mod1a)																	#Summary of the model

#two random effects
mod2a <- glmer(cbind(Longer, Total - Longer) ~ X + (1 + X| Subject),			#fit the GLMM(probit link function)
			 family = binomial(link = "probit"), data = datafr, nAGQ = 10)
summary(mod2a)																	#Summary of the model
																			
#compare the two models: AIC, BIC, LR test
summary(mod1a)@AICtab
summary(mod2a)@AICtab
anova(mod1a, mod2a, test = "Chisq")
#mod1 and mod2 are not significantly differrent

#1.2	Plot the models (either with MERplot or with GLMMplot)
MERplot(mod1a, col = T, p05line = T)
GLMMplot(dataframe = datafr, X.col = 1, Yes.col = 4, Total.col = 5,
 Subject.col = 6, lme4 = T, model = mod1a, p05line = T, col = F)

MERplot(mod2a, col = T, p05line = T, x.from = 300, x.to = 1300)
GLMMplot(dataframe = datafr, X.col = 1, Yes.col = 4, Total.col = 5,
 Subject.col = 6, lme4 = T, model = mod2a, p05line = T, col = T)

#1.3	Estimate the PSE with boostrap method or delta method
MERboot(mod1a, size = 600, fileout = F)											#Estimate of PSE and JND via botstrap method (this takes time!)
	#warnings will probably occur, due to false or singular convergence in 
	#glmm fitting. To check that the estimate is relible, repeat the analysis. 
	#Increase the size to make the estimate more reliable 
	#(although, this will increase the duration of the bootstrap)

delta.psy.probit(mod1a, lme4 = T)												#Estimate of PSE and JND via delta method 
 
#------------------------------------------------------------------------------#
#2	Simulated data: Example 2
#------------------------------------------------------------------------------#
#2.1	Simulation of data and Inference
#We use the function MERsimulate to simulate a single dataframe in this demo.
#set.seed ensure that your output will match mine (unless you change the script)
set.seed(32789)																	
datafr <- MERsimulate(nsubject = 5, constant = T)                               #Simulate a dataset (default values)
datafr

#one random effect
mod1b <- glmer(cbind(Longer, Total - Longer) ~ X + (1 | Subject),				#fit the GLMM(probit link function)
			 family = binomial(link = "probit"), data = datafr, nAGQ = 10)
summary(mod1b)																	#Summary of the model

#two random effects
mod2b <- glmer(cbind(Longer, Total - Longer) ~ X + (1 + X| Subject),			#fit the GLMM(probit link function)
			 family = binomial(link = "probit"), data = datafr, nAGQ = 10)
summary(mod2b)																	#Summary of the model
																			
#compare the two models: AIC, BIC, LR test
#both criterions and LR test are for mod2b
summary(mod1b)@AICtab
summary(mod2b)@AICtab
anova(mod1b, mod2b, test = "Chisq")

#2.2	Plot the models
MERplot(mod1b, col = T, p05line = T)
GLMMplot(dataframe = datafr, X.col = 1, Yes.col = 4, Total.col = 5,
 Subject.col = 6, lme4 = T, model = mod1b, p05line = T, col = F)

MERplot(mod2b, col = T, p05line = T, x.from = 300, x.to = 1300)
GLMMplot(dataframe = datafr, X.col = 1, Yes.col = 4, Total.col = 5,
 Subject.col = 6, lme4 = T, model = mod2b, p05line = T, col = T)

#2.3	Estimate the PSE with boostrap method or delta method
MERboot(mod2b, size = 600, fileout = F)											#Estimate of PSE and JND via botstrap method (this takes time!)
delta.psy.probit(mod2b, lme4 = T)												#Estimate of PSE and JND via delta method 

#------------------------------------------------------------------------------#
#3	More about plotting (Example 2)
#------------------------------------------------------------------------------#
#In subject 3, the slope of the function is mutch shallower.
#Do you want to highlight subject 3? Use palette! 
palette(value = c("gray1", "gray2","red","gray3","gray4"))
MERplot(mod2b, col = T, p05line = T, x.from = 300, x.to = 1300)
palette("default")

#In subject number 3, the slope of the function is mutch shallower.
#Do you want to highlight that subject? Use palette! 
palette(value = c("gray1", "gray2","red","gray3","gray4"))
MERplot(mod2b, col = T, p05line = T, x.from = 300, x.to = 1300)
palette("default")

#In order to export a plot use a function such as pdf(), postscript(), ect.
#use par() to modify graphical parameters
pdf(file = "Figure1.pdf")
par(mar = c(4,4,1,1), cex = 2, lwd = 2, cex.axis = 0.8, cex.lab = 0.8)
MERplot(mod2b, col = T, p05line = T, x.from = 300, x.to = 1300)
dev.off()

#Plot partial effects (requires the package languageR)
library("languageR")
plotLMER.fnc(mod1b, xlabel = "Stimulus Intensity",
 ylabel = "Predicted Response", ylimit = c(0,1))								
							