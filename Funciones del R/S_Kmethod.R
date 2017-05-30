
S_Kmethod <- function(independent_variable, proportion_of_responces){

# % Calculates the mean of the distribution of responses obtained for example in a 2AFC task. It 
# % makes a weighted sum of the independent values (proportion of responses).
# %NOTE: the range of the independent variable (i.e., asynchronies) to be
# %tested has to be large enough so that the proportion of responses reaches
# %0 at the two extremes
# %Usage:
#   % mean_cumulative=mean_data_s(independent_variable,proportion_of_responces,number_of_responces);
# %  independent_variable:
#   %  proportion_of_responces:are two vectors with the same number of elements
# %  mean_cumulative:         is the estimate of the mean of the underlying distribution
# %
# independent_variable <- c(240,-200, -160, -120,  -80,  -40, 0,  40,  80, 120, 160, 200, 240);
# proportion_of_responces<- c(   0  ,  0 ,   0  , .1 ,  .5  , .7 ,  .8,  .6,  .4,  .2,  .1  , 0  , 0 );
# % plot(independent_variable, proportion_of_responces)
  
# % x=-200:1:200;y=normpdf(x,25,44);plot(x,y);[mu,sigma]=mean_data_s(x,y)
# %
# % Created by Massimiliano Di Luca m.diluca@bham.ac.uk, Birmingham 17/05/2012 from mean_data which is used for cumulative psychometric functions 
#Adapted to R by Alexis Perez Bellido 10/01/2015

  matrix = as.data.frame(cbind(independent_variable , proportion_of_responces ))
  
  sorted_matrix =  matrix[order(matrix$independent_variable ),]
  
  #normalizes the probability (area under the graph) to one
  proportion_of_responces=proportion_of_responces/sum(proportion_of_responces);
  
  #weight each independent_variable at half point for the corresponding difference in responces
  
  mu=sum(proportion_of_responces*independent_variable);
  #plot(independent_variable_middle-mean_cumulative,prop)
  
  #sigma=sqrt(sum(proportion_of_responces*(independent_variable-mu)^2));
sigma=sqrt(sum(proportion_of_responces*(independent_variable-mu)^2));
  c(mu , sigma)
  
}