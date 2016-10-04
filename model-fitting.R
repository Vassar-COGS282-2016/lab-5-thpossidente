# Here's a (simulated) experiment, with a single subject and 500 categorization trials.
all.data <- read.csv('experiment-data.csv')
source('memory-limited-exemplar-model.R')
rm(sample.data.set)
rm(sample.training.data)
# Use optim() to fit the model to this data.
# Note: In optim() you can tell it to display updates as it goes with:
# optim( ... , control=list(trace=4))

exemplar.optim <- function(parameters){
  sensitivity <- parameters[1]
  decay.rate <- parameters[2]
  
  if(decay.rate > 1 || decay.rate < 0){return(NA)}
  
  if(sensitivity < 0){ return(NA)}
  
  return(exemplar.memory.log.likelihood(all.data, sensitivity, decay.rate))
}

fit <- optim(c(0.5,0.5), exemplar.optim, method="Nelder-Mead", control=list(trace=4))

fit$par # sensitivity = 5.1536900, decay.rate = 0.6272777
fit$value

# Now try fitting a restricted version of the model, where we assume there is no decay.
# Fix the decay.rate parameter to 1, and use optim to fit the sensitivity parameter.
# Note that you will need to use method="Brent" in optim() instead of Nelder-Mead. 
# The brent method also requires an upper and lower boundary:
# optim( ..., upper=100, lower=0, method="Brent")

exemplar.optim <- function(sensitivity){
  
  decay.rate <- 1
  
  if(sensitivity < 0){ return(NA)}
  
  return(exemplar.memory.log.likelihood(all.data, sensitivity, decay.rate))
}

fit <- optim(c(0.5), exemplar.optim, upper=100, lower=0, method="Brent", control=list(trace=4))

fit$par # sensitivity = 3.862599
fit$value



# What's the log likelihood of both models? (see the $value in the result of optiom(),
# remember this is the negative log likeihood, so multiply by -1.

## Log likelihood for model with memory decay: 187.5985
## Log likelihood for model with fixed decay: 248.5161

# What's the AIC and BIC for both models? Which model should we prefer?

##For model with memory decay: AIC =  379.197, BIC = 387.6262
##For model with fixed decay: AIC = 499.0322, BIC = 503.2468

##We should prefer the likelihood model with memory decay


#### BONUS...
# If you complete this part I'll refund you a late day. You do not need to do this.

# Use parametric bootstrapping to estimate the uncertainty on the decay.rate parameter.
# Unfortunately the model takes too long to fit to generate a large bootstrapped sample in
# a reasonable time, so use a small sample size of 10-100 depending on how long you are
# willing to let your computer crunch the numbers.

# Steps for parametric bootstrapping:
# Use the best fitting parameters above to generate a new data set (in this case, that means
# a new set of values in the correct column for all.data).
# Fit the model to this new data, record the MLE for decay.rate.
# Repeat many times to get a distribution of decay.rate values.
# Usually you would then summarize with a 95% CI, but for our purposes you can just plot a
# histogram of the distribution.

