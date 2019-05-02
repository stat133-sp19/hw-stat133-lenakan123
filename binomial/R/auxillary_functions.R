#1.2 private auxiliary functions

#write the following auxiliary functions: aux_mean(), aux_variance(),

aux_mean <- function(trials, prob){
  return(trials*prob)
}

aux_variance <- function(trials, prob){
  return((trials*prob)*(1-prob))
}

aux_mode <- function(trials, prob){
  if(trials*prob+prob == round(trials*prob+prob)){
    return(c(floor(trials*prob+prob), floor(trials*prob+prob)-1))
  }
  else if(trials*prob+prob != round(trials*prob+prob)){
    return(floor(trials*prob+prob))
  }
}

aux_skewness <- function(trials, prob){
  return((1-2*prob)/(trials*prob*(1-prob))^0.5)
}


aux_kurtosis <- function(trials, prob){
  return((1-6*prob*(1-prob))/((trials*prob)*(1-prob)))
}

