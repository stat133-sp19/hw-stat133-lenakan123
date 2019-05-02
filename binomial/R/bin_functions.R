

##1.3 function bin_choose()

#' @title function called bin choose
#' @description computes the number of combinations in which k success can occur in n trials
#' @param n is the number trials
#' @param k is the number of successes
#' @export
#' @return is the computed number of combinations
#' @examples
#' #choose a success of 2 out of 10 trials
#'
#' bin_choose(n=5,k=2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)

bin_choose <- function(n,k){
  if(any(k>n)){
    stop("k cannot be greater than n")
  }
  return(factorial(n)/(factorial(k)*factorial(n-k)))
}

#1.4 Function bin_probability()

#' @title function used to compute the bin probability
#' @description computes the probability of getting # successes in # trials
#' @param success success rate
#' @param trials is the number of trials
#' @param prob is the probability of success
#' @export
#' @return returns the bin probability
#' @examples
#'#binomial probability with success, trials and probability
#'
#'bin_probability(success= 2, trials = 5, prob = 0.5)
#'bin_probability(success=0:2, trials=5, prob=0.5)
#'bin_probability(success=55, trials = 100, prob= 0.45)

bin_probability <- function(success, trials, prob){
  check_prob(prob)
  check_success(success, trials)
  check_trials(trials)
  return(bin_choose(n = trials, k = success)*prob^success*(1-prob)^(trials-success))
}


##1.5 Function bin_distribution()

#' @title function used to compute the bin distribution
#' @description computes the distribution of the bin probability
#' @param trials number of trials
#' @param prob is the probability of success
#' @export
#' @return returns the bin distribution in form of table and a histogram
#' @examples
#' #default bin distribution
#'
#'bin_distribution(trials=5, prob=0.5)
#'dis1 <- bin_distribution(trials=5, prob = 0.5)
#'plot(dis1)

##use bin_probability(0 to create a main funtion bin_districbution

bin_distribution <- function(trials, prob){
  test <- (bin_probability(success = 0:trials, trials=trials,
                           prob=prob))
  frame <- data.frame(success = 0:trials, probability = test)
  class(frame) <- c("bindis", "data.frame")
  return(frame)
}

##Write a plotting mehtod plot.bindis() that graphs a barplot to display the proabbility historgram of a binomial distribution object "bindis"

#' @export

plot.bindis <- function(data){
  barplot(data$probability, names.arg = data$success, main = "Barplot probability and
          success",xlab = "successes", ylab = "probability")
}

#1.6 Function bin_cumulative()


#' @title function used to compute cumulative bins
#' @description generates cumulative table for trials and probabilities of success
#' @param trials number of trials
#' @param prob is the probability of success
#' @export
#' @return returns the daata talbe of success, probability and cumulative
#' @examples
#'
#'#cumulative toss graph
#'bin_cumulative(trials=5, prob=0.5)
#'dis2 <- bin_cumulative(trials=5, prob=0.5)
#'plot(dis2)
#'
bin_cumulative <- function(trials, prob){
  test <- (bin_probability(success = 0:trials, trials=trials,
                           prob=prob))
  cum <- cumsum(test)
  frame <- data.frame(success = 0:trials, probability = test, cumulative = cum)
  class(frame) <- c("bincum", "data.frame")
  return(frame)
}

#' @export
plot.bincum <- function(data){
  plot(data$success, data$cumulative, main = "Plotting binomial cumulative distribution",
       xlab = "successes", ylab = "probability")
  lines(data$success, data$cumulative)
  invisible(data)
}

#1.7 Function bin_varialbe()


#' @title function used to compute bin variable
#' @description generates bin variable
#' @param trials number of trials
#' @param prob is the probability of success
#' @export
#' @return returns the binomial random variable object
#' @examples
#'
#'#bin variable example
#'bin1 <- bin_variable(trials=10, p=0.3)
#'bin1
#'bin1 <- bin_variable(trials = 10, p =0.3)
#'binsum1 <- summary(bin1)
#'binsum1
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  variable <- list(trials = trials, prob= prob)
  class(variable) <- "binvar"
  return(variable)
}

#' @export
print.binvar <- function(data){
  cat('"Binomial variable" \n\n', 'Parameters \n',
      '- number of trials:', data$trials, '\n', '- prob of success :', data$prob)
}

#' @export


summary.binvar <- function(data){
  cat('"Binomial variable" \n\n', 'Parameters \n',
      '- number of trials:', data$trials, '\n', '- prob of success :',
      data$prob, '\n\n')
  cat('Measures \n',
      '- mean     :', aux_mean(data$trials, data$prob), '\n',
      '- varaiance:', aux_variance(data$trials, data$prob), '\n',
      '- mode     :', aux_mode(data$trials, data$prob), '\n',
      '- skewness :', aux_skewness(data$trials ,data$prob), '\n',
      '- kurtosis :', aux_kurtosis(data$trials, data$prob), '\n')
}



#1.8 Functions of Measures

#finally, you binomial package should also contain main functinos for each of the summary measures:
#e.g. bin_mean(), bin_variance(), etc.

#' @title compute the binomial mean
#' @description generates binomial mean
#' @param trials number of trials
#' @param prob is the probability of success
#' @export
#' @return returns the binomial mean of an object
#' #' @examples
#'
#'#bin_mean
#'bin_mean(10,0.2)
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title compute the binomial variance
#' @description generates binomial variance
#' @param trials number of trials
#' @param prob is the probability of success
#' @export
#' @return returns the binomial variance of an object
#' #' @examples
#'
#'#bin_variance
#'bin_variance(10,0.2)
bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title compute the binomial mode
#' @description generates binomial mode
#' @param trials number of trials
#' @param prob is the probability of success
#' @export
#' @return returns the binomial mode of an object
#' #' @examples
#'
#'#bin_mode
#'bin_mode(10,0.2)
bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title compute the binomial skewness
#' @description generates binomial skewness
#' @param trials number of trials
#' @param prob is the probability of success
#' @export
#' @return returns the binomial skewness of an object
#' #' @examples
#'
#'#bin_skewness
#'bin_skewness(10,0.2)

bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title compute the binomial kurtosis
#' @description generates binomial kurtosis
#' @param trials number of trials
#' @param prob is the probability of success
#' @export
#' @return returns the binomial kurtosis of an object
#' #' @examples
#'
#'#bin_kurtosis
#'bin_kurtosis(10,0.2)

bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}
