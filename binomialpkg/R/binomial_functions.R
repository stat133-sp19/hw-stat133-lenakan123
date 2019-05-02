# Binomial Package
#
# This is an example function named 'hello'
# which prints different binomial functions
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#1.1 private checker functions


check_prob <- function(prob){ #check if p is a probability
  if(prob > 1|prob<0){
    stop("p has to be a number from 0 to 1")
  }
  return(TRUE)
}

check_trials <- function(trials){ #check for valid number of trials
  if(trials <= 0){
    stop("invalid trials value")
  }
  return(TRUE)
}

check_success <- function(success, trials){ #check for success to be valid
  if(any(success <0)){
    stop('success cannot be lesse than')
  }
  else if (any(success>trials)){
    stop("success cannot be greater than trials")
  }
  return(TRUE)
}


