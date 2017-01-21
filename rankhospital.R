########################################################################################################
# Design Name: Programming Assignment 3 - R Programming (Coursera)
# Author: Suhas Yelluru 
# Create Date: Jan 20 2017; 02:34
# Description: This code is a function that takes three arguments namely, State Abbreviation, Outcome
#              and rankings which returns the hospital name with the least deaths caused due to that 
#              outcome based on if the rank was worst, or best.
########################################################################################################

rankhospital <- function(state, outcome, rank)
{
  
  #STEP 1 : Load Dataset, create new data frame that has only required values
  outcomes_DataFrame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  my_data <- as.data.frame(cbind(outcomes_DataFrame[, 2],   # hospital
                                 outcomes_DataFrame[, 7],   # state
                                 outcomes_DataFrame[, 11],  # heart attack
                                 outcomes_DataFrame[, 17],  # heart failure
                                 outcomes_DataFrame[, 23]), # pneumonia
                           stringsAsFactors = FALSE)
  colnames(my_data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  #STEP 2: Check if function arguments are valid
  
  #2.1 Retrieve all the unique state abbreviations in the Data Set and the 
  #    possible outcomes
  states <- unique(my_data$state)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  #2.2 Check if State(function arg) is in the states variable
  if ((state %in% states) == FALSE)
  {
    stop(print("invalid state"))
  }
  
  #2.3 Check if Outcome(function arg) is any of 
  else if ((outcome %in% outcomes) == FALSE)
  {
    stop(print("invalid outcome"))
  }
  
  #2.4 Get the results:
  else if(!is.numeric(rank))
  {
    if (rank == "best")
    {
      result <- best(state,outcome)
    }
    else if (rank == "worst")
    {
      #2.4.1: Get rows of user inputted state:
      rows <- which(my_data$state == state)
      #table for called state
      tab <- my_data[rows,]
      get <- as.numeric(tab[,eval(outcome)])
      tab <- tab[order(get,tab[,"hospital"], decreasing = T),]
      result <- tab[,"hospital"][1]
    }
    else
    {
      stop('invalid rank')
    }
  }
  return(result)
}