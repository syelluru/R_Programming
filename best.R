########################################################################################################
# Design Name: Programming Assignment 3 - R Programming (Coursera)
# Author: Suhas Yelluru 
# Create Date: Jan 19 2017; 23:34
# Description: This code is a function that takes two arguments namely, State Abbreviation and Outcome
#              and returns the hospital name with the least deaths caused due to that outcome
########################################################################################################

best <- function(state, outcome)
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
    else
    {
      #2.4.1: Get rows of user inputted state:
      rows <- which(my_data$state == state)
      #table for called state
      tab <- my_data[rows,]
      
      #2.4.2: Get info about inputted outcome:
      #get outcome --> convert to numeric --> get the least value(omit NA's) --> print result
      get <- as.numeric(tab[,eval(outcome)])
      least <- min(get, na.rm=T)
      output <- tab$hospital[which(get==least)]
      result <- output[order(output)]
    }
return(result)
}