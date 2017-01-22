########################################################################################################
# Design Name: Programming Assignment 3 - R Programming (Coursera)
# Author: Suhas Yelluru 
# Create Date: Jan 21 2017; 11:44
########################################################################################################
library(dplyr)
rankall <- function(outcome, num = "best") 
{
  #Load Dataset, create new data frame that has only required values
  DataFrame <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that outcome is valid
  if (!((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia"))) 
  {
    stop ("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  coloumn <- if (outcome == "heart attack") {11} 
             else if (outcome == "heart failure") {17} 
             else {23}
  
  DataFrame[, coloumn] <- suppressWarnings(as.numeric(levels(DataFrame[, coloumn])[DataFrame[, coloumn]]))
  DataFrame[, 2] <- as.character(DataFrame[, 2])
  op <- vector()
  
  states <- levels(DataFrame[,7])
  for(i in 1:length(states))
  {
    sd <- DataFrame[grep(states[i],DataFrame$State),]
    od <- sd[order(sd[,coloumn],sd[,2],na.last=NA),]
    hospital <- if(num=="best") {od[1,2]}
                else if(num == "worst") {od[nrow(od),2]}
                else {od[num,2]}
    op <- append(op, c(hospital,states[i]))
  }
  op <- as.data.frame(matrix(op, length(states),2,byrow = T))
  colnames(op) <- c("hospital", "state")
  rownames(op) <- states
  op
}