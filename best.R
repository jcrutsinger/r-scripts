## best.R
## Created by: Joseph Crutsinger
## Date: 06/19/2014
## Course rprog-004
## Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
## outcome name. The function reads the outcome-of-care-measures.csv and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
## be one of heart attack, heart failure, or pneumonia. Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings.

best <- function(state, outcome) {

    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
  
    index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    data[,index] <- suppressWarnings(as.numeric(data[,index]))
    data <- na.omit(data)
    
    states <- table(data$State)
    if (!state %in% names(states)) {
        stop("invalid state")
    }
    
    slice <- subset(data, State==state)
    slice <- slice[order(slice[,index], na.last=TRUE),2]
    slice <- na.omit(slice)
    
    slice[1]
}
 