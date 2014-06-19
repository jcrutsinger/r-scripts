## rankall.R
## Created by: Joseph Crutsinger
## Date: 06/19/2014
## Course rprog-004
## Write a function called rankall that takes two arguments: an outcome name (outcome)
## and a hospital ranking (num). The function reads the outcome-of-care-measures.csv 
## file and returns a 2-column data frame containing the hospital in each state that has
## the ranking specified in num.

rankall <- function(outcome, num = 'best') {
  oc <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  oc[,11] <- suppressWarnings(as.numeric(oc[,11]))
  oc[,17] <- suppressWarnings(as.numeric(oc[,17]))
  oc[,23] <- suppressWarnings(as.numeric(oc[,23]))
  
  states <- sort(unique(oc$State))
  
  conditions <- c('heart attack', 'heart failure', 'pneumonia')
    
    if (!outcome %in% conditions) { stop('invalid outcome') }
    if (outcome == 'heart attack' ) { selector <- 11 }
    if (outcome == 'heart failure' ) { selector <- 17 }
    if (outcome == 'pneumonia' ) { selector <- 23 }
  
  hospitals <- c()
  
    for (i in states) {
    soc <- oc[grep(i, oc$State, ignore.case=T),]
    
    sorted <- soc[order(soc[,selector],soc[,2]),c(1,2,selector)]
    sorted <- na.omit(sorted)
    
    if (num == 'best') { num <- 1 }
    if (num == 'worst') { num <- nrow(sorted) }
    
  hospitals <- append(hospitals, sorted[num,2])
  }
  
  data.frame(hospital=hospitals, state=states)
}