## complete.R
## Created by: Joseph Crutsinger
## Date: 06/12/2014
## Course rprog-004
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'id' is an integer vector indicating the monitor ID numbers to be used
## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases

complete <- function(directory, id=1:332) {
    result <- data.frame(id=id, nobs=numeric(length(id)))
      for (i in id) {
      files <- file.path(directory, paste(sprintf("%03d", i), ".csv", sep=""))
      data <- read.csv(files, header=TRUE)
      nobs <- nrow(data[complete.cases(data),])
    result[result["id"] == i,]["nobs"] <- nobs
  }
    result
}