## pollutantmean.R
## Created by: Joseph Crutsinger
## Date: 06/12/2014
## Course rprog-004
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".
## 'id' is an integer vector indicating the monitor ID numbers to be used
## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)

pollutantmean <- function(directory, pollutant, id=1:332) {
    all = numeric()
      for (i in id) {
      files <- file.path(directory, paste(sprintf("%03d", i), ".csv", sep=""))
      data <- read.csv(files, header=TRUE)
      noNA <- !is.na(data[, pollutant])
    all <- c(arll, data[noNA, pollutant])
  }
    mean(all)
}
