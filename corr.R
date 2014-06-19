## corr.R
## Created by: Joseph Crutsinger
## Date: 06/12/2014
## Course rprog-004
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0
## Return a numeric vector of correlations

corr <- function(directory, threshold=0) {
    result <- numeric(0)
      for (file in list.files(directory)) {
        data <- read.csv(file.path(directory, file), header=TRUE)
        completeCases <- data[complete.cases(data), ]
      if (nrow(completeCases) > threshold) {
        sulfate <- completeCases[, "sulfate"]
        nitrate <- completeCases[, "nitrate"]
      result <- c(result, cor(sulfate, nitrate))
    }
  }
  result
}
