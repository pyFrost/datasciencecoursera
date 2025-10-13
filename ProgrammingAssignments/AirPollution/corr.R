corr <- function(directory, threshold = 0){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed (on all variables)
  ## required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations.
  ## NOTE: The result is not rounded.
  
  # generate table of complete cases
  lookup_df <- complete(directory)
  
  # identify monitors where the number of complete cases > threshold
  suitable_cases <- lookup_df[lookup_df$nobs > threshold, "id"]
  
  # select CSV files that meet criterion
  rfiles = list.files(directory, full.names = TRUE)[suitable_cases]
  
  # compile data from relevant files into a numeric vector
  dat <- numeric()
  
  for (rfile in rfiles){
    
    # read data from file and add to numeric vector
    f <- read.csv(rfile, header = TRUE)
    dat <- append(dat, cor(f$nitrate, f$sulfate, use = "complete.obs"))
  }
  
  # return correlations
  dat
  
}