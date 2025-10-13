pollutantmean <- function(directory, pollutant, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant --either "sulfate" or "nitrate" -- for which
  ## the mean will be calculated
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Returns the mean of the pollutant across the monitors listed 
  ## in the 'id' vector (ignoring)
  ## NOTE: the results is not rounded
  
  # compile data in a data frame
  dat <- data.frame()
  
  for (csvfile in list.files(directory, full.names = TRUE)){
    
    # read data from file and add to data frame
    dat <- rbind(dat, read.csv(csvfile, header = TRUE))
  }
  
  # calc. mean for pollutant ignoring NAs
  mean(dat[which(dat[ , "ID"] %in% id), pollutant], na.rm = TRUE)

}