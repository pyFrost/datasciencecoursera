complete <- function(directory, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Returns a data frame of the form: 
  ## id nobs
  ## 1  117
  ## 2. 1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the 
  ## number of complete cases
  
  # prepare list of data files
  files_list <- list.files(directory, full.names = TRUE)
  
  # compile data in a data frame
  dat <- data.frame()
  
  for (i in 1:length(files_list)){
    
    # read data from file 
    contents <- read.csv(files_list[i], header = TRUE)
    
    # add monitor ID and number complete cases to data frame
    dat[i, "id"] <- unique(contents$ID)
    dat[i, "nobs"] <- sum(complete.cases(contents), na.rm = TRUE)
  }
  
  # filter data frame by 'id' 
  # NOTE: this works because rownames & 'id' are identical other use 'which'
  dat <- dat[id, ]
  
  # reindex data frame
  rownames(dat) <- 1:nrow(dat)
  
  # return answer
  dat
  
}