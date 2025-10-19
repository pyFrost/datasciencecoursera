## This function takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), 
## and the ranking of a hospital in that state for that outcome (num) and
## returns a character vector with the name
## of the hospital that has the ranking specified by the 'num' argument.

rankhospital <- function(state, outcome, num = "best") {
        ## read outcome data
        df <- read.csv("data/outcome-of-care-measures.csv",
                       colClasses = "character")
        
        ## check that 'state' is valid
        if(!(state %in% unique(df$State))) {
                stop("invalid state")
        }
        
        ## check that 'outcome' is valid
        if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        
        ## filter by specified state
        df <- df[df$State == state, ]
        
        ## check that 'num' is valid
        if(!(num %in% c("best", "worst") | num %in% 1:nrow(df))){
                return(NA)
        }
        
        ## coerce mortality rate to a numeric vector, remove NAs,
        ## and sort hospital names based on specified mortality rate
        ## in ascending order
        if(outcome == "heart attack") {
                df[, 11] <- as.numeric(df[, 11])
                df <- df[order(df[, 11],
                               df[, 2],
                               na.last = NA), ]
        }
        else if(outcome == "heart failure") {
                df[, 17] <- as.numeric(df[, 17])
                df <- df[order(df[, 17],
                               df[, 2],
                               na.last = NA), ]
        }
        else {
                df[, 23] <- as.numeric(df[, 23])
                df <- df[order(df[, 23],
                               df[, 2],
                               na.last = NA), ]
        }
        
        ## return the hospital name in the row based on the specified 'num'
        if(num == "best") {
                return(df[1, 2])
        }
        else if(num == "worst") {
                return(df[nrow(df), 2])
        }
        else {
                return(df[num, 2])
        }
        
}