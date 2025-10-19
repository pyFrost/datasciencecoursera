## This function take two arguments: state abbreviation and outcome name,
## and returns the name of the hospital that has the best 30-day mortality
## for the specified outcome.

best <- function(state, outcome) {
        ## read outcome data
        output <- read.csv("data/outcome-of-care-measures.csv",
                           colClasses = "character")
        
        ## check that 'state' is valid
        if(!(state %in% unique(output$State))) {
                stop("invalid state")
        }
        
        ## check that 'outcome' is valid
        if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        
        ## filter by specified state
        output <- output[output$State == state, ]
        
        ## coerce mortality rate to a numeric vector,
        ## remove NAs, and sort data based mortality rate and hospital name
        ## in ascending order
        if(outcome == "heart attack") {
                output[, 11] <- as.numeric(output[, 11])
                output <- output[order(output[, 11],
                                       output[, 2],
                                       na.last = NA), ]
        }
        else if(outcome == "heart failure") {
                output[, 17] <- as.numeric(output[, 17])
                output <- output[order(output[, 17],
                                       output[, 2],
                                       na.last = NA), ]
        }
        else {
                output[, 23] <- as.numeric(output[, 23])
                output <- output[order(output[, 23], 
                                       output[, 2],
                                       na.last = NA), ]
        }
        
        ## return the hospital name in first row of data frame
        ## this hospital has lowest 30-day death rate in the specified state
        return(output[1, 2])
}