rankall <- function(outcome, num = "best") {
        ## read outcome data
        df <- read.csv("data/outcome-of-care-measures.csv",
                       colClasses = "character")
        
        
        ## check that 'outcome' is valid
        if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        
        ## create empty data frame to hold results
        output <- data.frame()
        
        ## loop over each state
        for (st in sort(unique(df$State))){
                
                ## filter by specified state
                df_by_state <- df[df$State == st, ]
                
                ## check that 'num' is valid
                if(!(num %in% c("best", "worst") | 
                     num %in% 1:nrow(df_by_state))){
                        output[st, "state"] <- st
                        output[st, "hospital"] <- NA
                        next
                }
                
                ## coerce mortality rate to a numeric vector, remove NAs,
                ## and sort hospital names based on specified mortality rate
                ## in ascending order
                if(outcome == "heart attack") {
                        df_by_state[, 11] <- as.numeric(df_by_state[, 11])
                        df_by_state <- df_by_state[order(df_by_state[, 11],
                                                         df_by_state[, 2],
                                                         na.last = NA), ]
                }
                else if(outcome == "heart failure") {
                        df_by_state[, 17] <- as.numeric(df_by_state[, 17])
                        df_by_state <- df_by_state[order(df_by_state[, 17],
                                                         df_by_state[, 2],
                                                         na.last = NA), ]
                }
                else {
                        df_by_state[, 23] <- as.numeric(df_by_state[, 23])
                        df_by_state <- df_by_state[order(df_by_state[, 23],
                                                         df_by_state[, 2],
                                                         na.last = NA), ]
                }
                
                output[st, "state"] <- st
                
                ## return the hospital name
                ## in the row based on the specified 'num'
                if(num == "best") {
                        output[st, "hospital"] <- df_by_state[1, 2]
                }
                else if(num == "worst") {
                        output[st, "hospital"] <- df_by_state[nrow(df_by_state),
                                                              2]
                }
                else {
                        output[st, "hospital"] <- df_by_state[num, 2]
                }
                
                        
        }
        
        return(output[, c("hospital", "state")])
        
}