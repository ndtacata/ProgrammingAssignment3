best <- function(state, outcome) {
    
    ## read data from excel file outcome-of-care-measures.csv
    data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
    
    
    ## check if state is valid
    states <- unique(data[, 7])
    
    if(!state %in% states){
        stop('invalid state')
    }
    
    ## check if outcome is valid
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    }
    
    ## create a dataframe for storage of data from read .csv file
    
    df <-  data.frame("hospital" = data[, 2], 
                     "state" = data[ , 7], 
                     "heart attack" = data[, 11], 
                     "heart failure" = data[, 17], 
                     "pneumonia" = data[, 23])
    
    ## get rows of given state
    rowBasis <- which(df[, "state"] == state)
    ##rowBasis
    
    
    ## set column number for each outcome
    if(outcome == "heart attack"){
        colBasis <- 3
    }else if(outcome == "heart failure"){
        colBasis <- 4
    }else{
        colBasis <- 5
    }
    
    
    ## store all rows of given state
    df_new <- data.frame()
    df_new <- df[rowBasis, ]
    ##return(df_new)
    
    
    outcome_values <- df_new[, colBasis]
    
    ## get the minimum value; remove all NAs
    min_val <- min(outcome_values, na.rm = TRUE)
    ##min_val
    
    
    result <- df_new[, "hospital"][which(outcome_values == min_val)]
    hospital_name <- result[order(result)]
    
    return(hospital_name)
}