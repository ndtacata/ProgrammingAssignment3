best <- function(state, outcome){
    data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
    
    
    ##check if state passed is valid
    states <- unique(data$State)
    ##states
    
    if(!state %in% states){
        stop('invalid state')
    }
    
    
    ##check if outcome passed is valid
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if(!outcome %in% outcomes){
        stop('invalid outcome')
    }
    
    #data

    ##get column number of outcome
    if(outcome == "heart attack"){
        colBasis <- 3
    }else if(outcome == "heart failure"){
        colBasis <- 4
    }else{
        colBasis <- 5
    }
    
    ## limit the data that we will manipulate
    df <- data.frame("hospital" = data[, 2], 
                     "state" = data[ , 7], 
                     "heart attack" = data[, 11], 
                     "heart failure" = data[, 17], 
                     "pneumonia" = data[, 23])
    
    ##df
    df_new <- data.frame()
    
    ## get all rows tagged on state state
    df_new <- df[df$state == state, ]
    
    ##df_new
    
    min_value_of_outcome = min(df_new[, colBasis], na.rm = TRUE)
    
    ##min_value_of_outcome
    output <- df_new[which(df_new[, colBasis] == min_value_of_outcome), "hospital"]
    hospital_result <- output[order(output)]
    
    hospital_result
}