rankhospital <- function(state, outcome, num = "best"){
    data <- read.csv("outcome-of-care-measures.csv", na.string = "Not Available")
    
    states <- unique(data$State)
    
    if(!state %in% states){
        stop('invalid state')
    }
    
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if(!outcome %in% outcomes){
        stop('invalid outcome')
    }
    
    if(outcome == "heart attack"){
        colBasis <- 3
    }else if(outcome == "heart failure"){
        colBasis <- 4
    }else{
        colBasis <- 5
    }
    
    df <- data.frame("hospital" = data[, 2], 
                     "state" = data[ , 7], 
                     "heart attack" = data[, 11], 
                     "heart failure" = data[, 17], 
                     "pneumonia" = data[, 23])
    
    df_new <- df[df$state == state, ]   

    data_temp <- df_new[order(as.numeric(df_new[, colBasis]), df_new[, "hospital"], decreasing = FALSE, na.last = TRUE), ]
    data_temp <- data_temp[!is.na(data_temp[, colBasis]), ]
    
    if(num == "best"){
        num <- 1
    }
    
    if(num == "worst"){
        num <- nrow(data_temp)
    }
    
    ##data_temp
    hospital_output <- data_temp[num, "hospital"]
    hospital_output
    
}