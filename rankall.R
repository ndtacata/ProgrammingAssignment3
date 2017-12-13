rankall <- function(outcome, num = "best"){
    data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")

    
    states <- sort(unique(data$State))
    
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if(!outcome %in% outcomes){
        stop('invalid outcome')
    }

    df <- data.frame()
    
    for(i in 1:length(states)){
        ##print(states[i])
        source("rankhospital.R")
        rank_hospital_output <- rankhospital(states[i], outcome, num)
        
        ##print(rank_hospital_output)
        
        if(length(rank_hospital_output) == 0){
            rank_hospital_output <- "NA"
        }
        
        df_new <- data.frame("hospital" = rank_hospital_output,
                             "state" = states[i])

        df <- rbind(df, df_new)
        
    }
    
    df
}