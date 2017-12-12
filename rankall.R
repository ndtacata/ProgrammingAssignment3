rankall <- function(outcome, num = "best"){
    ## read data from excel file outcome-of-care-measures.csv
    data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
    
    ## check if state is valid
    states <- sort(unique(data[, 7]))
    
    ## check if outcome is valid
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    }

    
    df <-  data.frame("hospital" = character(0), 
                      "state" = character(0))
    
    ##length(states)
    
    ctr <- 1
    
    while(ctr <= length(states)){
        source("rankhospital.R")
        rank_hospital <- rankhospital(states[ctr], outcome, num)
        ##return(states[ctr])
        
        
        if (length(rank_hospital[1]) == 0){
            rank_hospital <- "NA"
        }
    
        df_new <- data.frame("hospital" = rank_hospital[1], 
                             "state" = states[ctr])
        
        df <- rbind(df, df_new)
        ctr <- ctr + 1
        
    }
    df
    #states
}