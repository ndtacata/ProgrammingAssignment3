rankhospital <- function(state, outcome, num = "best"){
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
    ##rowBasis <- which(df[, "state"] == state)
    data_cleanedup <- df[df$state == state, ]
    ##data
    
    ## set column number for each outcome
    if(outcome == "heart attack"){
        colBasis <- 3
    }else if(outcome == "heart failure"){
        colBasis <- 4
    }else{
        colBasis <- 5
    }
    
    ## Sort Data by outcome and hospital name
    data_sorted <- data_cleanedup[order(as.numeric(data_cleanedup[[colBasis]]),data_cleanedup[["hospital"]],decreasing=FALSE,na.last= TRUE), ]
    data_sorted <- data_sorted[!is.na(data_sorted[, colBasis]), ]

    ##data_sorted
    
    if(num == "best") {
        num <- 1
    }
    
    if(num == "worst"){
        num <- nrow(data_sorted)
    }
    
    ##num
    data_sorted[num, 1]
    
}