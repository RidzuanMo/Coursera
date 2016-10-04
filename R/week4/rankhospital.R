rankhospital <- function(state, outcome, num = "best") {
    ## define type of accepted outcome
    type_of_outcome <- c( "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                          "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                          "pneumonia"= "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )
    
    ## Read outcome data
    raw_data <- read.csv(file.path("data","outcome-of-care-measures.csv"), na.string = "Not Available", colClasses = "character")
    
    ## Check that state and outcome are valid
    hospital <- na.omit(subset(raw_data, State==state, select=c("Hospital.Name", type_of_outcome[outcome])))
    
    names(hospital) <- c("Hospital.Name", "Rate")
    
    if(nrow(hospital)==0){
        stop("Invalid State")
    }
    
    if(is.na(type_of_outcome[outcome])){
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    ranking <- hospital[order( as.numeric(hospital$Rate), hospital$Hospital.Name), ]
    ranking$rank <- 1:nrow(ranking)
    
    if(num=="best") {
        return(ranking[1,"Hospital.Name"])
    }
    
    if(num=="worst") {
        return(ranking[nrow(ranking),"Hospital.Name"])
    }
    
    if(num > nrow(ranking)) {
        return(NA)
    }
    
    ranking[ranking$rank == num ,"Hospital.Name"]
}