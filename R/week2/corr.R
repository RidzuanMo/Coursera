corr <- function(directory, threshold=0) {
    # load complete cases
    complete_cases <- complete(directory)
    
    # filter complete cases againt threshold
    selected_cases = subset(complete_cases, nobs > threshold)
    
    vector_corr = numeric()
    
    if(nrow(selected_cases) > 0) {
        for(i in 1:nrow(selected_cases)) {
            file_id <- selected_cases[i,"id"]
            
            # read data from csv
            filename <- file.path(".",directory,paste(sprintf("%03d",file_id),".csv",sep=""))
            data <- read.csv(filename[1])
            
            #apply filter
            filter <- complete.cases(data)
            filtered_data = subset(data, filter, c("sulfate", "nitrate"))
            
            vector_corr[i] <- cor(filtered_data$sulfate, filtered_data$nitrate)
        }
    }
    
    vector_corr
}