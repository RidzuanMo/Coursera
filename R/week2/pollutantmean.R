pollutantmean <- function(directory, pollutant, id = 1:332) {
    poll_vector <- vector()
    
    # read datasets
    for (i in id[1]:id[length(id)]) {
        # read data from csv
        filename <- file.path(".",directory,paste(sprintf("%03d",i),".csv",sep=""))
        data <- read.csv(filename[1])
        
        #apply filter
        filter <- !is.na(data[pollutant])
        filtered_data = subset(data, filter)
        
        poll_vector <- rbind(poll_vector, filtered_data[pollutant])
    }
    
    colMeans(poll_vector[pollutant])
}