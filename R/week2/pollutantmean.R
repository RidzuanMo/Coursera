pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files.
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of pollutant for which we will calculate the mean;
    ## either"sulfate" or "nitrate"
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant accross all monitor list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result
    
    poll_vector <- numeric()
    
    # read datasets
    for (i in id[1]:id[length(id)]) {
        # read data from csv
        filename <- file.path(".",directory,paste(sprintf("%03d",i),".csv",sep=""))
        data <- read.csv(filename[1])
        
        #apply filter
        filter <- !is.na(data[pollutant])
        filtered_data <- subset(data, filter, c(pollutant))
        
        poll_vector <- rbind(poll_vector, filtered_data)
    }

    colMeans(poll_vector)
}