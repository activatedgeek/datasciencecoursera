corr <- function(directory, threshold = 0) {
    files <- list.files(directory)
    correlations <- c()
    row <- 1
    for(file in files){
        data <- read.csv(paste(directory,file,sep="/"))
        data <- data[complete.cases(data),]
        if(nrow(data)>threshold){
            correlations[row] <- cor(data[,"nitrate"], data[,"sulfate"])
            row <- row + 1
        }
    }
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    correlations
}