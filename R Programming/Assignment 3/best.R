best <- function(state, outcome){
    col <- c(11,17,23)
    names(col) <- c("heart attack", "heart failure", "pneumonia")
    # Error handling
    if(!(outcome %in% names(col)))
        stop("invalid outcome", call. = T)
    
    data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")
    if(!(state %in% data[,"State"]))
        stop("invalid state", call. = T)
    data <- data[(data$State == state),]
    # Conversion to factor, lots of problems
    data[,col[outcome]] <- as.numeric(levels(data[,col[outcome]]))[data[,col[outcome]]] #Leads to NA coersion warnings
    minval <- min(data[,col[outcome]], na.rm = T)
    rownum <- which( data[,col[outcome]] == minval)
    as.character(data[rownum,2])
}