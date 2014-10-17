rankhospital <- function(state, outcome, num = "best"){
    col <- c(11,17,23)
    names(col) <- c("heart attack", "heart failure", "pneumonia")
    # Error handling
    if(!(outcome %in% names(col)))
        stop("invalid outcome", call. = T)
    
    data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")
    if(!(state %in% data[,"State"]))
        stop("invalid state", call. = T)
    
    data <- data[(data[,7] == state),][,]
    data[,col[outcome]] <- as.numeric(levels(data[,col[outcome]]))[data[,col[outcome]]]
    if(identical(num,"best")){
        num = 1
        data <- data[order(data[,col[outcome]],data[,2]),]
    }
    else if(identical(num,"worst")){
        num = 1
        data <- data[order(-data[,col[outcome]],data[,2]),]
    }
    else{
        data <- data[order(data[,col[outcome]],data[,2]),]
    }
    as.character(data[num,2])
}