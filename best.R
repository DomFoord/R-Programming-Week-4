best <- function(state, outcome){
  ## import data
  file <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
  ##subset to the important data
  outcomes <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  data <- as.data.frame(
    cbind(
      file[, 2],          # hospital
      file[, 7],          # state
      file[, 11],         # heart attack
      file[, 17],         # heart failure
      file[, 23]          # pneumonia
    ),
    stringsAsFactors = FALSE
  )
  colnames(data) <- outcomes
  
  if(!state %in% data[, 'state']){
    stop("invalid state")
  }
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  
  else{
    temp <- which(data[, "state"] == state)
    tempd <- data[temp, ]    
    vals <- as.numeric(tempd[, eval(outcome)])
    min_val <- min(vals, na.rm = TRUE)
    result  <- tempd[, "hospital"][which(vals == min_val)]
    output  <- result[order(result)]
  }
  return(output)
}