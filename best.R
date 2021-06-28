
best <- function(state,outcome){
  data <-read.csv("outcome-of-care-measures.csv")
  ## Read outcome data
  statename <- as.character(data$State)
  for(i in 1:length(statename)){
    if (statename[i]==state)
      break
    if(i==length(statename))
      stop("invalid state")
  }
  
  data <- subset.data.frame(data,State==state)
  if(outcome=="heart attack"){
    data <- data.frame(name=data$Hospital.Name,rate=data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  }else if(outcome=="heart failure"){
    data <- data.frame(name=data$Hospital.Name,rate=data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  }else if(outcome=="pneumonia"){
    data <- data.frame(name=data$Hospital.Name,rate=data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  }else{
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  hosname<-data$name[order(data$rate,data$name)[1]]
  as.character(hosname)
  
}