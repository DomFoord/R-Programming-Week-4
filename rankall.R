rankall <- function(outcome,num="best"){
  data <-read.csv("outcome-of-care-measures.csv")
  
  if(outcome=="heart attack"){
    data <- data.frame(hospital=data$Hospital.Name,rate=data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,state=data$State)
  }else if(outcome=="heart failure"){
    data <- data.frame(hospital=data$Hospital.Name,rate=data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,state=data$State)
  }else if(outcome=="pneumonia"){
    data <- data.frame(hospital=data$Hospital.Name,rate=data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,state=data$State)
  }else{
    stop("invalid outcome")
  }
  rank<-function(data){
    if(num=="best")
      num<-1
    else if(num=="worst")
      num<-nrow(data)
    
    hospitalname<-data$hospital[order(data$rate,data$hospital)[num]]
    as.character(hospitalname)
  }
  split_data<-split(data,data$state)
  ans<-lapply(split_data, rank)
  
  data.frame(hospital=as.character(ans),state=names(ans))
  
}