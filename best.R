best <- function(state, outcome) {
  state=state;outcome=outcome
  dt=read.csv("C:/Users/Lovebonito/Desktop/Programming in R/outcome-of-care-measures.csv")
  states=as.vector(unique(dt$State))
  if(sum(state==states)==0){
    stop("invalid state")
  }
  if(outcome!="pneumonia"&&outcome!="heart attack"&&outcome!="heart failure"){
    stop("invalid outcome")
  }
  d=dt[dt$State==state,]
  dt.s=d[order(d$Hospital.Name),]
  if(outcome=="heart attack"){
    dt.so=dt.s[order(as.numeric(as.character(dt.s$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))),]
    return(as.character(dt.so$Hospital.Name[1]))
  }
  if(outcome=="heart failure"){
    dt.so=dt.s[order(as.numeric(as.character(dt.s$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))),]
    return(as.character(dt.so$Hospital.Name[1]))
  }
  if(outcome=="pneumonia"){
    dt.so=dt.s[order(as.numeric(as.character(dt.s$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))),]
    return(as.character(dt.so$Hospital.Name[1]))
  }

}

