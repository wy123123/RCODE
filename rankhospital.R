rankhospital <- function(state, outcome, num = "best") {
        state=state;outcome=outcome;num=num
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
                temp="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        }
        else if(outcome=="heart failure"){
                temp="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        }
        else{
                temp="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        dt.so=dt.s[order(as.numeric(as.character(dt.s[,temp])),dt.s$Hospital.Name),]
        dt.so=dt.so[dt.so[,temp]!="Not Available",]
        if(is.numeric(num)&&num>length(dt.so$Hospital.Name)) return (NA)
        if(is.numeric(num)) return (as.character(dt.so$Hospital.Name[num]))
        if(num=="best") return (as.character(dt.so$Hospital.Name[1]))
        if(num=="worst") return (as.character(dt.so$Hospital.Name[length(dt.so$Hospital.Name)]))   
}

