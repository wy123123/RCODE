rankall <- function(outcome, num = "best") {
        outcome=outcome;num=num
        dt=read.csv("C:/Users/Lovebonito/Desktop/Programming in R/outcome-of-care-measures.csv")
        states=sort(as.vector(unique(dt$State)))
        if(outcome!="pneumonia"&&outcome!="heart attack"&&outcome!="heart failure"){
                stop("invalid outcome")
        }
        dt.s=dt[order(dt$Hospital.Name,as.character(dt$State)),]
        if(outcome=="heart attack"){
                temp="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        }
        else if(outcome=="heart failure"){
                temp="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        }
        else{
                temp="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
        worst=NA;hospital=NA
        dt.so=dt.s[order(as.numeric(as.character(dt.s[,temp])),dt.s$Hospital.Name),]
        dt.so=dt.so[dt.so[,temp]!="Not Available",]
        if(num=="best"){
                for(i in 1: length(states)){
                        d=dt.so[dt.so$State==states[i],]
                        hospital[i]=as.vector(d$Hospital.Name[1])
                }
                return (data.frame(hospital=hospital,state=states))
        }
        if(num=="worst"){
                for(i in 1: length(states)){
                        d=dt.so[dt.so$State==states[i],]
                        worst[i]=tail(as.vector(d$Hospital.Name),n=1)
                }
                return (data.frame(hospital=worst,state=states))
        }
        if(is.numeric(num)){
                for(i in 1: length(states)){
                        d=dt.so[dt.so$State==states[i],]
                        if(num>length(d$Hospital.Name)){
                                hospital[i]=NA
                        }
                        else{
                                hospital[i]=as.vector(d$Hospital.Name[num])
                        }      
                }
                return (data.frame(hospital=hospital,state=states))
        }
}
