changeColour=function(x){
  for(i in 1 :400){
    if(as.vector(x)==as.vector(colourCode_3$colourCode_2[i]))
        return(as.vector(colourCode_3$colour[i]))
  }
}
colour=sapply(as.vector(QTC_4$colourIndicator),function(x) changeColour(x))
colour=data.frame(unlist(colour),ncol=1)
