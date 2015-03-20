specdata="C:/Users/Lovebonito/Desktop/Programming in R/specdata"
pollutantmean <- function(directory,pollutant,id=1:332){
  dir=directory;pol=pollutant;id=id
  id=formatC(id,width=3,format ="d",flag="0")
  dt=NA
  for(i in 1 : length(id)){
  dt=rbind(dt,read.csv(paste("C:/Users/Lovebonito/Desktop/Programming in R/",dir,"/",id[i],".csv",sep=""),header=T))
  }
  return(round(mean(dt[pol][,1],na.rm=T),3))
}


