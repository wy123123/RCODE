specdata="C:/Users/Lovebonito/Desktop/Programming in R/specdata"
complete=function(directory,ids=1:332){
  dir=directory
  id=ids
  nobs=NA
  idc=formatC(id,width=3,format ="d",flag="0")
  for(i in 1 :length(idc)){
    dt=read.csv(paste("C:/Users/Lovebonito/Desktop/Programming in R/",dir,"/",idc[i],".csv",sep=""),header=T)
    nobs[i]=sum((!is.na(dt$sulfate))*(!is.na(dt$nitrate)))
  }
  return(data.frame(id,nobs))
}

