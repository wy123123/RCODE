corr=function(directory,threshole=0){
  dir=directory;th=threshole;n=NA
  s=complete(dir,ids=1:332)[complete(dir,ids=1:332)[,2]>th,]
  s.id=formatC(s[,1],width=3,format ="d",flag="0")
  if(length(s[,1])==0){
    return (vector(mode="numeric", length=0))}
  else{
    for(i in 1 :length(s[,1])){
    dt=read.csv(paste("C:/Users/Lovebonito/Desktop/Programming in R/",dir,"/",s.id[i],".csv",sep=""),header=T)
    n[i]=cor(dt[(!is.na(dt$sulfate))*(!is.na(dt$nitrate))==1,]$sulfate,dt[(!is.na(dt$sulfate))*(!is.na(dt$nitrate))==1,]$nitrate)
    }
  }
  return (n)
}

