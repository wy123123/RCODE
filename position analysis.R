POSITION=read.csv("C:/Users/Lovebonito/Downloads/PositionAnalysis/Position Analysis.csv",header=F,sep=",")
wilcox.test(POSITION$V2,POSITION$V3)
