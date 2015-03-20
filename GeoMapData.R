#Geo Map data
library(data.table)
MasterFile=data.table(read.csv("C:/Users/Lovebonito/Downloads/22222222.csv",header=TRUE,sep=","))
DT=MasterFile[substring(MasterFile$Custom.Variable..Value.04.,1,1)=="S",]
write.csv(DT,file="ss.csv")
TEST=strsplit(as.vector(DT$Custom.Variable..Value.04.),";")
TEST_1=data.frame(matrix(unlist(TEST),ncol=4,byrow=T))
table(TEST_1$X1)


require(RColorBrewer)
library(RgoogleMaps)
plotclr <-brewer.pal(8,"YlOrRd")
plotclr = AddAlpha(plotclr,0.5)
data(lat.lon.meuse, package="loa", envir = environment())
#map <- GetMap.bbox(bb$lonR, bb$latR, destfile = filename, maptype="mobile", SCALE = 2);
map <- GetMap(center=c(1.3620228,103.8189802), zoom=11,
              size=c(640,400),destfile = file.path(tempdir(),"meuse.png"),
              maptype="mobile", SCALE = 1);


ColorMap(map=map)

map
1.3620228,103.8189802

getGeoCode("Brooklyn")
getGeoCode("730825")
#You can run this on the entire column of a data frame or a data table:
DF = cbind.data.frame(address=c("Berlin,Germany", "Princeton,NJ",
                                "cadillac+mountain+acadia+national+park"), lat = NA, lon = NA)
DF <- with(DF, data.frame(address, t(sapply(DF$address, getGeoCode))))
