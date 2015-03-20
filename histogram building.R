datatest=read.csv("C:/Users/Lovebonito/Downloads/test1.csv",header=TRUE,sep=",")
colnames(datatest)
datatest=datatest[-14,]

library(ggplot2)
count <- data.frame(Product.Category=factor(datatest$Product.Category), 
                          Quantity=datatest$Quantity, 
                         AgeGroup=factor(datatest$Age))
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(9, "Set1"))
ngroups <- length(unique(count_group$group))

p <- qplot(Product.Category, Quantity, 
           data = count, 
           geom = "histogram", 
           stat = "bin2d",
           fill = AgeGroup,
           xlab = "Product.Category", 
           ylab = "Quantity",
           
)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 14))
p <- p + scale_fill_manual(values = cols(ngroups))
p
table(datatest$Age)
