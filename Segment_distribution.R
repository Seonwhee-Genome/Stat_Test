library(ggplot2)

csvFile <- "C:/Users/labge/Documents/NormalList_data.csv"
manualcheck_list <- read.csv(csvFile, sep=',')

View(chr1)
#p <- ggplot(data = subset(manualcheck_list, chrom==1)) + geom_dotplot(aes(x=loc.start, y=seg.mean, color="red", size=0.5)) +
#  geom_dotplot(aes(x=loc.end, y=seg.mean, color="green", size=0.5))
chr1 <- subset(manualcheck_list, chrom==8)

p <- ggplot(data = chr1) + geom_segment(aes(x=loc.start, y = seg.mean, xend=loc.end, yend=seg.mean)) + geom_point(aes(x=loc.start, y = seg.mean, color = "start")) + geom_point(aes(x=loc.end, y = seg.mean, color="end"))
#p <- ggplot(data = chr1) + geom_segment(aes(x=loc.start, y = seg.mean, xend=loc.end, yend=seg.mean)) + geom_point(aes(x=loc.start, y = seg.mean, color = "start"))                                        
p

chr3 <- subset(manualcheck_list, chrom==3)
p <- ggplot(data = chr3) + geom_segment(aes(x=loc.start, y = seg.mean, xend=loc.end, yend=seg.mean)) + geom_point(aes(x=loc.start, y = seg.mean, color = "start")) + geom_point(aes(x=loc.end, y = seg.mean, color="end"))                                      
p
dev.off()

write.csv(chr1, file = "C:/Users/labge/Desktop/앙팡가드/앙팡DB_with형주임님/Chr8Normal.csv", sep = ',')
