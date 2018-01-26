library(ggplot2)
library(outliers)
library(reshape2)
set.seed(1123)


#################### sample X bin ##############################################
csvFile <- "C:/Users/labge/Documents/OUT_window_matrix100.txt"
manualcheck_list <- read.csv(csvFile, sep='\t')
new_checklist <- melt(manualcheck_list, id="bin")

meanList <- list()
medianList <- list()

for (i in 2:length(manualcheck_list$bin)){
  meanList[[length(meanList)+1]] <- mean(manualcheck_list[,i])
  medianList[[length(medianList)+1]] <- median(manualcheck_list[,i])
  
}
mean <- data.frame(matrix(unlist(meanList), nrow = length(manualcheck_list$bin)))
StatList <- data.frame(matrix(unlist(meanList), nrow = length(manualcheck_list$bin)))
View(StatL)
p <- ggplot(data = new_checklist) + geom_point(aes(x=bin, y=value)) 
p <- p + geom_hline(yintercept = 0, aes(colour="Reference"))
p <- p + geom_point(data=meanList)
p
dev.off()


#################### bin X sample #############################################
#p <- ggplot(data = manualcheck_list) + geom_point(aes(x=775000.5, y=manualcheck_list$X1.775000.5)) + geom_point(aes(x=875000.5, y=manualcheck_list$X1.875000.5)) 

csvFileT <- "C:/Users/labge/Documents/transposed_OUT_window_matrix200.csv"
csvFileT_small <- "C:/Users/labge/Documents/transposed_OUT_window_matrix_small.csv"
manualcheck_listT <- read.csv(csvFileT, sep=',')
manualcheck_listT_small <- read.csv(csvFileT_small, sep=',')
source("./DB_for_normalityTest.R")

draw_Histogram_bin(manualcheck_list$X1.875000.5, -2, 2, 0.01)

Gfit <- pnorm(manualcheck_list$X1.875000.5)

density(Gfit)
dev.off()

ph <- ggplot(data = manualcheck_list) + geom_histogram(aes(x=X1.775000.5), binwidth = 0.01, fill="red", alpha=0.2) 
ph

unlist(meanList, use.names=FALSE)
