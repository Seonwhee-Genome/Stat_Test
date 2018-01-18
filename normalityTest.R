#! /data/Tools/system/R-3.4.3/bin/Rscript
#################################################################################
#										#
#			NormalityTest.R						#
#										#
#################################################################################
#######
# LOG #
#######
# developed by Kieun hyeong & Seonwhee Jin  (18/01/17) 
#################################################################################
if (!require("nortest")) install.packages("nortest")
if (!require("ggplot2")) install.packages("ggplot2")

input_dir = "C:/Users/labge/Documents/Code_Test/2017062214065-EF3-LT"
input_dir2 = "C:/Users/labge/Documents/Code_Test/2017121210154-EF3-LT"
source("./DB_for_normalityTest.R")
data=read.table(sprintf("%s/2017121210154-EF3-LT_segment.data", input_dir2), header=T, stringsAsFactors=FALSE, sep="\t", col.names=c("chr", "start_p", "stuff", "ratio"))

test_col <- individual_DB_read(input_dir)
test_col <- data.frame(test_col)
colnames(test_col) <- c("chr1", "chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13", "chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21", "chr22", "chrX", "chrY")
test_data <- test_col$chr1[is.na(test_col$chr1)==FALSE]
filtered_data <- Quantile_Filter(test_data)

filtered_sampled_data <- sample(filtered_data, 452)


save_BoxPlot(test_col, input_dir)


test_Name="Shapiro-Wilk test"
shapiro.test(test_col)$p.value

shapiro.test(test_data)
shapiro.test(filtered_data)
shapiro.test(filtered_sampled_data)
#cat(shapiro.test(test_col), "\n")

test_Name="Anderson-Darling test"
ad.test(test_data)$p.value
ad.test(filtered_data)$p.value
ad.test(filtered_sampled_data)$p.value
cat(test_Name)
cat(shapiro.test(test_col), "\n")

test_Name="Cramer-von mises test"
cvm.test(test_col)$p.value


cat(test_Name)
cat(shapiro.test(test_col), "\n")

test_Name="Lilliefors test(Kolmogorov-Smirnov)"
lillie.test(test_data)$p.value
lillie.test(test_data)$p.value
lillie.test(filtered_sampled_data)$p.value
cat(test_Name)
cat(shapiro.test(test_col))





































### Loading User defined function ###
source("/home/kieun/enfant_test/Script/tmp_develop_180109/usageOFstep3_v2.R")
###


### option check ###
opt=GetOpt()
mode=opt$version
test=opt$test
input=opt$input
###

data=read.table(input, header=T, stringsAsFactors=FALSE, sep="\t")
#class(data$Sample[1])
#head(data)
write(paste("now ttest run is : ", data$Sample[1], "\t", data$SegInterval[1], "\n",sep=""), stderr())		#print to stderr(2) to check what step error will be

### Get only data set of REF ###
ref_all_set <- data[-which(data$Group=="TEST"),]
TEST <- data[which(data$Group=="TEST"),1]
###

### How many REF Groups ###
ref_list=unique(ref_all_set$Group)
###

### t-TEST for every TEST & REF ###
p_values=c()

if (test == "student")
{
	for (each_ref in ref_list)
	{
		REF <- data[which(data$Group==each_ref),1]
		#p_values=c(p_values,t.test(TEST, REF)$p.value)
		p_values=c(p_values,-log(t.test(TEST, REF, var.equal= TRUE)$p.value))
	}
} else if(test == "welches")
{
	for (each_ref in ref_list)
	{
		REF <- data[which(data$Group==each_ref),1]
		p_values=c(p_values,-log(t.test(TEST, REF)$p.value))	# Welche's | t.test(a,b, var.equal = TRUE) : student t-test
	}
}

###

switch(mode, Median={representing_p=median(p_values)}, Min={representing_p=min(p_values)}, Max={representing_p=max(p_values)}, Mean={representing_p=mean(p_values)})

segInterval=data$SegInterval[1]
segSize=data$SegSize[1]
mark=data$Existenceof_DB[1]
sampleID=data$Sample[1]

#p_values
cat(paste(segInterval,segSize,representing_p,mark,sampleID,sep="\t"), "\n", sep="")

