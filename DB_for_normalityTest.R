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

if (!require("DBI")) install.packages("DBI")
if (!require("RSQLite")) install.packages("RSQLite")

individual_DB_make <- function(DB_dir, inputData){
  ##############################################
  ## Save split the segment data by chromosome numbers and save into DB
  ##############################################
  db <- SQLite()
  connect <- dbConnect(db, sprintf("%s/segment.data.db", DB_dir))
  dbWriteTable(connect, "segment_data", data)
  for (i in 1:24){
    test_table_chr <- dbSendQuery(connect, sprintf("SELECT * FROM segment_data WHERE chr = %d", i))
    test_table <- dbFetch(test_table_chr)
    dbClearResult(test_table_chr)
    dbWriteTable(connect, sprintf("chr%d",i), test_table)
  }
  dbDisconnect(connect)
}

individual_DB_read <- function(DB_dir){
  #########################################
  ## Return data.frame of ratio values
  #########################################
  db <- SQLite()
  connect <- dbConnect(db, sprintf("%s/segment.data.db", DB_dir))
  for (i in 1:24) {
    test_table <- dbReadTable(connect, sprintf("chr%d",i))
    
    if (i != 1){
      test_col_2 = test_table$ratio
      test_col <- cbind(test_col, test_col_2)
    }
    else{
      
      test_col=test_table$ratio
    }
  }
  dbDisconnect(connect)
  return(test_col)
}

individual_DB_read_filtered <- function(DB_dir){
  #########################################
  ## Return data.frame of ratio values
  #########################################
  db <- SQLite()
  connect <- dbConnect(db, sprintf("%s/segment.data.db", DB_dir))
  
  for (i in 1:24) {
    test_table_filtered <- dbSendQuery(connect, sprintf("SELECT * FROM chr%d WHERE ratio < 0.55 AND ratio > -0.55", i))
    test_table <- dbFetch(test_table_filtered)
    dbClearResult(test_table_filtered)
    
    if (i != 1){
      test_col_2 = test_table$ratio
      test_col <- cbind(test_col, test_col_2)
    }
    else{
      
      test_col=test_table$ratio
    }
  }
  dbDisconnect(connect)
  return(test_col)
}
##################################################
Quantile_Filter <- function(origin_data){
  
  q25 = quantile(origin_data)[2]
  q75 = quantile(origin_data)[4]
  IQR = q75 - q25
  print(q25 - 1.5*IQR)
  print(q75 + 1.5*IQR)
  filtered_data <- origin_data[origin_data >= q25 - 1.5*IQR & origin_data <= q75 + 1.5*IQR]
  return(filtered_data)
  
}
#################################################
save_BoxPlot <- function(input_data, output_dir){
  jpeg(sprintf('%s/boxplotall.jpg', output_dir))
  boxplot(output_data)
  dev.off()
}

################################################
draw_Histogram <- function(input_data){
  hist(input_data, breaks=seq(from=-1.5, to=1.5, by=0.01))
  
}