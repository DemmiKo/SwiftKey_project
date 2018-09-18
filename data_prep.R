#####################################################
# Data preparation for SwifKey project
#
# Dimitra 18/09/2018
#####################################################
library(data.table)
library(tm)

# Import data

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", temp)

# Unzip folder
unzip (zipfile = temp)
rm(temp)

# Read all files
list_of_files <- list.files(pattern = ".txt", recursive = TRUE)
datalist = lapply(list_of_files, FUN=readLines,stringsFactors = F ) 
t1 <- readLines(list_of_files[[1]])