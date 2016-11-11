rm(list = ls())

DataPath <- "C:/gxx/Database/internanz1123/debt_issuance"

#Import Data
DebtBalance <- read.delim(file = paste(DataPath, "/debt_balance.tsv", sep = ""),
                          header = T,
                          stringsAsFactors = F,
                          quote = "")
