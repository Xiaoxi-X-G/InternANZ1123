rm(list = ls())

DataPath <- "C:/gxx/Database/internanz1123/debt_issuance"

DebtBalance <- read.delim(file = paste(DataPath, "/debt_balance.tsv", sep = ""),
                          header = T,
                          stringsAsFactors = F,
                          quote = "",
                          na.strings = c("","NA", "NULL"))

IncomeStatement<- read.delim(file = paste(DataPath, "/income_statement.tsv", sep = ""),
                          header = T,
                          stringsAsFactors = F,
                          quote = "",
                          na.strings = c("","NA", "NULL")) 
