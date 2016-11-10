##### Import data
rm(list = ls())

DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"

BalanceSheet <- read.delim(file = paste(DataPath, "/balance_sheet.tsv", sep = ""))
DebtBalance <- read.delim(file = paste(DataPath, "/debt_balance.tsv", sep = ""),
                          quote = "" ,
                          stringsAsFactors = F)


Metrics <- read.delim(file = paste(DataPath, "/metrics.tsv", sep = ""))