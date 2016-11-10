##### Import data
rm(list = ls())

DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"


##
BalanceSheet <- read.delim(file = paste(DataPath, "/balance_sheet.tsv", sep = ""))
DebtBalance <- read.delim(file = paste(DataPath, "/debt_balance.tsv", sep = ""),
                          quote = "" ,
                          stringsAsFactors = F)

Cashflow <- read.delim(file = paste(DataPath, "/cashflow.tsv", sep = ""),
                                      quote = "" ,
                                      stringsAsFactors = F)

IncomeStatement <- read.delim(file = paste(DataPath, "/income_statement.tsv", sep = ""),
                       quote = "" ,
                       stringsAsFactors = F)

Metrics <- read.delim(file = paste(DataPath, "/metrics.tsv", sep = ""))