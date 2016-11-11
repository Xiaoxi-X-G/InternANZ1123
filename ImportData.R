rm(list = ls())

DataPath <- "C:/gxx/Database/internanz1123/debt_issuance"

#Import Data
# ColType <- c("int", #CompanyID 
#              "int", #componentID
#              "POSIXct", #StartDate
#              "chr",
#              "chr",
#              "POSIXct", #filingDate
#              "POSIXct",
#              "chr", #periodType
#              "num",
#              "num", #Rate
#              "int",
#              "int",
#              "int", #Seniority
#              "chr",
#              "chr",
#              "chr",
#              "chr",
#              "chr", "int")
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
