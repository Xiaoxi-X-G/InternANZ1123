rm(list = ls())
getMKLthreads()
DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"

######################Task 1 ############################
# 2. Using debt_balance and debt_issuance, find all the 
# companies issuing debt in "new" markets (in currencies they 
# haven't issued debt in the last 3 years), and the dates in 
# which they issued debt. ("market_entries")
##########################################################


####Import debt_balance, debt_issuance ####
DebtBalance.raw <- read.delim(file = paste(DataPath, "/debt_balance.tsv", sep = ""),
                              header = T,
                              stringsAsFactors = F,
                              quote = "",
                              na.strings = c("","NA", "NULL"))

DebtIssuance.raw <- read.delim(file = paste(DataPath, "/debt_issuance.rpt", sep = ""),
                              header = T,
                              stringsAsFactors = F,
                              quote = "",
                              na.strings = c("","NA", "NULL"))
# 
# write.csv(DebtIssuance.raw[, c(1)], paste(DataPath, "/DebtIssuance_Short.csv", sep = ""),
#           row.names = F, quote =  F)


CompanyIDComm<- read.csv(file = paste(DataPath, "/CompanyIDComm.csv", sep=""), 
                         header = T,
                         stringsAsFactors = F)


length(unique(DebtBalance.raw$ï..companyId))
length(unique(DebtIssuance.raw$ï..companyId))

length(intersect(unique(DebtBalance.raw$ï..companyId), unique(DebtIssuance.raw$ï..companyId)))

length(intersect(unique(CompanyIDComm$DebtIssueCompany), unique(DebtIssuance.raw$ï..companyId)))