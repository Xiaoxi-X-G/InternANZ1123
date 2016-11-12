rm(list = ls())

DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"

DebtBalance.raw <- read.delim(file = paste(DataPath, "/debt_balance.tsv", sep = ""),
                          header = T,
                          stringsAsFactors = F,
                          quote = "",
                          na.strings = c("","NA", "NULL"))

# Find Debt Issue: The component ID, 
# where the Outstanding Balance of the last Filing date > first Filing DAte

DebtBalance.raw$filingDate <- as.POSIXct(DebtBalance.raw$filingDate)

DebtBalance.2use.temp <- data.frame(CompanyID = DebtBalance.raw$ï..companyId,
                               ComponentID = DebtBalance.raw$componentId,
                               filingDate = as.Date(DebtBalance.raw$filingDate),
                               OutstandingBalance = DebtBalance.raw$outstandingBalance,
                               stringsAsFactors=FALSE)

DebtBalance.2use <- DebtBalance.2use.temp[order(DebtBalance.2use.temp$CompanyID,
                                                DebtBalance.2use.temp$ComponentID,
                                                DebtBalance.2use.temp$filingDate),]


DebtBalance.2use <- DebtBalance.2use[-1,]

