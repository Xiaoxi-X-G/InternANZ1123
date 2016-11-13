rm(list = ls())
getMKLthreads()
DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"


# Run in SQL
library(RODBC)
odbcDataSources()
conn<-odbcConnect(dsn="localdb") 

DebtBalance2 <- sqlQuery(conn, "select companyId, componentId, convert(char(10),startDate,126) AS startDate, 
convert(char(10),filingDate, 126) AS filingDate, convert(char(10), periodEndDate,  126) AS PeriodEndDate, 
                         filingCurrency, issuedCurrency from debt_balance DB
                         where DB.companyId in 
                         (
                         select CompanyID from DebtIssueCompany2use
                         )",
                        stringsAsFactors = FALSE, as.is = TRUE, 
                        na.string = c("NULL", "NA", "", "NULL      "))

DebtBalance2$companyId <- as.integer(DebtBalance2$companyId)
DebtBalance2$componentId <- as.integer(DebtBalance2$componentId)

DebtBalance2$startDate <- as.Date(DebtBalance2$startDate)
DebtBalance2$filingDate <- as.Date(DebtBalance2$filingDate)
DebtBalance2$PeriodEndDate <- as.Date(DebtBalance2$PeriodEndDate)

DebtBalance2.order <- DebtBalance2[order(DebtBalance2$companyId,
                                         DebtBalance2$componentId,
                                         DebtBalance2$filingDate),]


#### Find market enty dates ###

