rm(list = ls())
getMKLthreads()
DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"

######################Task 1 ############################
# 1. Use "debt_balance" to filter the other files 
# to remove data not from companies with debt issuances.
##########################################################


####1. Find CompanyIDs by inner join  cashflow, balance_sheet, income_statement####

library(RODBC)
odbcDataSources()
conn<-odbcConnect(dsn="localdb") 

CompanyID.Common <- sqlQuery(conn, "select distinct CF.capiq_company_id AS DebtIssueCompany from cashflow CF
                             inner join debt_balance DC
                             on DC.companyId = CF.capiq_company_id
                             inner join balance_sheet BS
                             on DC.CompanyID = BS.capiq_company_id
                             inner join income_statement IST
                             on DC.CompanyID = IST.capiq_company_id
                             ",
                             stringsAsFactors = FALSE, as.is = TRUE, 
                             na.string = c("NULL", "NA", "", "NULL      "))


# write.csv(CompanyID.Common,paste(DataPath, "/CompanyIDComm.csv",sep=""), 
#           row.names = F, quote =  F)


####2. Import "CompanyIDComm" to SQL #####

####3. Filter all other fills, CompanyIDComm & Balance_Issuance  #####
Cashflow.2use <- sqlQuery(conn, "select * from cashflow CF
 where CF.capiq_company_id in
                          (
                          	select distinct CIC.CompanyID from CompanyIDComm CIC
	inner join  DebtIssuance_Short DIS
                          on CIC.CompanyID = DIS.CompanyID
                          )",
                          stringsAsFactors = FALSE, as.is = TRUE, 
                          na.string = c("NULL", "NA", ""))

# write.csv(Cashflow.2use, paste(DataPath, "/Cashflow_Filtered2.csv", sep = ""),
#           row.names = F, quote =  F)

###################

IncomeStatement.2use <- sqlQuery(conn, "select * from income_statement CF  where CF.capiq_company_id in
                          (
                                 select distinct CIC.CompanyID from CompanyIDComm CIC
                                 inner join  DebtIssuance_Short DIS
                                 on CIC.CompanyID = DIS.CompanyID
)",
                          stringsAsFactors = FALSE, as.is = TRUE, 
                          na.string = c("NULL", "NA", ""))

# write.csv(IncomeStatement.2use, paste(DataPath, "/IncomeStatement_Filtered2.csv", sep = ""),
#           row.names = F, quote =  F)

######################

BalanceSheet.2use <- sqlQuery(conn, "select * from balance_sheet CF  where CF.capiq_company_id in
                          (
                          	select distinct CIC.CompanyID from CompanyIDComm CIC
	inner join  DebtIssuance_Short DIS
                          on CIC.CompanyID = DIS.CompanyID
                          )",
                          stringsAsFactors = FALSE, as.is = TRUE, 
                          na.string = c("NULL", "NA", ""))

# write.csv(BalanceSheet.2use, paste(DataPath, "/BalanceSheet_Filtered2.csv", sep = ""),
#           row.names = F, quote =  F)

