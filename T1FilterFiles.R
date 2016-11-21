rm(list = ls())
getMKLthreads()
DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"

######################Task 1 ############################
# 1. Use "debt_balance" to filter the other files 
# to remove data not from companies with debt issuances.
##########################################################

library(RODBC)

#Step1: Find CompanyIDs by inner join  cashflow, balance_sheet, income_statement
#Step2: Import "CompanyIDComm" to SQL
#Step3: Filter all other files using CompanyIDComm & Balance_Issuance 
#Step4: Filter out "NULL" columns



####Step1 ####
###Find CompanyIDs by inner join  cashflow, balance_sheet, income_statement

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


####Step2#####
##Import "CompanyIDComm" to SQL 


####Step3 #####
#  Filter all other fills, CompanyIDComm & Balance_Issuance 
Cashflow.2use <- sqlQuery(conn, "select * from interanz1123.dbo.cashflow CF
                          where CF.capiq_company_id in
                          (
                          select distinct CIC.CompanyID from interanz1123.dbo.CompanyIDComm CIC
                          inner join  interanz1123.dbo.DebtIssuance_Short DIS
                          on CIC.CompanyID = DIS.CompanyID
                          )",
                          stringsAsFactors = FALSE, as.is = TRUE, 
                          na.string = c("NULL", "NA", ""))

# write.csv(Cashflow.2use, paste(DataPath, "/Cashflow_Filtered2.csv", sep = ""),
#           row.names = F, quote =  F)

###################

IncomeStatement.2use <- sqlQuery(conn, "select * from interanz1123.dbo.income_statement CF  where CF.capiq_company_id in
                                 (
                                 select distinct CIC.CompanyID from interanz1123.dbo.CompanyIDComm CIC
                                 inner join  interanz1123.dbo.DebtIssuance_Short DIS
                                 on CIC.CompanyID = DIS.CompanyID
                                 )",
                          stringsAsFactors = FALSE, as.is = TRUE, 
                          na.string = c("NULL", "NA", ""))

# write.csv(IncomeStatement.2use, paste(DataPath, "/IncomeStatement_Filtered2.csv", sep = ""),
#           row.names = F, quote =  F)

######################

BalanceSheet.2use <- sqlQuery(conn, "select * from interanz1123.dbo.balance_sheet CF  where CF.capiq_company_id in
                              (
                              select distinct CIC.CompanyID from interanz1123.dbo.CompanyIDComm CIC
                              inner join  interanz1123.dbo.DebtIssuance_Short DIS
                              on CIC.CompanyID = DIS.CompanyID
                              )",
                          stringsAsFactors = FALSE, as.is = TRUE, 
                          na.string = c("NULL", "NA", ""))

# write.csv(BalanceSheet.2use, paste(DataPath, "/BalanceSheet_Filtered2.csv", sep = ""),
#           row.names = F, quote =  F)


####Step4####
###Filter out "NULL" columns
Col2Keep <- function(InputData, Percentage){
  NACols <- apply(InputData, MARGIN = 2, function(x, y=nrow(InputData)){ 
    length(which(is.na(x)))/y}
  )
  
  KeepInd <- match(names(NACols[which(NACols < Percentage)]), names(InputData))
  return(InputData[,KeepInd])
}

# Delete the column, if more than 30% of rows are NULL

###
Cashflow_Filter3 <- Col2Keep(Cashflow.2use, Percentage=0.3)
IncomeStatement_Filter3 <- Col2Keep(IncomeStatement.2use, Percentage=0.3)
BalanceSheet_Filter3 <- Col2Keep(BalanceSheet.2use, Percentage=0.3)

# write.csv(Cashflow_Filter3, paste(DataPath, "/Cashflow_Filter3.csv", sep = ""),
#           row.names = F, quote =  F)
# write.csv(IncomeStatement_Filter3, paste(DataPath, "/IncomeStatement_Filter3.csv", sep = ""),
#           row.names = F, quote =  F)
# write.csv(BalanceSheet_Filter3, paste(DataPath, "/BalanceSheet_Filtered3.csv", sep = ""),
#           row.names = F, quote =  F)
