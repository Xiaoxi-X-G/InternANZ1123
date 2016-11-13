rm(list = ls())
getMKLthreads()
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

head(DebtBalance.2use, n =40)

## Find company with debt issue
DebtIssueCompany <-c()
DebtIssueComponent <-c()
m <-0
for (i in unique(DebtBalance.2use$ComponentID)){
  m <- m + 1
  Ind <- which(DebtBalance.2use$ComponentID == i) # Find Index
  OutstandingBalance.all <- DebtBalance.2use$OutstandingBalance[Ind]
  if ((tail(OutstandingBalance.all, n = 1) - OutstandingBalance.all[1]) > 0){
    temp <- DebtBalance.2use$CompanyID[Ind[1]] # Find Compnay ID
    DebtIssueCompany <- c(DebtIssueCompany, temp) 
    DebtIssueComponent <- c(DebtIssueComponent, i)
    print(1252005-m)
  }
}

DebtIssueCompany <- unique(DebtIssueCompany)

#write.csv(as.integer(DebtIssueCompany), "DebtIssueCompany.csv", row.names = F)


### Filter out companies without debt issuance ####
# 1. Load all others files
# 2. find company ID included in all files: Income Statement, Balance Sheet, Cashflow
# 3. filter out company based on 2
# 4. Clean Null & duplicated columns

# 1.1 Cashflow
Cashflow.raw <- read.delim(file = paste(DataPath, "/cashflow.tsv", sep = ""),
                              header = T,
                              stringsAsFactors = F,
                              quote = "",
                              na.strings = c("","NA", "NULL"))

# 1.2 Income Statement
IncomeStatement.raw <- read.delim(file = paste(DataPath, "/income_statement.tsv", sep = ""),
                           header = T,
                           stringsAsFactors = F,
                           quote = "",
                           na.strings = c("","NA", "NULL"))

# 1.3 Balance Sheet
BalanceSheet.raw <- read.delim(file = paste(DataPath, "/balance_sheet.tsv", sep = ""),
                                  header = T,
                                  stringsAsFactors = F,
                                  quote = "",
                                  na.strings = c("","NA", "NULL"))

# 2. company IDs that are availabe for all files

# DebtIssueCompany.2use <- intersect(intersect(intersect(DebtIssueCompany, unique(Cashflow.raw$ï..capiq_company_id)), 
#                                              unique(IncomeStatement.raw$ï..capiq_company_id)), 
#                                    unique(BalanceSheet.raw$capiq_company_id) )

# #### Out of Memory,  Run in SQL
library(RODBC)
odbcDataSources()

conn<-odbcConnect(dsn="localdb") 


DebtIssueCompany.2use <- sqlQuery(conn, "select distinct CF.capiq_company_id AS DebtIssueCompany2use from cashflow CF 
inner join DebtIssueCompany DC
                                  on DC.CompanyID = CF.capiq_company_id
                                  inner join balance_sheet BS
                                  on DC.CompanyID = BS.capiq_company_id
                                  inner join income_statement IST
                                  on DC.CompanyID = IST.capiq_company_id")

DebtIssueCompany.2use <- as.vector(DebtIssueCompany.2use)
DebtIssueCompany.2use <- unname(DebtIssueCompany.2use) 



# 3. filter out company based on 2

# Done in SQL
Cashflow.2use <- sqlQuery(conn, "select * from cashflow CF where CF.capiq_company_id in 
                          (
                          select CompanyID from DebtIssueCompany2use
                          )",
                          stringsAsFactors = FALSE, as.is = TRUE, 
                          na.string = c("NULL", "NA", ""))
# 
# write.csv(Cashflow.2use, paste(DataPath, "/Cashflow_DebtIssue.csv", sep = ""),
#           row.names = F, quote =  F)



IncomeStatement.2use <- sqlQuery(conn, "select * from income_statement CF where CF.capiq_company_id in 
                          (
                          select CompanyID from DebtIssueCompany2use
                          )",
                          stringsAsFactors = FALSE, as.is = TRUE, 
                          na.string = c("NULL", "NA", ""))

 # write.csv(IncomeStatement.2use, paste(DataPath, "/IncomeStatement_DebtIssue.csv", sep = ""),
 #           row.names = F, quote =  F)




BalanceSheet.2use <- sqlQuery(conn, "select * from balance_sheet CF where CF.capiq_company_id in 
                              (
                              select CompanyID from DebtIssueCompany2use
                              )",
                          stringsAsFactors = FALSE, as.is = TRUE, 
                          na.string = c("NULL", "NA", ""))

# write.csv(BalanceSheet.2use, paste(DataPath, "/BalanceSheet_DebtIssue.csv", sep = ""),
#           row.names = F, quote =  F)
  
  
  
  