rm(list = ls())
getMKLthreads()
DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"

######################Task 4 ############################
#4. Cluster companies based on their financial metrics, 
# so that there is a spread of leaders and followers
##########################################################



# Import financial data
CompanyScore.Finial <- read.csv(paste(DataPath, "/FinialCompanyScore.csv", sep=""),
                                stringsAsFactors = F)
IncomeStatement <- read.csv(paste(DataPath, "/IncomeStatement_Filtered3.csv", sep=""),
                            stringsAsFactors = F)
BalanceSheet <- read.csv(paste(DataPath, "/BalanceSheet_Filtered3.csv", sep=""),
                         stringsAsFactors = F                         )
Cashflow <- read.csv(paste(DataPath, "/Cashflow_Filtered3.csv", sep=""),
                     stringsAsFactors = F)



### Check####
CommCol <- intersect(intersect(unique(names(IncomeStatement)),unique(names(BalanceSheet))), 
                     unique(names(Cashflow)))

ncol(IncomeStatement) + ncol(BalanceSheet) + ncol(Cashflow)


### For each ID check if there are repeated EndofPeriod
ToCheck <- Cashflow

ID2Check <- c()
for (m in unique(ToCheck$capiq_company_id)){
  Rows <- which(ToCheck$capiq_company_id == m)
  if (length(Rows) != length(unique(ToCheck$period[Rows]))){
    ID2Check <- c(ID2Check, m)
    print(m)
  }
}

Inds <- which(duplicated(ToCheck$period_end_date[Rows]))

ToCheck$period_end_date[Rows[Inds]]

ToCheck[Rows,c(1:5)]

#### Combine all Data ####
BalanceSheet$period <- as.numeric(substr(BalanceSheet$period, start = 3, stop = 8))
Cashflow$period <- as.numeric(substr(Cashflow$period, start = 3, stop = 8))
IncomeStatement$period <- as.numeric(substr(IncomeStatement$period, start = 3, stop = 8))

AllCol <- sort(union(union(names(BalanceSheet), names(IncomeStatement)), names(Cashflow)))
CommCompanyID <- unique(CompanyScore.Finial$CompanyID)


AllFinancialData <- data.frame()
n <-0
for (m in CommCompanyID){
  n<-n+1
  print(n)
  
  RowInd.BS <- which(BalanceSheet$capiq_company_id == m)
  RowInd.CF <- which(Cashflow$capiq_company_id == m)
  RowInd.IS <- which(IncomeStatement$capiq_company_id == m)
  
  # Find all Dates, where a company has published a report
  AllDate <- sort(unique(c(BalanceSheet$period[RowInd.BS], 
                                   Cashflow$period[RowInd.CF], 
                                   IncomeStatement$period[RowInd.IS])))
  
  # Combine all data of a company

  AllData.temp <- matrix(NA, nrow=length(AllDate), ncol = length(AllCol))
  rownames(AllData.temp) <- c(as.character(AllDate))
  colnames(AllData.temp) <- AllCol
  AllData.temp <- data.frame(AllData.temp)
  
  ColInd.BS <- match(names(BalanceSheet), AllCol) # for balance sheet
  AllData.temp[as.character(BalanceSheet$period[RowInd.BS]), ColInd.BS] <-
    BalanceSheet[RowInd.BS, ]
  
  ColInd.CF <- match(names(Cashflow), AllCol) # for Cashflow
  AllData.temp[as.character(Cashflow$period[RowInd.CF]), ColInd.CF] <-
    Cashflow[RowInd.CF, ]     
  
  ColInd.IS <- match(names(IncomeStatement), AllCol) # for income statement
  AllData.temp[as.character(IncomeStatement$period[RowInd.IS]), ColInd.IS] <-
    IncomeStatement[RowInd.IS, ]
  
  
  AllFinancialData <- rbind(AllFinancialData, AllData.temp)
}
# 
write.csv(AllFinancialData, paste(DataPath, "/AllFinancialData.csv", sep=""),
          quote = F, row.names = F)

head(AllFinancialData[, c(1:5, 200:206)])


Percentage <- 0.3 # Keep columns where the number of NAs < 30%
InputData <- AllFinancialData

NACols <- apply(InputData, MARGIN = 1, function(x, y=nrow(InputData)){
  length(which(is.na(x)))/y}
)
#
KeepInd <- match(names(NACols[which(NACols < Percentage)]), names(InputData))




BalanceSheet[head(which(BalanceSheet$capiq_company_id==21671), n =30), CommCol]




IncomeStatement[head(which(IncomeStatement$capiq_company_id==21671), n =30), CommCol]

Cashflow[which(Cashflow$capiq_company_id==21671), CommCol]



# c(2:8)
# 
# head(tail(c(1:ncol(InputData)), n = 4),2)
# 
# 
# 
# #########
# InputData <- Cashflow
# 
# head(colnames(InputData), n =10)
# tail(colnames(InputData), n =10)
# 
Percentage <- 0.3 # Keep columns where the number of NAs < 30%


NACols <- apply(InputData, MARGIN = 2, function(x, y=nrow(InputData)){
  length(which(is.na(x)))/y}
)
#
KeepInd <- match(names(NACols[which(NACols < Percentage)]), names(InputData))
# 
# InputData.Keep.temp <- InputData[,KeepInd]
# InputData.Keep <- InputData.Keep.temp[, -c(2:8)]
# 
# ###
# Col2Keep <- function(InputData, Percentage){
#   NACols <- apply(InputData, MARGIN = 2, function(x, y=nrow(InputData)){ 
#     length(which(is.na(x)))/y}
#   )
#   
#   KeepInd <- match(names(NACols[which(NACols < Percentage)]), names(InputData))
#   return(InputData[,KeepInd])
# }