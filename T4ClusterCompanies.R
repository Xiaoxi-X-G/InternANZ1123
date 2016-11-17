rm(list = ls())
getMKLthreads()
DataPath <- "C:/gxx/Database/internanz1123/debt_issuance"

######################Task 4 ############################
#4. Cluster companies based on their financial metrics, 
# so that there is a spread of leaders and followers
##########################################################



##### Step1 ####
# Import financial data, and combine into single file

### 1.1 Import Data
CompanyScore.Finial <- read.csv(paste(DataPath, "/FinialCompanyScore.csv", sep=""),
                                stringsAsFactors = F)
IncomeStatement <- read.csv(paste(DataPath, "/IncomeStatement_Filtered3.csv", sep=""),
                            stringsAsFactors = F)
BalanceSheet <- read.csv(paste(DataPath, "/BalanceSheet_Filtered3.csv", sep=""),
                         stringsAsFactors = F                         )
Cashflow <- read.csv(paste(DataPath, "/Cashflow_Filtered3.csv", sep=""),
                     stringsAsFactors = F)



### 1.2 For each CompanyID, check if there are repeated Period --- NO
ToCheck <- IncomeStatement

ID2Check <- c()
for (m in unique(ToCheck$capiq_company_id)){
  Rows <- which(ToCheck$capiq_company_id == m)
  if (length(Rows) != length(unique(ToCheck$period[Rows]))){
    ID2Check <- c(ID2Check, m)
    print(m)
  }
}



###1.3 Combine Data, using CompanyID and Period
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

# write.csv(AllFinancialData, paste(DataPath, "/AllFinancialData.csv", sep=""),
#           quote = F, row.names = F)


AllFinancialData <- read.csv(paste(DataPath, "/AllFinancialData.csv", sep=""),
                             stringsAsFactors = F)

####Step2#######
### Delete duplicated columns, and columns with high correlation  
library(caret)

AllFinancialData.filtered <- 
  AllFinancialData[, c(1: which(colnames(AllFinancialData)=="period"))]

AllFinancialData.filtered$filing_date <- 
  as.integer(as.Date(AllFinancialData.filtered$filing_date))

AllFinancialData.filtered <- AllFinancialData.filtered[, -c(2:6)]

correlationMatrix <- cor(AllFinancialData.filtered, 
                         use = "pairwise.complete.obs")

AllFinancialData.filtered2 <- 
  AllFinancialData.filtered[-findCorrelation(correlationMatrix, cutoff=0.75)] 

AllFinancialData.filtered2.order <- 
  cbind(AllFinancialData.filtered2[, ncol(AllFinancialData.filtered2)],
        AllFinancialData.filtered2[, c(1: (ncol(AllFinancialData.filtered2)-1))])

colnames(AllFinancialData.filtered2.order)[[1]] <- "period"

# write.csv(AllFinancialData.filtered2.order, paste(DataPath, "/AllFinancialData.filtered2.order.csv", sep=""),
#           quote = F, row.names = F)


AllFinancialData.filtered2.order <- 
  read.csv(paste(DataPath, "/AllFinancialData_filtered2_order.csv", sep= ""), stringsAsFactors = F)

### check, common columns  
length(intersect(colnames(Cashflow),colnames(AllFinancialData.filtered2)))
length(intersect(colnames(BalanceSheet),colnames(AllFinancialData.filtered2)))
length(intersect(colnames(IncomeStatement),colnames(AllFinancialData.filtered2)))


## highest score
AllFinancialData.filtered2.order[which(AllFinancialData.filtered2.order$capiq_company_id == "3648306"),]

## lowest score
AllFinancialData.filtered2.order[which(AllFinancialData.filtered2.order$capiq_company_id == "3116362"),]



### Re-design matrix2 for display in Tableau

CompanyScore.Finial.Redesinged <- data.frame()
yy <- 0
for (w in 1:nrow(CompanyScore.Finial)){
  yy <- yy + 1 
  IndName <- colnames(CompanyScore.Finial)[which(CompanyScore.Finial[w,] > 0)]
  IndValue <- CompanyScore.Finial[w, IndName]
  lg <- length(IndValue)
  df.temp <- data.frame(CompanyID = rep(as.integer(unname(IndValue[1])), (lg-1)), 
                        Currency = tail(names(IndValue), n =(lg-1)), 
                        Score= tail(as.numeric(unname(IndValue)), n =(lg-1)),
                        stringsAsFactors = F)
  
  CompanyScore.Finial.Redesinged<-rbind(CompanyScore.Finial.Redesinged, df.temp)
  
}
# write.csv(CompanyScore.Finial.Redesinged, paste(DataPath, "/CompanyScore_Finial_Redesinged.csv", sep=""),
#           quote = F, row.names = F)



### Re-design matrix2 for display in Tableau
AllFinancialData.filtered2.order.redesign <- data.frame()
v <- 0
for (v in 1:nrow(AllFinancialData.filtered2.order)){
  v <- v+1
  print(121070-v)
  
  IndName2 <- 
    colnames(AllFinancialData.filtered2.order)[which(!is.na(AllFinancialData.filtered2.order[v,]) )]
  
  IndValue2 <- AllFinancialData.filtered2.order[v, IndName2]
  lg2 <- length(IndValue2)
  df.temp2 <- data.frame(CompanyID = rep(as.integer(unname(IndValue2[2])), (lg2-2)),
                         Period = rep(as.numeric(unname(IndValue2[1])), (lg2-2)),
                         Terms = tail(names(IndValue2), n =(lg2-2)) ,
                         Value =  tail(as.numeric(unname(IndValue2)), n =(lg2-2)),
                         stringsAsFactors = F)
  
  AllFinancialData.filtered2.order.redesign <- rbind(AllFinancialData.filtered2.order.redesign,
                                                     df.temp2)
}
write.csv(AllFinancialData.filtered2.order.redesign, 
          paste(DataPath, "/AllFinancialData.filtered2.order.redesign.csv", sep=""),
          quote = F, row.names = F)