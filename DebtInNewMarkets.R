rm(list = ls())
getMKLthreads()
#DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"
DataPath <-  "C:/gxx/Database/internanz1123/debt_issuance"


# Run in SQL
library(RODBC)
odbcDataSources()
conn<-odbcConnect(dsn="localdb") 

DebtBalance2 <- sqlQuery(conn, "select DB.companyId, DB.componentId, convert(char(10),DB.startDate,126) AS startDate, 
convert(char(10),DB.filingDate, 126) AS filingDate, 
                         convert(char(10), DB.periodEndDate,  126) AS PeriodEndDate, 
                         DB.filingCurrency, DB.issuedCurrency,DB.outstandingBalance
                         from interanz1123.dbo.debt_balance DB
                         where DB.componentId in 
                         (
                         select DII.ComponentID from interanz1123.dbo.DebtIssueInfo DII
                         )
                         order by DB.companyId, DB.componentId, DB.filingDate
                         ",
                        stringsAsFactors = FALSE, as.is = TRUE, 
                        na.string = c("NULL", "NA", "", "NULL      "))

DebtBalance2$companyId <- as.integer(DebtBalance2$companyId)
DebtBalance2$componentId <- as.integer(DebtBalance2$componentId)

DebtBalance2$startDate <- as.Date(DebtBalance2$startDate)
DebtBalance2$filingDate <- as.Date(DebtBalance2$filingDate)
DebtBalance2$PeriodEndDate <- as.Date(DebtBalance2$PeriodEndDate)

DebtBalance2$outstandingBalance <- format(round(DebtBalance2$outstandingBalance, 2), nsmall = 1)

DebtBalance2.order <- DebtBalance2[order(DebtBalance2$companyId,
                                         DebtBalance2$componentId,
                                         DebtBalance2$filingDate),]


#### Find  dates ###
# Remove rows where issuedCurrency = NA, around 2.6% of the rows
DebtBalance2.order <- DebtBalance2.order[-which(is.na(DebtBalance2.order$issuedCurrency)),]

ColInd <- c(1,2,4,6, 7) #"CompanyID", "ComponentID", "IssuedDate", "FilingCurrency", issuedCurrency


MarketEntryData <- data.frame()

m <-0
for (i in unique(DebtBalance2.order$companyId)){
  m<-m+1
  print(33182-m)
  
  RowInd <- which(DebtBalance2.order$companyId == i)
  UniqueComponent <- unique(DebtBalance2.order$componentId[RowInd])
  MatchInd1st <- match(UniqueComponent, DebtBalance2.order$componentId) 
  
  #1 For each componentID of a company, find Currencies and the first dates
  matrix.currency <- DebtBalance2.order[MatchInd1st, ColInd] 
  
  #2 Find entry dates which is the date a compnay hasn't issued debts of a currency in last three years
  matrix.currency.order <- matrix.currency[order(matrix.currency$issuedCurrency,
                                                 matrix.currency$filingDate),]
  
  #3.1 write down first enty date of all currency
  MatchInd1st.currency <- match(unique(matrix.currency.order$issuedCurrency), matrix.currency.order$issuedCurrency)
  df.firstdate <- matrix.currency.order[MatchInd1st.currency,]
  
  #3.2 if the same currency re-apprears, check if the difference is > 3 years
  for (j in unique(matrix.currency.order$issuedCurrency)){
    RowInd.currency <- which(matrix.currency.order$issuedCurrency == j)
    
    Ind.temp <- which(as.integer(diff(matrix.currency.order$filingDate[RowInd.currency])) > (365*3))
    
    if (length(Ind.temp) > 0){
      df.firstdate<- rbind(df.firstdate, matrix.currency.order[RowInd.currency[Ind.temp+1],]) 
    }
  }
  colnames(df.firstdate)[[3]] <- "MarketEntryDate"
  df.firstdate <- df.firstdate[order(df.firstdate$issuedCurrency, df.firstdate$MarketEntryDate),]
  
  MarketEntryData <- rbind(MarketEntryData, df.firstdate)
}

#write.csv(MarketEntryData, "MarketEntryData.csv", row.names = F)



