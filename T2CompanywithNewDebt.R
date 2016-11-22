getMKLthreads()
DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"

######################Task 2 ############################
# 2. Using debt_balance and debt_issuance, find all the 
# companies issuing debt in "new" markets (in currencies they 
# haven't issued debt in the last 3 years), and the dates in 
# which they issued debt. ("market_entries")
##########################################################
library(RODBC)


# Step1: Import debt balance and filter out companies using the companyIDs shared by all files
# Step2: Find new market entry dates of each companies
 


##### Step 1 #####
##### Import debt balance and filter out companies using the companyIDs shared by all files
odbcDataSources()
conn<-odbcConnect(dsn="localdb") 

DebtBalance2 <- sqlQuery(conn, "select DB.companyId,   convert(char(10),DB.filingDate, 126) AS filingDate, 
                         convert(char(10), DB.periodEndDate,  126) AS PeriodEndDate, 
                         DB.issuedCurrency AS IssuedCurrency from internanz1123.dbo.debt_balance DB
                         where DB.companyId in 
                         (
                         select distinct CIC.CompanyID from internanz1123.dbo.companyIDComm CIC
                         inner join internanz1123.dbo.DebtIssuance_Short DIS
                         on CIC.companyId = DIS.CompanyID
                         )
                         order by DB.companyId, DB.issuedCurrency, DB.periodEndDate
                         ",
           stringsAsFactors = FALSE, as.is = TRUE, 
           na.string = c("NULL", "NA", "", "NULL      "))


DebtBalance2$companyId <- as.integer(DebtBalance2$companyId)
DebtBalance2$filingDate <- as.Date(DebtBalance2$filingDate)
DebtBalance2$PeriodEndDate <- as.Date(DebtBalance2$PeriodEndDate)

DebtBalance2.order <- DebtBalance2[order(DebtBalance2$companyId,
                                         DebtBalance2$IssuedCurrency,
                                         DebtBalance2$PeriodEndDate),]


##### Step 2####
#### Find new market entry dates of each companies

#2.1 Remove rows where issuedCurrency = NA, around 3.3% of the rows
DebtBalance2.order <- DebtBalance2.order[-which(is.na(DebtBalance2.order$IssuedCurrency)),]

MarketEntryData <- data.frame()
m <-0
for (i in unique(DebtBalance2.order$companyId)){
  m<-m+1
  print(2401-m)
  
  RowInd <- which(DebtBalance2.order$companyId == i)
  
  matrix.currency <- c()
  matrix.currency <- DebtBalance2.order[RowInd, ] # all currencies and the issue dates (used by Period End Date)
  
  matrix.currency.order <- c() # ordered by types of currencies and PeriodEndDate
  matrix.currency.order <- matrix.currency[order(matrix.currency$IssuedCurrency,
                                                 matrix.currency$PeriodEndDate),]
  
  #2.2 write down first entry date of all currencies
  MatchInd1st.currency <- match(unique(matrix.currency.order$IssuedCurrency), matrix.currency.order$IssuedCurrency)
  df.firstdate <- c()
  df.firstdate <- matrix.currency.order[MatchInd1st.currency,]
  
  #2.3, if the same currency re-apprears, check if the difference is > 3 years
  for (j in unique(matrix.currency.order$IssuedCurrency)){
    RowInd.currency <- which(matrix.currency.order$IssuedCurrency == j)
    
    Ind.temp <- which(as.integer(diff(matrix.currency.order$PeriodEndDate[RowInd.currency])) > (365*3))
    
    if (length(Ind.temp) > 0){ #2.3 if > 3 years, keep another record 
      df.firstdate<- rbind(df.firstdate, matrix.currency.order[RowInd.currency[Ind.temp+1],]) 
    }
  }
  colnames(df.firstdate)[[3]] <- "MarketEntryDate"
  df.firstdate <- df.firstdate[order(df.firstdate$IssuedCurrency, df.firstdate$MarketEntryDate),]
  
  #2.4,, combiningn data 
  MarketEntryData <- rbind(MarketEntryData, df.firstdate)
}

