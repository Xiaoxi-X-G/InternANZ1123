rm(list = ls())
getMKLthreads()
DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"

######################Task 2 ############################
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




### filter debt balance
library(RODBC)
odbcDataSources()
conn<-odbcConnect(dsn="localdb") 

DebtBalance2 <- sqlQuery(conn, "select DB.companyId,   convert(char(10),DB.filingDate, 126) AS filingDate, 
                         convert(char(10), DB.periodEndDate,  126) AS PeriodEndDate, 
                         DB.issuedCurrency AS IssuedCurrency from interanz1123.dbo.debt_balance DB
                         where DB.companyId in 
                         (
                         select distinct CIC.CompanyID from interanz1123.dbo.companyIDComm CIC
                         inner join interanz1123.dbo.DebtIssuance_Short DIS
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


#### Find  dates ####
# Remove rows where issuedCurrency = NA, around 2.6% of the rows
DebtBalance2.order <- DebtBalance2.order[-which(is.na(DebtBalance2.order$IssuedCurrency)),]

ColInd <- c(1,2,4,6, 7) #"CompanyID", "filingDate", ""PeriodEndDate", issuedCurrency


MarketEntryData <- data.frame()
m <-0
m <-0
for (i in unique(DebtBalance2.order$companyId)){
  m<-m+1
  print(2401-m)
  
  RowInd <- which(DebtBalance2.order$companyId == i)
  # UniqueComponent <- unique(DebtBalance2.order$componentId[RowInd])
  # MatchInd1st <- match(UniqueComponent, DebtBalance2.order$componentId) 
  
  #1 For each componentID of a company, find Currencies and the first dates
  matrix.currency <- DebtBalance2.order[RowInd, ] 
  
  #2 Find entry dates which is the date a compnay hasn't issued debts of a currency in last three years
  matrix.currency.order <- matrix.currency[order(matrix.currency$IssuedCurrency,
                                                 matrix.currency$PeriodEndDate),]
  
  #3.1 write down first enty date of all currency
  MatchInd1st.currency <- match(unique(matrix.currency.order$IssuedCurrency), matrix.currency.order$IssuedCurrency)
  df.firstdate <- matrix.currency.order[MatchInd1st.currency,]
  
  #3.2 if the same currency re-apprears, check if the difference is > 3 years
  for (j in unique(matrix.currency.order$IssuedCurrency)){
    RowInd.currency <- which(matrix.currency.order$IssuedCurrency == j)
    
    Ind.temp <- which(as.integer(diff(matrix.currency.order$PeriodEndDate[RowInd.currency])) > (365*3))
    
    if (length(Ind.temp) > 0){
      df.firstdate<- rbind(df.firstdate, matrix.currency.order[RowInd.currency[Ind.temp+1],]) 
    }
  }
  colnames(df.firstdate)[[3]] <- "MarketEntryDate"
  df.firstdate <- df.firstdate[order(df.firstdate$IssuedCurrency, df.firstdate$MarketEntryDate),]
  
  MarketEntryData <- rbind(MarketEntryData, df.firstdate)
}


######Rank ######
CompanyScore.temp <- MarketEntryData[order(MarketEntryData$IssuedCurrency,
                                           MarketEntryData$MarketEntryDate,
                                           MarketEntryData$companyId),
                                     ]


# score each companies across all currencies
# i.e., CompnayID AED AMD AOA ARS AUD AZN BDT BEF BGN

CompanyScore <- matrix(0, nrow = length(unique(CompanyScore.temp$companyId)),
                       ncol = (1+ length(unique(CompanyScore.temp$IssuedCurrency))))


colnames(CompanyScore) <- c("CompnayID",  
                            unique(CompanyScore.temp$IssuedCurrency) )
CompanyScore[,1] <- unique(CompanyScore.temp$companyId)


for (i in unique(CompanyScore.temp$IssuedCurrency)){
  RowInd <- which(CompanyScore.temp$IssuedCurrency == i) # for each currency find all records
  temp11 <- CompanyScore.temp[RowInd, c(1,3)]
  rank.temp <- rank(-as.integer(temp11$MarketEntryDate))
  temp11$rank <- rank.temp/max(rank.temp) # rank and score based on debts issue date, normalize to [0,1]
  
  Score <- tapply(temp11$rank, INDEX = temp11$companyId, FUN = sum)
  RowInd.score <- match(names(Score), CompanyScore[,1]) # Find the companyID index for the given currency
  
  CompanyScore[RowInd.score, i] <- unname(Score)
}

df.CompanyScore <- data.frame(CompanyScore)
df.CompanyScore$TotalScore <- rowSums(df.CompanyScore[,-1])

Finial.CompanyScore <- df.CompanyScore[order(df.CompanyScore$TotalScore, decreasing=T),]

plot(sort(Finial.CompanyScore$USD, decreasing =T))
