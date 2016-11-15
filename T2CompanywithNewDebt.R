rm(list = ls())
getMKLthreads()
DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"

######################Task 2 ############################
# 2. Using debt_balance and debt_issuance, find all the 
# companies issuing debt in "new" markets (in currencies they 
# haven't issued debt in the last 3 years), and the dates in 
# which they issued debt. ("market_entries")
##########################################################


# Step1: Import debt balance and filter out companies using the companyIDs shared by all files
# Step2: Find new market entry dates of each companies
# Step3: Rank the companies for each currency: A score in [0,1] is given for a company for each currency


# ####1. Import debt_balance, debt_issuance ####
# DebtBalance.raw <- read.delim(file = paste(DataPath, "/debt_balance.tsv", sep = ""),
#                               header = T,
#                               stringsAsFactors = F,
#                               quote = "",
#                               na.strings = c("","NA", "NULL"))
# 
# DebtIssuance.raw <- read.delim(file = paste(DataPath, "/debt_issuance.rpt", sep = ""),
#                                header = T,
#                                stringsAsFactors = F,
#                                quote = "",
#                                na.strings = c("","NA", "NULL"))
# # 
# # write.csv(DebtIssuance.raw[, c(1)], paste(DataPath, "/DebtIssuance_Short.csv", sep = ""),
# #           row.names = F, quote =  F)


# CompanyIDComm<- read.csv(file = paste(DataPath, "/CompanyIDComm.csv", sep=""), 
#                          header = T,
#                          stringsAsFactors = F)



##### Step 1 #####
##### Import debt balance and filter out companies using the companyIDs shared by all files
library(RODBC)
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


#####Step 2####
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


# ######Step3 ######
# #### Rank the companies for each currency: A score in [0,1] is given for a company for each currency
# 
# #3.1: Order all companyID by Currencies, and by Market Entry Date
# CompanyScore.temp <- MarketEntryData[order(MarketEntryData$IssuedCurrency,
#                                            MarketEntryData$MarketEntryDate,
#                                            MarketEntryData$companyId),
#                                      ]
# 
# #3.2 score each company across all currencies
# CompanyScore <- matrix(0, nrow = length(unique(CompanyScore.temp$companyId)),
#                        ncol = (1+ length(unique(CompanyScore.temp$IssuedCurrency))))
# 
# 
# colnames(CompanyScore) <- c("CompanyID",  
#                             unique(CompanyScore.temp$IssuedCurrency) )
# CompanyScore[,1] <- unique(CompanyScore.temp$companyId)
# 
# 
# for (i in unique(CompanyScore.temp$IssuedCurrency)){
#   #3.2.1: For each currency find all records
#   RowInd <- which(CompanyScore.temp$IssuedCurrency == i) # 
#   temp11 <-c()
#   temp11 <- CompanyScore.temp[RowInd, c(1,3)]
#   
#   #3.2.2: Rank and  give a score based on debts issue date
#   rank.temp <- rank(-as.integer(temp11$MarketEntryDate))  
#   temp11$Rank <- rank.temp/max(rank.temp) # normalize score to [0,1]
#   
#   #3.2.3: If a company issued multiple currencies, add-up the score 
#   Score <- tapply(temp11$Rank, INDEX = temp11$companyId, FUN = sum) 
#   RowInd.score <- match(names(Score), CompanyScore[,1]) # Find the companyID index for the given currency
#   
#   CompanyScore[RowInd.score, i] <- unname(Score) #
# }
# 
# #3.3: Find the total score of each company, based on which to order companyID
# df.CompanyScore <- data.frame(CompanyScore)
# df.CompanyScore$TotalScore <- rowSums(df.CompanyScore[,-1])
# 
# Finial.CompanyScore <- df.CompanyScore[order(df.CompanyScore$TotalScore, decreasing = T),]
# 
# plot(sort(Finial.CompanyScore$AUD, decreasing =T))
