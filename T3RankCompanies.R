rm(list = ls())
getMKLthreads()
DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"

######################Task 3 ############################
#3. Using "market_entries", rank companies based on the timing 
# of the entrance into that market (this rank will be your 
# leadership score
##########################################################


### Step1: Order all companyID by Currencies, and by Market Entry Date
### Step2: Rank the companies for each currency: A score in [0,1] is given for a company for each currency
### Step3: Find the total score of each company, based on which to order companyID



######Step1 ######
#Order all companyID by Currencies, and by Market Entry Date
CompanyScore.temp <- MarketEntryData[order(MarketEntryData$IssuedCurrency,
                                           MarketEntryData$MarketEntryDate,
                                           MarketEntryData$companyId),
                                     ]
######Step2 ######
#Rank the companies for each currency: A score in [0,1] is given for a company for each currency
CompanyScore <- matrix(0, nrow = length(unique(CompanyScore.temp$companyId)),
                       ncol = (1+ length(unique(CompanyScore.temp$IssuedCurrency))))


colnames(CompanyScore) <- c("CompanyID",  
                            unique(CompanyScore.temp$IssuedCurrency) )
CompanyScore[,1] <- unique(CompanyScore.temp$companyId)



for (i in unique(CompanyScore.temp$IssuedCurrency)){
  #2.1: For each currency find all records
  RowInd <- which(CompanyScore.temp$IssuedCurrency == i) # 
  temp11 <-c()
  temp11 <- CompanyScore.temp[RowInd, c(1,3)]
  
  #2.2: Rank and  give a score based on debts issue date
  rank.temp <- rank(-as.integer(temp11$MarketEntryDate))  
  temp11$Rank <- rank.temp/max(rank.temp) # normalize score to [0,1]
  
  #2.3: If a company issued multiple currencies, add-up the score 
  Score <- tapply(temp11$Rank, INDEX = temp11$companyId, FUN = sum) 
  RowInd.score <- match(names(Score), CompanyScore[,1]) # Find the companyID index for the given currency
  
  CompanyScore[RowInd.score, i] <- unname(Score) # 
}


######Step3 ######
# Find the total score of each company, based on which to order companyID
df.CompanyScore <- data.frame(CompanyScore)
df.CompanyScore$TotalScore <- rowSums(df.CompanyScore[,-1])

Finial.CompanyScore <- df.CompanyScore[order(df.CompanyScore$TotalScore, decreasing = T),]

# write.csv(Finial.CompanyScore, file = paste(DataPath, "/FinialCompanyScore.csv", sep=""),
#       quote = F, row.names =  F)

plot(sort(Finial.CompanyScore$TotalScore, decreasing =T))



