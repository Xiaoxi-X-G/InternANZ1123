rm(list = ls())
getMKLthreads()
#DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"
DataPath <-  "C:/gxx/Database/internanz1123/debt_issuance"

MarketEntryData <- read.csv(paste(DataPath,"/MarketEntryData.csv", sep = ""), stringsAsFactors = F)

MarketEntryData$MarketEntryDate <- as.Date(MarketEntryData$MarketEntryDate)


# rank all
CompanyScore.temp <- MarketEntryData[order(MarketEntryData$issuedCurrency,
                                           MarketEntryData$MarketEntryDate,
                                           MarketEntryData$companyId),
                                     c(5,3,1,2)]


# score each companies across all currencies
# i.e., CompnayID AED AMD AOA ARS AUD AZN BDT BEF BGN

CompanyScore <- matrix(0, nrow = length(unique(CompanyScore.temp$companyId)),
                       ncol = (1+ length(unique(CompanyScore.temp$issuedCurrency))))


colnames(CompanyScore) <- c("CompnayID",  
                            unique(CompanyScore.temp$issuedCurrency) )
CompanyScore[,1] <- unique(CompanyScore.temp$companyId)


for (i in unique(CompanyScore.temp$issuedCurrency)){
  RowInd <- which(CompanyScore.temp$issuedCurrency == i) # for each currency find all records
  temp11 <- CompanyScore.temp[RowInd, c(2,3)]
  rank.temp <- rank(-as.integer(temp11$MarketEntryDate))
  temp11$rank <- rank.temp/max(rank.temp) # rank and score based on debts issue date, normalize to [0,1]
  
  Score <- tapply(temp11$rank, INDEX = temp11$companyId, FUN = sum)
  RowInd.score <- match(names(Score), CompanyScore[,1]) # Find the companyID index for the given currency
  
  CompanyScore[RowInd.score, i] <- unname(Score)
}

df.CompanyScore <- data.frame(CompanyScore)
df.CompanyScore$TotalScore <- rowSums(df.CompanyScore[,-1])

Finial.CompanyScore <- df.CompanyScore[order(df.CompanyScore$TotalScore, decreasing=T),]

#write.csv(Finial.CompanyScore,"Finial.CompanyScore.csv", row.names = F)