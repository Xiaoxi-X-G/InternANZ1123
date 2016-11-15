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
IncomeStatement <- read.csv(paste(DataPath, "/IncomeStatement_Filtered2.csv", sep=""),
                            stringsAsFactors = F)
BalanceSheet <- read.csv(paste(DataPath, "/BalanceSheet_Filtered2.csv", sep=""),
                         stringsAsFactors = F                         )
Cashflow <- read.csv(paste(DataPath, "/Cashflow_Filtered2.csv", sep=""),
                     stringsAsFactors = F)


CommCol <- intersect(intersect(unique(names(IncomeStatement)),unique(names(BalanceSheet))), 
                     unique(names(Cashflow)))


BalanceSheet[head(which(BalanceSheet$capiq_company_id==21671), n =30), CommCol]


IncomeStatement[head(which(IncomeStatement$capiq_company_id==21671), n =30), CommCol]

Cashflow[which(Cashflow$capiq_company_id==21671), CommCol]

c(2:8)

head(tail(c(1:ncol(InputData)), n = 4),2)



#########
InputData <- Cashflow

head(colnames(InputData), n =10)
tail(colnames(InputData), n =10)

KeepPercent <- 0.3 # Keep columns where the number of NAs < 30%


NACols <- apply(InputData, MARGIN = 2, function(x, y=nrow(InputData)){ 
  length(which(is.na(x)))/y}
)

KeepInd <- match(names(NACols[which(NACols < KeepPercent)]), names(InputData))

InputData.Keep.temp <- InputData[,KeepInd]
InputData.Keep <- InputData.Keep.temp[, -c(2:8)]
