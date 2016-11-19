getMKLthreads()
DataPath <- "C:/gxx/Database/internanz1123/debt_issuance"

##############################################################
##5. Given the clusters, create some explanation on why these 
##companies should be grouped together that a business user 
##(non-ml, non stats) person could understand.
##############################################################

require(caret)
require(pROC)


### Step0: 

##Partition data for traning (75%) and testing (25%) 
PreparedData <- read.csv(paste(DataPath, "/PreparedData.csv", sep=""),
                         stringsAsFactors = F)

set.seed(23)
RowInd.Train <- createDataPartition(PreparedData$IsLeader, p = 0.75, list = F)

Training.data <- PreparedData[RowInd.Train, -1]
Testing.data <- PreparedData[-RowInd.Train, -1]

### Setup model training parameters used in K–fold cross–validation 
cv.ctrl <- trainControl(method = "repeatedcv",
                        repeats = 5,  
                        K = 4,# 4–fold cross–validation 5 times
                        classProbs = T, # use ROC curve to find probiblity
                        summaryFunction = twoClassSummary
)


### Step1: Logistic modelling for Inference/interpretation


### Step2: predictive modelling (boosting, svm, rf) for accuracy
require(ada)
require(randomForest)


## step3: model evaluation
library(e1071)