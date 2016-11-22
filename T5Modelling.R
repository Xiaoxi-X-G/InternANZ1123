getMKLthreads()
DataPath <- "C:/DataAnalysis/Data/anz_debt_issuance"

##############################################################
##5. Given the clusters, create some explanation on why these 
##companies should be grouped together that a business user 
##(non-ml, non stats) person could understand.
##############################################################

require(caret)
require(pROC)
require(ada)
require(randomForest)
require(kernlab)
require(e1071)
require(pROC)


# Step0: Partition data for traning (75%) and testing (25%) 
# Step1: Inference modelling: Logistic regression
# Step2: Predictive modelling - boosting, random forest
# Step3: Model validation



### Step0####
##Partition data for traning (75%) and testing (25%) 
PreparedData <- read.csv(paste(DataPath, "/PreparedData.csv", sep=""),
                         stringsAsFactors = F)

set.seed(23)
RowInd.Train <- createDataPartition(PreparedData$IsLeader, p = 0.75, list = F)

Training.data <- PreparedData[RowInd.Train, c(-1)]
Testing.data <- PreparedData[-RowInd.Train, c(-1)]

### Setup model training parameters used in K–fold cross–validation 
cv.ctrl <- trainControl(method = "repeatedcv",
                        repeats = 5,  
                        number = 4,# K–fold (K=4) cross–validation 5 times
                        classProbs = T, # use ROC curve to find probiblity
                        summaryFunction = twoClassSummary
)


### Step1 ##### 
#######Inference modelling: Logistic regression

##

# Use all the features
set.seed(35)
Logistic1 <- train(IsLeader ~ . ,
                 data = Training.data,
                 method = "glm",
                 metric = "ROC",
                 trControl = cv.ctrl)
Logistic1
summary(Logistic1)

# DependVar <- data.frame(Name = names(Training.data),
#                         Value = 0)
# 
# write.csv(DependVar, paste(DataPath, "/DependVar.csv", sep=""),
#           quote = F, row.names = F) 

# Use limited features
set.seed(35)
Logistic2 <- train(IsLeader ~ IQ_LT_INVEST
                   +IQ_NI_AVAIL_INCL
                   +IQ_NON_CASH_ITEMS
                   +IQ_OTHER_FINANCE_ACT_SUPPL
                   +IQ_OTHER_INVESTING
                   +IQ_OTHER_OPER_ACT
                   +IQ_PROPERTY_NET
                   +IQ_TREASURY_OTHER_EQUITY
                   +latest_iq_rev,
                   data = Training.data,
                   method = "glm",
                   metric = "ROC",
                   trControl = cv.ctrl)
Logistic2
summary(Logistic2)


##### Step2 ####
#### predictive modelling - boosting and random forest

#####2.1: boosting

ada.grid <- expand.grid(.iter = c(50, 100),
                        .maxdepth = c(4, 10),
                        .nu = c(0.1, 1))


# Use all the features
set.seed(35)
ada.tune1 <- train(IsLeader ~ .-latest_iq_total_assets,
                  data = Training.data,
                  method = "ada",
                  metric = "ROC",
                  tuneGrid = ada.grid,
                  trControl = cv.ctrl)
ada.tune1
plot.train(ada.tune1)

# Use limited features
set.seed(35)
ada.tune2 <- train(IsLeader ~ IQ_LT_INVEST
                   +IQ_NI_AVAIL_INCL
                   +IQ_NON_CASH_ITEMS
                   +IQ_OTHER_FINANCE_ACT_SUPPL
                   +IQ_OTHER_INVESTING
                   +IQ_OTHER_OPER_ACT
                   +IQ_PROPERTY_NET
                   +IQ_TREASURY_OTHER_EQUITY
                   +latest_iq_rev,
                   data = Training.data,
                   method = "ada",
                   metric = "ROC",
                   tuneGrid = ada.grid,
                   trControl = cv.ctrl)

ada.tune2
plot.train(ada.tune2)



#####2.2: Random forest

# Use all feature
set.seed(35)
rf.grid1 <- data.frame(.mtry = c(5,6)) # sqrt(30) = 5.48

rf.tune1 <- train(IsLeader ~ .-latest_iq_total_assets,
                 data = Training.data,
                 method = "rf",
                 metric = "ROC",
                 tuneGrid = rf.grid1,
                 trControl = cv.ctrl)
rf.tune1


## step3 #####
#### Model validation


# Logistics
glm.pred1 <- predict(Logistic1, Testing.data)
confusionMatrix(glm.pred1, Testing.data$IsLeader)

glm.pred2 <- predict(Logistic2, Testing.data)
confusionMatrix(glm.pred2, Testing.data$IsLeader)

# Boost 
ada.pred1 <- predict(ada.tune1, Testing.data)
confusionMatrix(ada.pred1, Testing.data$IsLeader)


# RF
rf.pred1 <- predict(rf.tune1, Testing.data)
confusionMatrix(rf.pred1, Testing.data$IsLeader)



# Check ROC  
glm.probs <- predict(Logistic1, Testing.data, type = "prob")
glm.ROC <- roc(response = Testing.data$IsLeader,
               predictor = glm.probs$Y,
               levels = levels(as.factor(Testing.data$IsLeader)))
plot(glm.ROC, type = "S")


# Boosting  
ada.probs <- predict(ada.tune1, Testing.data, type = "prob")
ada.ROC <- roc(response = Testing.data$IsLeader,
               predictor = ada.probs$Y,
               levels = levels(as.factor(Testing.data$IsLeader)))
plot(ada.ROC, add=T, col = "green")

# RF  
rf.probs <- predict(rf.tune1, Testing.data, type = "prob")
rf.ROC <- roc(response = Testing.data$IsLeader,
              predictor = rf.probs$Y,
              levels = levels(as.factor(Testing.data$IsLeader)))
plot(rf.ROC, add = T, col = "red")
 



## ROC from all the resampling results 
cv.values <- resamples(list(Logistic = Logistic1,
                            Ada = ada.tune1,
                            RF = rf.tune1,
                            SVM = svm.tune1))
cv.values 
 
dotplot(cv.values, metric = "ROC")

bwplot(cv.values)