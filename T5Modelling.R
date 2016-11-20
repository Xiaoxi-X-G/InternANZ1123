getMKLthreads()
DataPath <- "C:/gxx/Database/internanz1123/debt_issuance"

##############################################################
##5. Given the clusters, create some explanation on why these 
##companies should be grouped together that a business user 
##(non-ml, non stats) person could understand.
##############################################################

require(caret)
require(pROC)


### Step0####

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
                        number = 4,# K–fold (K=4) cross–validation 5 times
                        classProbs = T, # use ROC curve to find probiblity
                        summaryFunction = twoClassSummary
)




### Step1 ##### 
##Logistic modelling for Inference/interpretation

# Use all the features
set.seed(35)
Logistic1 <- train(IsLeader ~ . -latest_iq_total_assets,
                 data = Training.data,
                 method = "glm",
                 metric = "ROC",
                 trControl = cv.ctrl)
Logistic1
summary(Logistic1)


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


### Step2: predictive modelling (boosting, svm, rf) for accuracy

#####2.1: boosting
# In particular, boosting is to find stronger and more comprehesive 
# rulse from many weaker rules. 
# Step1: Use a weak/simple rule as a classfier, and then evolve 
# to multiple version by panilying incorrect results
# step2: Combine all weak rules and generate a stronger rules. 
# Which later can be used to generate more 
#        stronger rules with different version
# ..., continue, until .depth or accuracy met


require(ada)

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
# The number of randomly pre-selected predictor variables 
# for each node,  designated mtry, is the sole parameter 
# available for tuning an RF with train.

# step1: Use bootstrap to generate many training dataset.
# step2: build predictive model for each training dataset.
# step3: Combine results (averaging or majority vote) and 
#produce finial predicts.


require(randomForest)

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


# Use limited feature
set.seed(35)
rf.grid2 <- data.frame(.mtry = c(3,4)) # sqrt(9) = 3

rf.tune2 <- train(IsLeader ~ IQ_LT_INVEST
                  +IQ_NI_AVAIL_INCL
                  +IQ_NON_CASH_ITEMS
                  +IQ_OTHER_FINANCE_ACT_SUPPL
                  +IQ_OTHER_INVESTING
                  +IQ_OTHER_OPER_ACT
                  +IQ_PROPERTY_NET
                  +IQ_TREASURY_OTHER_EQUITY
                  +latest_iq_rev,
                  data = Training.data,
                  method = "rf",
                  metric = "ROC",
                  tuneGrid = rf.grid2,
                  trControl = cv.ctrl)
rf.tune2



## svm
require(kernlab)
# Use Kernel to separate data one the observation feature space
# One of the population is Radial Kernel to meature the distance 
# between observations,
# ie: exp(-gamma sum(Xi-Xj)^2)


#As SVM is considered sensitive to the scale and magnitude of the 
# presented features, I'll use the preProcess argument to instruct 
# train to make arrangements for normalizing 

# Use all feature
set.seed(35)
svm.tune1 <- train(IsLeader ~ .-latest_iq_total_assets,
                  data = Training.data,
                  method = "svmRadial",
                  tuneLength = 9,
                  preProcess = c("center", "scale"),
                  metric = "ROC",
                  trControl =  cv.ctrl)

svm.tune1

## use limited features
set.seed(35)
svm.tune2 <- train(IsLeader ~ IQ_LT_INVEST
                   +IQ_NI_AVAIL_INCL
                   +IQ_NON_CASH_ITEMS
                   +IQ_OTHER_FINANCE_ACT_SUPPL
                   +IQ_OTHER_INVESTING
                   +IQ_OTHER_OPER_ACT
                   +IQ_PROPERTY_NET
                   +IQ_TREASURY_OTHER_EQUITY
                   +latest_iq_rev,
                   data = Training.data,
                   method = "svmRadial",
                   tuneLength = 9,
                   preProcess = c("center", "scale"),
                   metric = "ROC",
                   trControl =  cv.ctrl)

svm.tune2

## step3: #####
# model evaluation
library(e1071)

# Logistics
glm.pred1 <- predict(Logistic1, Testing.data)
confusionMatrix(glm.pred1, Testing.data$IsLeader)

glm.pred2 <- predict(Logistic2, Testing.data)
confusionMatrix(glm.pred2, Testing.data$IsLeader)

# Boosted 
ada.pred1 <- predict(ada.tune1, Testing.data)
confusionMatrix(ada.pred1, Testing.data$IsLeader)

ada.pred2 <- predict(ada.tune2, Testing.data)
confusionMatrix(ada.pred2, Testing.data$IsLeader)


# RF
rf.pred1 <- predict(rf.tune1, Testing.data)
confusionMatrix(rf.pred1, Testing.data$IsLeader)

rf.pred2 <- predict(rf.tune2, Testing.data)
confusionMatrix(rf.pred2, Testing.data$IsLeader)


# SVM
svm.pred1 <- predict(svm.tune1, Testing.data)
confusionMatrix(svm.pred1, Testing.data$IsLeader)

svm.pred2 <- predict(svm.tune2, Testing.data)
confusionMatrix(svm.pred2, Testing.data$IsLeader)



library(pROC)
# We can also calculate, using each of the four fitted models, 
# the predicted probabilities for the test.batch, and use those 
# probabilities to plot the ROC curves.


# logistic = 0.7117
glm.probs <- predict(Logistic1, Testing.data, type = "prob")
glm.ROC <- roc(response = Testing.data$IsLeader,
               predictor = glm.probs$Y,
               levels = levels(as.factor(Testing.data$IsLeader))) 
plot(glm.ROC, type = "S")


# Boost model = 0.7432
ada.probs <- predict(ada.tune1, Testing.data, type = "prob")
ada.ROC <- roc(response = Testing.data$IsLeader,
               predictor = ada.probs$Y,
               levels = levels(as.factor(Testing.data$IsLeader)))
plot(ada.ROC, add=T, col = "green")

# RF = 0.7376
rf.probs <- predict(rf.tune1, Testing.data, type = "prob")
rf.ROC <- roc(response = Testing.data$IsLeader,
              predictor = rf.probs$Y,
              levels = levels(as.factor(Testing.data$IsLeader)))
plot(rf.ROC, add = T, col = "red")


# SVM = 0.6936
svm.probs <- predict(svm.tune1, Testing.data, type = "prob")
svm.ROC <- roc(response = Testing.data$IsLeader,
               predictor = svm.probs$Y,
               levels = levels(as.factor(Testing.data$IsLeader)))
plot(svm.ROC, add = T, col = "blue")




## The following R script uses caret function resamples to collect the resampling results, 
# then calls the dotplot function to create a visualization of the resampling distributions.
# One graph which sums up the performance of the four models, this is it.
cv.values <- resamples(list(Logistic = Logistic1,
                            Ada = ada.tune1,
                            RF = rf.tune1,
                            SVM = svm.tune1))
cv.values 
# Number of resamples = K * repeats (K = K-fold cross validation, where repeat 'repeats' times)

dotplot(cv.values, metric = "ROC")

bwplot(cv.values)