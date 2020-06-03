#Load in Data
train <- read.csv('train.csv', stringsAsFactors = T)
test <- read.csv('test.csv', stringsAsFactors = T)
sample <- read.csv('sample_submission.csv')

#Load in Packages
library(tidyverse)
library(corrplot)
library(randomForest)
library(xgboost)
library(lubridate)

#High Level view
str(train)
summary(train)

train$istrain <- T
test$istrain <- F
test$SalePrice <- NA

#Merge Datasets
full <- rbind(train, test)
summary(full)

#Clean Full Dataset
  #MSZoning. Replace NAs with Most common
full[is.na(full$MSZoning), "MSZoning"] <- "RL"

  #Lot Frontage
full %>% ggplot(aes(x = as.factor(Neighborhood), y = LotFrontage)) + 
  geom_bar(stat="summary", fun = "median", fill = "red")

full %>% ggplot(aes(LotFrontage, Neighborhood)) +
  geom_boxplot() + geom_jitter(height = .25)

for (i in 1:nrow(full)) {
  if(is.na(full$LotFrontage[i])) {
    full$LotFrontage[i] = median(full$LotFrontage[full$Neighborhood == full$Neighborhood[i]],
                              na.rm = T)
  }
}

  #Alley
levels(full$Alley) <- c("Grvl", "Pave", "None")
full[is.na(full$Alley), "Alley"] <- "None"

  #Utilities
full[is.na(full$Utilities), "Utilities"] <- "AllPub"

  #Exterior1st and 2nd
full[is.na(full$Exterior1st), "Exterior1st"] <- "VinylSd"
full[is.na(full$Exterior2nd), "Exterior2nd"] <- "VinylSd"

  #MasVnr
full[is.na(full$MasVnrType), c("MasVnrType", "MasVnrArea")]

full[2611, "MasVnrType"] <- "BrkFace"

full[is.na(full$MasVnrType), "MasVnrType"] <- "None"
full[is.na(full$MasVnrArea), "MasVnrArea"] <- 0

  #BsmtQual
table(full[, c("BsmtFinType1", "BsmtQual")])
levels(full$BsmtQual) <- c("Ex", "Fa", "Gd", "TA", "None")
full[is.na(full$BsmtQual), "BsmtQual"] <- "None"

  #BsmtFinType1 and 2
levels(full$BsmtFinType1) <- c("ALQ", "BLQ", "GLQ", "LwQ", "Rec", "Unf", "None")
full[is.na(full$BsmtFinType1), "BsmtFinType1"] <- "None"

levels(full$BsmtFinType2) <- c("ALQ", "BLQ", "GLQ", "LwQ", "Rec", "Unf", "None")
full[is.na(full$BsmtFinType2), "BsmtFinType2"] <- "None"

  #Bsmt Cond
full[is.na(full$BsmtCond), c("BsmtQual","BsmtFinType1", "BsmtCond")]
full[c(2041, 2186, 2525), "BsmtCond"] <- "TA"

levels(full$BsmtCond) <- c("Fa", "Gd", "Po", "TA", "None")
full[is.na(full$BsmtCond), "BsmtCond"] <- "None"

  #BsmtFinSF1 and 2
full[is.na(full$BsmtFinSF1), c("BsmtQual", "BsmtFinSF1", "BsmtFinSF2")]
full[is.na(full$BsmtFinSF1), c("BsmtFinSF1", "BsmtFinSF2")] <- 0

  #BsmtExposure
full[is.na(full$BsmtExposure), c("BsmtFinSF1", "BsmtExposure")]
levels(full$BsmtExposure) <- c("Av", "Gd", "Mn", "No", "None")
full[is.na(full$BsmtExposure), "BsmtExposure"] <- "None"

  #BsmtUnfSF, TotalBsmtSF
full[is.na(full$BsmtUnfSF), c("BsmtUnfSF", "TotalBsmtSF")] <- 0

  #Electrical
full[is.na(full$Electrical), "Electrical"] <- "SBrkr"

  #BsmtFullBath and Halfbath
full[is.na(full$BsmtFullBath), c("BsmtFinType1", "BsmtFullBath", "BsmtHalfBath")]
full[is.na(full$BsmtFullBath), c("BsmtFullBath", "BsmtHalfBath")] <- 0

  #KitchenQual
full[is.na(full$KitchenQual), c("KitchenAbvGr", "KitchenQual")]
full[is.na(full$KitchenQual),  "KitchenQual"] <- "TA"

  #Functional
full[is.na(full$Functional), c("Functional")] <- "Typ"

  #Fireplaces
sum(full[is.na(full$FireplaceQu), "Fireplaces"])
levels(full$FireplaceQu) <- c("Ex", "Fa", "Gd", "Po", "TA", "None")
full[is.na(full$FireplaceQu), "FireplaceQu"] <- "None"

  #GarageType
levels(full$GarageType) <- c("2Types", "Attchd", "Basment", "BuiltIn", "CarPort", "Detchd", "None" )
full[is.na(full$GarageType), "GarageType"] <- "None"

  #GarageYearBuilt
full %>% ggplot(aes(GarageYrBlt, YearBuilt)) + geom_point(alpha = .25) + geom_abline(slope=1, color = "red")
which.max(full[full$GarageYrBlt, "GarageYrBlt"])

nrow(full[full$YearBuilt == full$GarageYrBlt,])

full[is.na(full$GarageYrBlt) & full$GarageType != "None", c("GarageType", "GarageYrBlt")]
full[2127, "GarageYrBlt"] <- full[2127, "YearBuilt"]
full[2577, "GarageYrBlt"] <- full[2577, "YearBuilt"]
which.max(full$GarageYrBlt)
full[2593, "GarageYrBlt"] <- full[2593, "YearBuilt"]


full$GarageYrBlt[is.na(full$GarageYrBlt)] <- full$YearBuilt[is.na(full$GarageYrBlt)]

  #GarageFinish
table(full$GarageFinish, full$GarageType)
full$GarageFinish[c(2127, 2577)] <- "Unf"
levels(full$GarageFinish) <- c("Fin", "RFn", "Unf", "None")
full$GarageFinish[is.na(full$GarageFinish)] <- "None"

  #GarageCars
full[is.na(full$GarageCars), c("GarageType", "GarageCars")]
full$GarageCars[is.na(full$GarageCars)] <- 2

  #GarageArea
full[is.na(full$GarageArea), c("GarageCars", "GarageArea")]
full[full$GarageCars == 2,] %>% ggplot(aes(GarageCars, GarageArea)) + geom_boxplot() + geom_jitter(height = .25)
full[is.na(full$GarageArea), "GarageArea"] <- median(full[full$GarageCars == 2 & !is.na(full$GarageArea), "GarageArea"])

  #GarageQual
full[is.na(full$GarageQual) & full$GarageType != "None",  "GarageQual"] <- "TA"
levels(full$GarageQual) <- c("Ex", "Fa", "Gd", "Po", "TA", "None")
full[is.na(full$GarageQual), "GarageQual"] <- "None"

  #GarageCond
full[is.na(full$GarageCond) & full$GarageType != "None",  c("GarageCond", "GarageType")]
full[is.na(full$GarageCond) & full$GarageType != "None",  "GarageCond"] <- "TA"
levels(full$GarageCond) <- c("Ex", "Fa", "Gd", "Po", "TA", "None")
full[is.na(full$GarageCond), "GarageCond"] <- "None"

  #PoolQC
full[is.na(full$PoolQC) & full$PoolArea > 0, c("PoolQC", "PoolArea")]
full[is.na(full$PoolQC) & full$PoolArea > 0, "PoolQC"] <- "Gd"
levels(full$PoolQC) <- c("Ex", "Fa", "Gd", "None")
full[is.na(full$PoolQC), "PoolQC"] <- "None"

  #Fence
levels(full$Fence) <- c("GdPrv", "GdWo", "MnPrv", "MnWw", "None")
full[is.na(full$Fence), "Fence"] <- "None"

  #MiscFeature
levels(full$MiscFeature) <- c("Gar2", "Othr", "Shed", "TenC", "None")
full[is.na(full$MiscFeature) & full$MiscVal > 0, c("MiscVal", "MiscFeature")]

full %>% ggplot(aes(MiscVal, MiscFeature)) + geom_boxplot() + geom_jitter()
full[2550, "MiscFeature"] <- "Gar2"
full[is.na(full$MiscFeature), "MiscFeature"] <- "None"

  #SaleType
full[is.na(full$SaleType), "SaleType"] <- "WD"

  #YrSold as Factor
full$YrSold <- as.factor(full$YrSold)

#Resplit full into train and test
train1 <- full[full$istrain == T,]
test1 <- full[full$istrain == F,]

train1$istrain <- NULL
test1$istrain <- NULL

#Linear Model
summary(lm(SalePrice ~., data = train1))

  #RegSubFits
library(leaps)
regfit <- regsubsets(SalePrice ~., data = train1, nvmax = 20, nbest = 10, method = "forward")
reg.summary <- summary(regfit)

par(mfrow = c(2,2))
plot(reg.summary$adjr2, xlab = "number of variables", ylab = "adjusted r squared", type = "line")
plot(reg.summary$cp, xlab = "number of variables", ylab = "cp", type = "line")
plot(reg.summary$rss, xlab = "number of variables", ylab = "RSS", type = "line")
plot(reg.summary$bic, xlab = "number of variables", ylab = "BIC", type = "line")

which.min(reg.summary$cp)
reg.summary$adjr2[201]
coef(regfit, 201)

train2 <- train1[, c("MSSubClass", "LotArea", "Neighborhood", "Condition2", "OverallQual",
                     "OverallCond", "YearBuilt", "RoofMatl", "BsmtExposure", "BsmtFinType2",
                     "BedroomAbvGr", "GarageCond", "MiscFeature", "SaleType", "Exterior2nd",
                     "SalePrice")]

#CreateDummyVariables
library(fastDummies)
test2 <- dummy_cols(test1, select_columns = c("Neighborhood", "Condition2", "RoofMatl", "BsmtExposure",
                                               "BsmtFinType2", "GarageCond", "MiscFeature", "SaleType",
                                               "Exterior2nd"))

train2 <- dummy_cols(train2, select_columns = c("Neighborhood", "Condition2", "RoofMatl", "BsmtExposure",
                                      "BsmtFinType2", "GarageCond", "MiscFeature", "SaleType",
                                      "Exterior2nd"))

train2 <- train2[,c("MSSubClass", "LotArea", "Neighborhood_Crawfor", "Neighborhood_NoRidge",
                    "Neighborhood_NridgHt","Neighborhood_StoneBr","Condition2_PosN", "OverallQual",
                    "OverallCond", "YearBuilt", "RoofMatl_WdShngl","BsmtExposure_No", "BsmtFinType2_LwQ",
                    "BedroomAbvGr", "GarageCond_Fa", "MiscFeature_Othr", "MiscFeature_Shed",
                    "MiscFeature_TenC", "SaleType_Con", "Exterior2nd_CBlock",
                    "SalePrice")]

lm.model <- lm(SalePrice ~., data = train2)
summary(lm.model)

library("car")
vif(lm.model)

#MakePredictions
predictions <- predict(object = lm.model, newdata = test2)
HousePrice.linear <- sample
HousePrice.linear$SalePrice <- predictions

write.csv(HousePrice.linear, file = "LinearSubmissionHouse.csv", row.names = F)

#RandomForestModel
rf.model <- randomForest(SalePrice ~., data = train1)
res <- tuneRF(x = subset(train1, select = -SalePrice), y = train1[,"SalePrice"], ntreeTry = 500)
print(res)

#Tuning randomForest
mtry <- seq(26, ncol(train1) * .8, 2)
nodesize <- seq(3,9,2)
sampsize <- nrow(train1) * c(.7, .8)

hypergrid <- expand.grid(mtry=mtry, nodesize=nodesize, sampsize=sampsize)

oob_err <- c()

for (i in 1:nrow(hypergrid)) {
  model <- randomForest(SalePrice ~., data = train1,
                        mtry = hypergrid$mtry[i], 
                        nodesize = hypergrid$nodesize[i],
                        sampsize = hypergrid$sampsize[i])
  
  hypergrid$rmse[i] <- sqrt(model$mse[500])
}

opt_i <- which.min(hypergrid$rmse)
print(hypergrid[opt_i,])

rf.model <- randomForest(SalePrice ~., data = train1, mtry = 34, nodesize = 7,
                         sampsize = 1168)

pred <- predict(object = rf.model, newdata = test1)
rf.pred <- sample
rf.pred$SalePrice <- pred

write.csv(rf.pred, "rfhousesub.csv", row.names = F)

#GBM
library(gbm)
set.seed(1)
gbm.model <- gbm(SalePrice ~., data = train1, distribution = "gaussian", n.trees = 5000)
gbm.model
summary(gbm.model)

#Tuning GBM
ntree_opt_oob <- gbm.perf(object = gbm.model, 
                          method = "OOB", 
                          oobag.curve = TRUE)

gbm.model_cv <- gbm(SalePrice ~., data = train1, distribution = "gaussian", 
                 n.trees = 5000, cv.folds = 2, n.cores = 1)


ntree_opt_cv <- gbm.perf(object = gbm.model_cv, 
                         method = "cv")

preds <- predict(object = gbm.model, newdata = test1, type = "response", n.trees = 591)
gbmpreds <- sample
gbmpreds$SalePrice <- preds

write.csv(gbmpreds, "gbmhousesub.csv", row.names = F)
