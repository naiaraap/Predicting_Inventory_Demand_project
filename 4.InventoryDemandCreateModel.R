# Project developed during the DSA BigData Analystics with R and Microsoft Azure course:

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                         #
# Forecasting Sales Demand for Inventory                                  #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# In this script I created predict model.

# Setting work directory
setwd("/home/naiara/Documentos/DataScience/FCD/BigDataRAzure/Project_Inventory_Demand")

# source
source("src/InventoryDemandTools.R")

# Creating numeric feature for state
# Train dataset
train_data$StateNum <- train_data$State
levels(train_data$StateNum) <- seq(1:33)
train_data$StateNum <- as.integer(train_data$StateNum)

#Test dataset
test_data$StateNum <- test_data$State
levels(test_data$StateNum) <- seq(1:33)
test_data$StateNum <- as.integer(test_data$StateNum)

# Converting categorical features to numeric
for (name in ModelColNames) { train_data[, name] <- as.integer(train_data[, name])
                              test_data[, name] <- as.integer(test_data[, name]) }

# Saving transformed dataset
write.csv(train_data[, c(ModelColNames, "Demanda_uni_equil")], "data/model_train_data.csv")
write.csv(test_data[, c(ModelColNames, "Demanda_uni_equil")], "data/model_test_data.csv")

model_train <- train_data[, c(ModelColNames, "Demanda_uni_equil")]
model_test <- test_data[, c(ModelColNames, "Demanda_uni_equil")]

# Using variables with cardinality less than or equal to 100.
Formula <- "Demanda_uni_equil ~ Canal_ID + StateNum + Semana"

# Normalize integer feature
for (name in names(translatedNames)[1:6]) { train_data[, name] <- normalize(train_data[, name]) }

# Getting data
# Train
# model_train <- read.csv(unz("data/model_train_data.zip", "model_train_data.csv"))
# model_train <- model_train[,2:ncol(model_train)]

# Test
# model_test <- read.csv(unz("data/model_test_data.zip", "model_test_data.csv"))
# model_test <- model_test[2:ncol(model_test)]

# Normalising train dataset
norm_model_train <- model_train
norm_model_train$Demanda_uni_equil <- normalize(norm_model_train$Demanda_uni_equil)


# Normalising test dataset
norm_model_test <- model_test
norm_model_test$Demanda_uni_equil <- equalNormalize(x=norm_model_test$Demanda_uni_equil,
                                                    y=model_train$Demanda_uni_equil)

# Getting dataset train sample for 
set.seed(10000000)
index <- sample(1:nrow(norm_model_train), size=100000)
sample_norm_model_train <- norm_model_train[index,]

# To be able to perform the tests on my machine, I selected smaller
# samples of the training and test datasets in the proportion 70% and 30% respectively.
# Getting test dataset sample in proportion: training (70%) test (30%)
index <- sample(1:nrow(norm_model_test), size=round(100000*3/7))
sample_norm_test <- norm_model_test[index,]

# Logistic regression
logistic <- glm(formula = eval(parse(text = Formula)), data = sample_norm_model_train, family = "binomial")

# randomForest model
forest <- randomForest::randomForest (eval(parse(text = Formula)), data = sample_norm_model_train,
                                      ntree = 100, nodesize = 10)

# Ridge Regression model
RidgeMod <- ridge::linearRidge(eval(parse(text = Formula)), data = sample_norm_model_train)

# k-nearest neighbor
knnTestPred <- DMwR::kNN(eval(parse(text = Formula)),
                         sample_norm_model_train, 
                         sample_norm_test[,1:7],
                         norm=FALSE, k=13)

knnTrainPred <- DMwR::kNN(eval(parse(text = Formula)),
                         sample_norm_model_train, 
                         sample_norm_model_train[,1:7],
                         norm=FALSE, k=13)


# xgBoost model  
xgboostmodel <- xgboost::xgboost(data = as.matrix(sapply(sample_norm_model_train[1:7], as.numeric)),
                                label = sample_norm_model_train$Demanda_uni_equil,
                                max.depth = 2, eta = 1, nthread = 2, nrounds = 2, 
                                objective = "binary:logistic", verbose = 1)

