# Project developed during the DSA BigData Analystics with R and Microsoft Azure course:

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                         #
# Forecasting Sales Demand for Inventor                                   #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# In this script I evaluate and calculate models score. 
# I calculated the square root of the mean error (RMSD) and the Mean
# absolute error (MAE) for each model for both train and test dataset.

# Setting work directory
setwd("/home/naiara/Documentos/DataScience/FCD/BigDataRAzure/Project_Inventory_Demand")

# source
source("src/InventoryDemandTools.R")

# Predict
# logistic prediction
predicted <- data.frame(true_values = sample_norm_test$Demanda_uni_equil,
                        logistic = predict(logistic, sample_norm_test[,1:7], type="response"))

# random forest prediction
predicted$randomForest <- predict(forest, sample_norm_test[,1:7])

# ridge regression prediction
predicted$ridge <- predict(RidgeMod, sample_norm_test[,1:7])

# k-nearest neighbor prediction
predicted$knn <- as.numeric(knnTestPred)

# xgboost prediction
predicted$xgboost <- predict(xgboostmodel, as.matrix(sample_norm_test[,1:7]))


# appending predictions of the training datasets for calculating RMSD and MAE.
# logistic
predictedTrain <- data.frame(true_values = sample_norm_model_train$Demanda_uni_equil,
                             logistic = predict(logistic, sample_norm_model_train[,1:7], type="response"))

# random forest train prediction
predictedTrain$randomForest <- predict(forest, sample_norm_model_train[,1:7])

# ridge regression train prediction
predictedTrain$ridge <- predict(RidgeMod, sample_norm_model_train[,1:7])

# k-nearest neighbor train prediction
predictedTrain$knn <- as.numeric(knnTrainPred)

# xgboost train prediction
predictedTrain$xgboost <- predict(xgboostmodel, as.matrix(sample_norm_model_train[,1:7]))

# Calculating RMSD and MAE
# Creating dataframe to store values
error <- data.frame(Models =c("Logistic Regression", "Ridge regression", "Random Forest",
                               "k-nearest neighbors", "xgBoost"),
                    Train_MAE = c(0,0,0,0,0),
                    Train_RMSE = c(0,0,0,0,0),
                    Test_MAE = c(0,0,0,0,0),
                    Test_RMSE = c(0,0,0,0,0))

# Getting MAE and RMSE for train prediction and test prediction
i <- 1
for (model in names(predictedTrain[2:6])) {
  error$Train_MAE[i] <-MAE(predictedTrain$true_values, predictedTrain[, model])
  error$Train_RMSE[i] <-RMSE(predictedTrain$true_values, predictedTrain[, model])
  error$Test_MAE[i] <-MAE(predicted$true_values, predicted[, model])
  error$Test_RMSE[i] <-RMSE(predicted$true_values, predicted[, model])
  i = i + 1
}

# Models it's RMSE and MAE
error

# Smaller train_MAE model
error[error$Train_MAE == min(error$Train_MAE),]

# Smaller train_RMSE model
error[error$Train_RMSE == min(error$Train_RMSE),]

# Smaller test_MAE model
error[error$Test_MAE == min(error$Test_MAE),]

# Smaller test_RMSE model
error[error$Test_RMSE == min(error$Test_RMSE),]

# I chose Logistic regression model for presenting low RMSE and MAE values for test dataset.





