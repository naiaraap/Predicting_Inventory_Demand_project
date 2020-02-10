# Project developed during the DSA BigData Analystics with R and Microsoft Azure course:

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                         #
# Forecasting Sales Demand for Inventor                                   #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# in this script, I tried to optimize and evaluate the chosen model through
# calculations and visualizations.
# I chose Logistic regression model for presenting low RMSE and MAE values

# Setting work directory
setwd("/home/naiara/Documentos/DataScience/FCD/BigDataRAzure/Project_Inventory_Demand")

# source
source("src/InventoryDemandTools.R")

# Summary model
summary(logistic)

# Modifing dataframe to try to improve logistic regression model
new_train_df <- sample_norm_model_train[,c("Canal_ID", "StateNum", "Semana", "Demanda_uni_equil")]
new_test_df <- sample_norm_test[,c("Canal_ID", "StateNum", "Semana", "Demanda_uni_equil")]

# Normalizing all variables
new_train_df[, 1:3] <- data.frame(lapply(new_train_df[, 1:3], normalize))
new_test_df[, 1:3] <- data.frame(mapply(equalNormalize, new_test_df[, 1:3],
                                        sample_norm_model_train[, c("Canal_ID", "StateNum", "Semana")]))


# Using variables with cardinality less than or equal to 100.
Formula <- "Demanda_uni_equil ~ Canal_ID + StateNum + Semana"

# New model
newLogistic <- glm(formula = eval(parse(text = Formula)), data = new_train_df, family = "binomial")

# Evaluating new model
MAE(new_test_df$Demanda_uni_equil, predict(newLogistic, new_test_df[, 1:3], type="response"))
RMSE(new_test_df$Demanda_uni_equil, predict(newLogistic, new_test_df[, 1:3], type="response"))

# RMSE and MAE values didn't change, so I used original values for dependent variables
new_train_df <- sample_norm_model_train[,c("Canal_ID", "StateNum", "Semana", "Demanda_uni_equil")]
new_test_df <- sample_norm_test[,c("Canal_ID", "StateNum", "Semana", "Demanda_uni_equil")]

# As the variable "Canal_ID" has a high level of significance, I will increase its weight
# Multiplying channel by 3
new_train_df$Canal_ID <- 10*new_train_df$Canal_ID
new_test_df$Canal_ID <- 10*new_test_df$Canal_ID

# New model
newLogistic <- glm(formula = eval(parse(text = Formula)), data = new_train_df, family = "binomial")

# Evaluating new model
MAE(new_test_df$Demanda_uni_equil, predict(newLogistic, new_test_df[, 1:3], type="response"))
RMSE(new_test_df$Demanda_uni_equil, predict(newLogistic, new_test_df[, 1:3], type="response"))

# RMSE and MAE values didn't change, so I will evaluate original logistic model

# Getting not-normalized data
x <- round(original_values(x=predicted$logistic, y=model_train$Demanda_uni_equil))
y <- round(original_values(x=predicted$true_values, y=model_train$Demanda_uni_equil))
z <- round(original_values(x=predicted$randomForest, y=model_train$Demanda_uni_equil))

# not normalized data MAE and RMSE
MAE(x, y)
RMSE(x, y)

# The final version of the model presents MAE = 0.97 and RMSD = 3.12 for the evaluated samples.


