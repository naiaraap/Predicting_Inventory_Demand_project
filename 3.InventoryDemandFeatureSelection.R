# Project developed during the DSA BigData Analystics with R and Microsoft Azure course:

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                         #
# Forecasting Sales Demand for Inventor                                   #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# In this script I performed feature selection.

# Setting work directory
setwd("/home/naiara/Documentos/DataScience/FCD/BigDataRAzure/Project_Inventory_Demand")

# Loading Tools.R script
source("src/InventoryDemandTools.R")

# Feature selecting categorical variables
# Converting categorical variables to integer:
train_data[, names(translatedNames)] <- as.data.frame(lapply(train_data[, names(translatedNames)], as.integer))
sample_train_data[, names(translatedNames)] <- as.data.frame(lapply(sample_train_data[, names(translatedNames)],
                                                             as.integer))


# Loading randomForest library for feature select using random forest model
library(randomForest)

# Creating formula before creating model
Formula <- "Demanda_uni_equil ~ "
for (name in names(translatedNames)) { Formula <- paste(Formula, name, "+") }
Formula <- substr(Formula, start = 1, stop = nchar(Formula)-2)

# randomForest model using sample train dataset for avoid crashing.
select_model <- randomForest (eval(parse(text = Formula)), data = sample_train_data,
                              ntree = 100, nodesize = 10,
                              importance = TRUE)


# Plotting features' importance
varImpPlot(select_model)



