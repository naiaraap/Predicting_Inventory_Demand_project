# Project developed during the DSA BigData Analystics with R and Microsoft Azure course:

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                         #
# Forecasting Sales Demand for Inventor                                   #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Project available on the kaggle platform
# DESCRIPTION:
# Planning a celebration is a balancing act of preparing just enough food to go 
# around without being stuck eating the same leftovers for the next week. 
# The key is anticipating how many guests will come. Grupo Bimbo must weigh 
# similar considerations as it strives to meet daily consumer demand for fresh 
# bakery products on the shelves of over 1 million stores along its 45,000 routes across Mexico.
# In this project, I am suppose to develop a model to accurately forecast inventory demand 
# based on historical sales data.

# I viewed features histograms provided automatically by the Microsoft Azure
# "Unpack Zipped Datasets" function and noticed that the numbers of records 
# for each week are similar. Therefore, I divided dataset into 7 parts, take
# samples of same size from each part and join it in a single training dataset 
# for data exploratory analysis. This process was executed by a python script.

# In this script I cleaned and transformed the data before analyzing it.

# Setting work directory
setwd("/home/naiara/Documentos/DataScience/FCD/BigDataRAzure/Project_Inventory_Demand")

# Getting and visualizing all datasets
# Get the first 10.000.000 rows of train dataset
train <- read.csv(unz("data/sample_train.zip", "sample_train.csv"))

# Getting customer data
customer <- read.csv(unz("data/cliente_tabla.csv.zip", "cliente_tabla.csv"))

# Getting product data
product <- read.csv(unz("data/producto_tabla.csv.zip", "producto_tabla.csv"))

# Getting test data
test <- read.csv(unz("data/test.csv.zip", "test.csv"))

# Getting town/state data
townState <- read.csv(unz("data/town_state.csv.zip", "town_state.csv"))

# Getting sample_submission data
submission <- read.csv(unz("data/sample_submission.csv.zip", "sample_submission.csv"))

# Visualizing each dataset
head(train)
head(customer)
head(product)
head(test)
head(townState)
head(submission)

# Eliminating columns created during sampling process
train$X <- NULL
train$Unnamed..0 <- NULL

# Getting first row for each duplication
#Loading required library
library(dplyr)

# Eliminating duplicate ids in the clients' table
# Getting first row for each Cliente_ID
customer <- customer %>%
  group_by(Cliente_ID) %>%
  slice(1)

# Merging datasets for analysis
train_data <- merge(train, customer, by = "Cliente_ID")
train_data <- merge(train_data, product, by = "Producto_ID")
train_data <- merge(train_data, townState, by = "Agencia_ID")

# Merging test dataset and respective demand
test_data <- merge(test, submission, by = "id")
test_data <- merge(test_data, townState, by = "Agencia_ID")

# Saving merged datasets on csv files
write.csv(train_data, "data/train_data.csv")
write.csv(test_data, "data/test_data.csv")

# Getting merged train and test datasets
# train_data <- read.csv(unz("data/train_data.zip", "train_data.csv"))
# test_data <- read.csv(unz("data/test_data.zip", "test_data.csv"))

# Removing extra variable
# train_data$X <- NULL
# test_data$X <- NULL
# test_data$id <- NULL

# Checking test and train data
str(train_data)
str(test_data)

head(test_data)
head(train_data)

# Checking for missing values
any(is.na(train_data))
any(is.na(test_data))

# Converting categorical variables to factor
train_data[, 1:6] <- data.frame(lapply(train_data[,1:6],
                                       function (x) {return(x <- as.factor(x))}))

test_data[, 1:6] <- data.frame(lapply(test_data[,1:6],
                                       function (x) {return(x <- as.factor(x))}))
