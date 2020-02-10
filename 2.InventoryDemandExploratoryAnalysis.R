# Project developed during the DSA BigData Analystics with R and Microsoft Azure course:

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                         #
# Forecasting Sales Demand for Inventor                                   #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# In this script I performed exploratory data analysis.

# Setting work directory
setwd("/home/naiara/Documentos/DataScience/FCD/BigDataRAzure/Project_Inventory_Demand")

# Loading Tools.R script
source("src/InventoryDemandTools.R")

# Getting summary of train_data numerical features
summary(train_data[, NumColNames])

# Getting statistics of train_data categorial features
lapply(FacColNames, describe)

# Visual analysis by plotting
# Loading libraries
library(dplyr)
library(ggplot2)

# Get the number of unique values for categoriacal dependent variables
uniq <- sapply(train_data[, FacColNames], function(x) {return(length(unique(x)))})
uniq_data <- data.frame(value=uniq, name=names(uniq), row.names = NULL)
uniq <- NULL

# Disabling scientific notation in R
options(scipen = 999)

# Bar plot of the number of single values
uniq_plot <- ggplot(uniq_data, aes(x=name, y=value, fill=name)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of unique values per feature of sample train dataset") +
  xlab("Feature") +
  ylab("log(unique count)") +
  scale_y_log10(limits = c(1,1e6)) +
  geom_text(aes(label = sprintf("%d", value), y=value),  vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.8, size=11,color="black")) +
  theme(axis.text.y = element_text(hjust = 0.5, size=10,color="black"))

uniq_plot
uniq_plot <- NULL


# Plotting htograms of train_data numeric features
for (name in NumColNames) { hist_graph(name)}

# Barplot of train_data categorical features
for (name in FacColNames) { freq_graph(name)}

# Checking correlation between numerical variables
# To avoid processing problems I did correlation analysis with a less sample of the data.
# Getting random sample
set.seed(10000000)
index <- sample(1:nrow(train_data), size=100000)
sample_train_data <- train_data[index,]

# getting correlation values
cor(sample_train_data[, NumColNames])

# Install package
# install.packages("GGally")

# Different correlation plots
corrplot::corrplot.mixed(cor(sample_train_data[, NumColNames]), lower="ellipse", upper="circle")
GGally::ggcorr(sample_train_data[, NumColNames], palette = "RdBu", label = TRUE)
GGally::ggpairs(sample_train_data[, NumColNames])
pairs(sample_train_data[, NumColNames])

# grouping data by features to check for pattens of sales, returns and demand.
# sales scale on the left and return scale on the right.
for (name in names(translatedNames)) { demand_grouped_by_graph(name)  }

# grouping data by client to check for pattens of sales, returns and demand.
# Values changed to log10 scale to better visualization.
log_demand_grouped_by_graph("Cliente_ID")

# Adding a column with the demand value (sales minus returns)
train_data$demand_value <- train_data$Venta_hoy - train_data$Dev_proxima

# financial loss dataframe
finantial_loss <- train_data[train_data$demand_value < 0 ,
                             c(names(translatedNames), "demand_value")]

# Checking the number of records with deficit values (negative demand value)
nrow(finantial_loss)

# Plotting finantial value loss per feature 
for (name in names(translatedNames)) { loss_per_group_graph(name)  }


# Calculating the percentage of null sales.
round((length(train_data[train_data$Venta_uni_hoy == 0, "Venta_uni_hoy"])/nrow(train_data)) * 100, 2)

# Calculating the percentage of products that had no sales.
num_zero_sales <- train_data %>%
  group_by(Producto_ID) %>%
  summarise(sale_count = sum(Venta_uni_hoy)) %>%
  filter(sale_count == 0) %>%
  data.frame() %>%
  nrow()

round((num_zero_sales/length(unique(train_data$Producto_ID))) * 100, 2)


# Removing column demand_value
train_data$demand_value <- NULL

