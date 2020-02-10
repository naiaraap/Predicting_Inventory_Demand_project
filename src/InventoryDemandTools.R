# Project developed during the DSA BigData Analystics with R and Microsoft Azure course:

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                         #
# Forecasting Sales Demand for Inventor                                   #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Inventory Demand functions and utilities for data analysis, model creation and evaluation.

# Setting work directory
setwd("/home/naiara/Documentos/DataScience/FCD/BigDataRAzure/Project_Inventory_Demand")

#Loading libraries
library(dplyr)
library(ggplot2)
require(gridExtra)

# Disabling scientific notation in R
options(scipen = 999)

# Names of numeric features
NumColNames <- c("Venta_uni_hoy", "Venta_hoy", "Dev_uni_proxima",
                 "Dev_proxima", "Demanda_uni_equil")

# Names of categorical features
FacColNames <- c("Agencia_ID", "Producto_ID", "Cliente_ID" , "Semana",
                 "Canal_ID", "Ruta_SAK", "NombreCliente", "NombreProducto",
                 "Town", "State")

ModelColNames <- c("Agencia_ID", "Producto_ID", "Cliente_ID" , "Semana",
                 "Canal_ID", "Ruta_SAK", "StateNum")

# English names for some variables
translatedNames <- c("agency", "product", "client", "week",
                            "channel", "route", "town", "state")

names(translatedNames) <- c("Agencia_ID", "Producto_ID", "Cliente_ID" , "Semana",
                     "Canal_ID", "Ruta_SAK", "Town", "State")

# Function "describe" returns feature statistics
describe <- function(name) {
  
  # Analysed feature
  data <- train_data[, name]

  # getting data statistics
  # Most common value frequency
  MAX <- MAX <- 800000
  freq <- max(summary(data, maxsum = MAX))
  
  # Most common value
  top <- names(summary(data, maxsum = MAX)[summary(data, maxsum = MAX) == freq])
  
  # dataframe of statistical data
  desc <- data.frame(statistic = c("name", "count", "unique", "top", "freq"),
             values = c(name, length(data), length(unique(data)),
                        top, freq))
  
  return(desc)
}

# Function returns frequency graphs (histograms)
hist_graph <- function(name) {
  # Disabling scientific notation in R
  options(scipen = 999)
  
  # filtering data
  assign(name, train_data[, name])
  
  # Plotting
  hist(log10(train_data[, name]),
               main = paste("Histogram for log of ", name, sep = ""),
               border="darkred", 
               col="darkgreen",
               xlab = paste("Log of ", name, sep = "")
  )
}
 
# Function returns count frequency of top 100 (or less) most common register per feature
freq_graph <- function(name) {

  # getting data
  df <- train_data %>%
    group_by(.dots=as.symbol(name)) %>%
    summarise(Count=n()) %>%
    arrange(desc(Count)) %>%
    slice(1:100) %>%
    data.frame()
  
  # Plotting
  barplot(log10(df$Count) ~ seq(1:nrow(df)),
       main=paste("Log of counting unique values for top 100 (or less) most common ", name, sep=""),
       xlab = name,
       ylab = "log10(count)",
       col="darkred",
       ) 
  
  df <- NULL
  
}


# Function to group data by features and plot it
demand_grouped_by_graph <- function(name) {
  
  # named vectors for building plots
  column <- c("demand", "sale", "return")
  title <- c("most demanded ", "highest selling ", "highest return ")
  values <- list( c("demand_uni", "sale_uni", "return_uni"),
                  c("demand_pesos", "sale_pesos", "return_pesos")  )
  names(values) <- c("units of ", "values of ")
  names(column) <- c("demand_uni", "sale_uni", "return_uni")
  names(title) <- c("demand_uni", "sale_uni", "return_uni")
  p1 = p2 = p3 = p4 = p5 = p6 = 0
  
  
  # one graph for a graph for variables with few categories and several
  # graphs for variables with a number of categories equal to or greater than 100.
  i = 1 # graph index
  title_name <- ""
  
  # Loop for group data by certain variable, sort by different parameters
  # and select top 100.
  for (col in names(column)) {
    
    # getting data: group it by feature and selecting top 100
    df <- train_data %>%
      group_by(.dots=as.symbol(name)) %>%
      summarise(sale_uni=sum(Venta_uni_hoy), sale_pesos=sum(Venta_hoy),
                return_uni=sum(Dev_uni_proxima), return_pesos=sum(Dev_proxima),
                demand_uni=sum(Demanda_uni_equil),
                demand_pesos=(sum(Venta_hoy)-sum(Dev_proxima))) %>%
      arrange(desc(eval(parse(text=col)))) %>%
      slice(1:100) %>%
      arrange(eval(parse(text=name))) %>%
      data.frame()
    
    # Treating negative and null values
    df$demand_pesos <- sapply(df$demand_pesos, function(x) { ifelse(x <= 0, 1, x) })
    
    
    
    # add expressions for features that contains many levels (100 or more)
    if (nrow(df) >= 100) { title_name <-  title[col]}
    
    # Creating array of y labels
    y <- c("units", "value in pesos")
    names(y) <- c("units of ", "values of ")
    
    # Loop for create two kind of graphs: units and values
    for (title_part in c("units of ", "values of ")) {
      
      # Obtain the coefficient so that both y axes are on the same scale 
      MAX <- max(df[, values[title_part][[1]][2]])
      mx <- max(df[, values[title_part][[1]][3]])
      coef <- mx/MAX

      # Getting y values objects for plot
      sale <- parse(text=values[title_part][[1]][2])
      returns <- parse(text=values[title_part][[1]][3])
      demands <- parse(text=values[title_part][[1]][1])
      
      # Building plot
      plt <- ggplot(df, aes(seq(1:length(unique(df[,name]))))) +
        ggtitle(paste("Sales, returns and demands ",
                      title_part,
                      title_name,
                      translatedNames[name], sep="")) +
        # returns (units or values)
        geom_bar(aes(x = seq(1:length(unique(df[,name]))),
                     y = eval(returns)/coef, fill = "return"),
                 stat="identity") +
        # demand (units or values)
        geom_line(aes(y = eval(demands), color = "demand")) +
        # sale (units or values)
        geom_line(aes(y = eval(sale), color = "sale"), linetype = "3313") +
        xlab(translatedNames[name]) +
        scale_y_continuous(
          # Features of the first axis
          name = paste(y[title_part], " (sale scale)"),
          # Add a second axis and specify its features
          sec.axis = sec_axis(~.*coef, name=paste(y[title_part], " (return scale)"))
        ) +
        scale_color_manual(values = c("sale"="black", "return"="#DB7093", "demand"="dodgerblue3")) +
        scale_fill_manual(values = "#DB7093") +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.box="vertical") +
        theme(axis.text.x = element_text(hjust = 0.5, size=8,color="black")) +
        theme(axis.text.y = element_text(hjust = 0.5, size=8,color="black")) +
        theme(axis.title.y.right = element_text(hjust = 0.5, size=8,color="black")) +
        theme(axis.title.y.right = element_text(hjust = 0.5, size=8,color="black")) +
        theme(plot.title = element_text(hjust = 0.5, size=10))
      
      assign(paste("p", as.character(i), sep = ""), plt)
      i = i + 1
      
    }
    
    # organizes two graphs in two the same window (when the variable has few categories)
    if (nrow(df) < 100) {
      print(grid.arrange(p1, p2, nrow = 2))
      break
    }
    
  }
  
  # organizes 6 graphs in two the same window (when the variable has more than 100 categories)
  if (nrow(df) >= 100) {
    grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)
  }
  
  plt <- NULL
}


# Function to group finantial loss by feature and plot value and frequency
loss_per_group_graph <- function(name) {
  
  # data grouped by feature
  df <- finantial_loss %>%
    group_by(.dots=as.symbol(name)) %>%
    summarise(Count = n(), money_loss=sum(demand_value)) %>%
    arrange(desc(abs(money_loss))) %>%
    slice(1:100) %>%
    arrange(eval(parse(text=name))) %>%
    data.frame()
  
  ifelse(nrow(df) < 100, title_part <- "", title_part <- " (top 100 with highest loss)")
  
  # Plotting
  
  # Two graphs at same window
  par(mfrow = c(2, 1))
  
  # plot losses' count per feature
  barplot(df$Count ~ seq(1:nrow(df)),
          main=paste("Count of losses per ", translatedNames[name], title_part, sep=""),
          xlab = translatedNames[name],
          ylab = "count",
          col="deeppink4"
  )
  
  # plot losses' value per feature
  barplot(abs(df$money_loss) ~ seq(1:nrow(df)),
          main=paste("Lost value per ", translatedNames[name], title_part, sep=""),
          xlab = translatedNames[name],
          ylab = "price (pesos)",
          col="hotpink4"
  )
  
}


# Function to group data by features and plot it
log_demand_grouped_by_graph <- function(name) {
  
  # named vectors for building plots
  column <- c("demand", "sale", "return")
  title <- c("most demanded ", "highest selling ", "highest return ")
  values <- list( c("demand_uni", "sale_uni", "return_uni"),
                  c("demand_pesos", "sale_pesos", "return_pesos")  )
  names(values) <- c("units of ", "values of ")
  names(column) <- c("demand_uni", "sale_uni", "return_uni")
  names(title) <- c("demand_uni", "sale_uni", "return_uni")
  p1 = p2 = p3 = p4 = p5 = p6 = 0
  
  
  
  # one graph for a graph for variables with few categories and several
  # graphs for variables with a number of categories equal to or greater than 100.
  i = 1 # graph index
  title_name <- ""
  
  # Loop for group data by certain variable, sort by different parameters
  # and select top 100.
  for (col in names(column)) {
    
    # getting data: group it by feature and selecting top 100
    df <- train_data %>%
      group_by(.dots=as.symbol(name)) %>%
      summarise(sale_uni=sum(Venta_uni_hoy), sale_pesos=sum(Venta_hoy),
                return_uni=sum(Dev_uni_proxima), return_pesos=sum(Dev_proxima),
                demand_uni=sum(Demanda_uni_equil),
                demand_pesos=(sum(Venta_hoy)-sum(Dev_proxima))) %>%
      arrange(desc(eval(parse(text=col)))) %>%
      slice(1:100) %>%
      arrange(eval(parse(text=name))) %>%
      data.frame()
    
    # Treating negative and null values
    df$demand_pesos <- sapply(df$demand_pesos, function(x) { ifelse(x <= 0, 1, x) })
    
    # add expressions for features that contains many levels (100 or more)
    if (nrow(df) >= 100) { title_name <-  title[col]}
    
    # Creating array of y labels
    y <- c("log10(units)", "log10(value in pesos)")
    names(y) <- c("units of ", "values of ")
    
    # Loop for create two kind of graphs: units and values
    for (title_part in c("units of ", "values of ")) {
      
      # Getting y values objects for plot
      sale <- parse(text=values[title_part][[1]][2])
      returns <- parse(text=values[title_part][[1]][3])
      demands <- parse(text=values[title_part][[1]][1])
      
      # Building plot
      plt <- ggplot(df, aes(seq(1:length(unique(df[,name]))))) +
        ggtitle(paste("Log10 of Sales, returns and demands ",
                      title_part,
                      title_name,
                      translatedNames[name], sep="")) +
        # returns (units or values)
        geom_bar(aes(x = seq(1:length(unique(df[,name]))),
                     y = log10(eval(returns)), fill = "return"),
                 stat="identity") +
        # demand (units or values)
        geom_line(aes(y = log10(eval(demands)), color = "demand")) +
        # sale (units or values)
        geom_line(aes(y = log10(eval(sale)), color = "sale"), linetype = "3313") +
        xlab(translatedNames[name]) +
        ylab(y[title_part]) +
        scale_color_manual(values = c("sale"="black", "return"="gold2", "demand"="dodgerblue3")) +
        scale_fill_manual(values = "gold2") +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.box="vertical") +
        theme(axis.text.x = element_text(hjust = 0.5, size=8,color="black")) +
        theme(axis.text.y = element_text(hjust = 0.5, size=8,color="black")) +
        theme(plot.title = element_text(hjust = 0.5, size=10))
      
      assign(paste("p", as.character(i), sep = ""), plt)
      i = i + 1
      
    }
    
    
    if (nrow(df) < 100) {
      print(grid.arrange(p1, p2, nrow = 2))
      
    }
    
  }
  
  if (nrow(df) >= 100) {
    grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)
  }
  
  plt <- NULL
}

# Normalize numeric feature
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

equalNormalize <- function(x, y) {
  return ((x - min(y)) / (max(y) - min(y)))
}

# function to calculate mean_absolute_error
MAE <- function(x, y) {
  return(mean(abs(x-y)))
}

# function to calculate Root-mean-square deviation
RMSE <- function(x, y){
  sqrt(mean((x - y)^2))
}

# function to convert normalized data to original values
original_values <- function(x,y) {
  return(x*(max(y) - min(y)) + min(y))
  
}



