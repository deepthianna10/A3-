#NSSO
library(dplyr)
setwd("C:\\Users\\HP\\OneDrive\\Desktop\\deepthi  asignments scma")
getwd()
# Load the dataset
data <- read.csv("C:\\Users\\HP\\OneDrive\\Desktop\\deepthi  asignments scma\\NSSO68 (2).csv")
unique(data$state_1)
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
  # Reading the file into R
  data <- read.csv("NSSO68.csv")
  dim(data)
  unique(data$Religion)
  # Finding missing values
  missing_info <- colSums(is.na(ARP))
  cat("Missing Values Information:\n")
  print(missing_info)
  # Sub-setting the data
  ARPnew <- ARP %>%
    select(state_1,Religion, District, Region, Sector,emftt_q, emftt_v)
  # Check for missing values in the subset
  cat("Missing Values in Subset:\n")
  print(colSums(is.na(ARPnew)))
  
  dim(ARPnew)
  
  # Impute missing values with mean for specific columns
  impute_with_mean <- function(column) {
    if (any(is.na(column))) {
      column[is.na(column)] <- mean(column, na.rm = TRUE)
    }
    return(column)
  }
  ARPnew$emftt_q <- impute_with_mean(ARPnew$emftt_q)
  ARPnew$emftt_v <- impute_with_mean(ARPnew$emftt_v)
  
  dim(ARPnew)
  
  # Check for missing values after imputation
  cat("Missing Values After Imputation:\n")
  print(colSums(is.na(ARPnew)))
  
  ARP$Religion
  
  ARPnew$emftt_v
  
  ARP$Religion
  
  unique(ARP$Religion)
  str(ARP$Religion)
  
  # Sub-setting the data
  ARP_pr <- APR %>%
    select(Religion, eggsno_q, fishprawn_q, goatmeat_q, beef_q, pork_q, chicken_q, othrbirds_q)
  
  dim(ARP_pr)
  ARP_pr$eggsno_q
  data
  names(ARP_pr)
  str(ARP_pr)
  
  # Fitting a probit regression to identify non-vegetarians. 
  
  religion_mapping <- c("Hinduism", "Islam", "Christianity","Jainism","Others")
  ARP_pr$Religion <- factor(ARP_pr$Religion, labels = religion_mapping)
  table(ARP_pr$Religion)
  
  columns <- c('eggsno_q','fishprawn_q', 'goatmeat_q', 'beef_q','pork_q', 'chicken_q', 'othrbirds_q')
  data1 <- ARP[columns]
  data1$target <- ifelse(data1$eggsno_q>0,1,0) 
  probit_modet <- glm(target~., data = data1, family = binomial(link = "probit"))
  summary(probit_modet)
  
  # Performorming a Tobit regression analysis on "NSSO68.csv" 
  df_ARP = data[data$state_1 == 'ARP',]
  vars <- c("state_1","Religion", "District", "Region", "Sector","emftt_q", "emftt_v")
  
  df_ARP_p = df_ARP[vars]
  names(df_ARP_p)
  
  df_ARP_p$price = df_ARP_p$emftt_v / df_ARP_p$emftt_q
  names(df_ARP_p)
  
  summary(df_ARP_p)
  
  head(table(df_ARP_p$emftt_q))
  
  dim(df_ARP_p)
  
  names(ARP)
  
  #  dependent variable and independent variables
  y <- ARP$foodtotal_v
  X <- ARP[, c("sauce_jam_v", "Othrprocessed_v", "Beveragestotal_v", "fv_tot")]
  
  #  data for Tobit regression
  y_tobit <- pmin(pmax(y, 0), 1)  
  X_tobit <- cbind(1, X) 
  
  install.packages("censReg")
  library(censReg)
  # Fitting the Tobit model
  X_tobit_df <- as.data.frame(X_tobit)
  model <- censReg(y_tobit ~ ., data = X_tobit_df[, -1])
  
  # Printing model summary
  summary(model)
  
  
  