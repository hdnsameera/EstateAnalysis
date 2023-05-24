#############################################################################################################
# Name            : Hettige Don Nuwan Sameera
# Task            : Predict productivity based on the given datasets of estates
#############################################################################################################
# ===================================== SETTING UP THE WORKING DIRECTORY ====================================

setwd('/estate/Resources_R/')

# ===================================== READING DATA FROM CSV FILES =========================================

# Read Employee.csv file
emp <- read.csv(file = 'Employee.csv')

# Read Fields.csv file
fld <- read.csv(file = 'Fields.csv')

# Read WorkCode.csv file
wcod <- read.csv(file = 'WorkCode.csv', stringsAsFactors = TRUE)

# Read WorkDetails-old.csv file
wdet <- read.csv(file = 'WorkDetails-old.csv')

# ======================================= USER-DEFINED FUNCTIONS - GLOBAL ===================================

# function to calculate service in number of years
fun.get.years <- function(x) {
  return (length(seq(from = x, to = as.Date('2015-12-14'), by = 'year')) - 1)
}

# Function to visualize ------------------------------------------- -------------
fun.visualize.missing <- function(x1) {
  
  # Close if any other graphic is opened
  #dev.off()
  
  # NA level analysis ----------
  library(tidyverse)
  theme_set(theme_bw(base_size=16))
    
  x1  %>%
    summarise_all(list(~is.na(.)))%>%
    pivot_longer(everything(), names_to = "variables", values_to="missing") %>%
    count(variables, missing) %>%
    ggplot(aes(y=variables,x=n,fill=missing))+
    geom_col(position = "fill")+
    labs(x="Proportion")+
    scale_fill_manual(values=c("skyblue3","gold"))+
    ggtitle(label = paste0('"NA" Level Analysis (',deparse(substitute(x1)),')'))+
    theme(axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5))    
}

# Function to get lower and upper levels of boxplots -----------
fun.getLowerUpper <- function(x, iqrTimes = 1.5) {
# Get quantiles
  quants <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  
# list lower and upper
  valLowerUpper <- ''
  
# Calculate lower and upper values
  valLowerUpper[1] <- quants[1] - ( IQR(x) * iqrTimes )
  valLowerUpper[2] <- quants[2] + ( IQR(x) * iqrTimes )
  
  return (as.numeric(valLowerUpper))
}

# Function used to do normalization --------------------------------------------
fun.normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Function used to do z-score --------------------------------------------------
fun.z <- function(x) {
  return ( (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# ========================================= EMPLOYEE DATA SET ===========================================
# Check structure
str(emp)

# Check summary
summary(emp)


# Get number of observations
length(wdet[,1])

# Checkin for NAs
length(emp[is.na(emp),1])

# NULL to NA conversion
emp[emp == 'NULL'] <- NA

# ______________________________________________________ NA LEVEL ANALYSIS - emp
fun.visualize.missing(emp)

# Get the number of NAs in the dataframe
length(emp[is.na(emp),1])

# Get number of duplicate observations
length(wdet[duplicated(wdet),1])

# Factorization of Categorical-Nominal data

emp$Estate <- factor(emp$Estate)
emp$Division <- factor(emp$Division)
emp$Gender <- factor(emp$Gender, levels = c('M','F'), labels = c('Male','Female'))
emp$PAMACT <- factor(emp$PAMACT)

# ___________________________________________________________ DUMMY CODING - emp
emp$Gender_m <- ifelse(emp$Gender == 'Male', 1,0)
emp$Gender_f <- ifelse(emp$Gender == 'Female', 1,0)

# Checking for duplicate records
length(emp[duplicated(emp),1])

# Checking for number of duplicate records comparing the EmpCode
length(emp[duplicated(emp$EmpCode),1])

# ______________________________________________________ REMOVE DUPLICATES - emp
# Display duplicate records comparing the EmpCode
emp[duplicated(emp$EmpCode),]

# Filtering NULL or NA value records
emp.NULL <- emp[emp$DateofJoin == 'NULL',]

# Removing NULL or NA value records
emp.NoNULL <- emp[emp$DateofJoin != 'NULL',]


# Data conversion -------
# Arranging DateofJoin in YYYY-MM-DD format
emp[!is.na(emp$DateofJoin),]$DateofJoin = substring(emp[!is.na(emp$DateofJoin),]$DateofJoin, 1,10)

# Convert DataofJoin from character to date
emp$DateofJoin <- as.Date(emp$DateofJoin)


# Analyzing the latest record in work.detail data frame
tail(wdet[order(wdet$Year, wdet$Month, wdet$Day),])

# calculating the service in number of years
emp$service[!is.na(emp$DateofJoin)] <-  sapply(emp$DateofJoin[!is.na(emp$DateofJoin)], fun.get.years)

# Converting double as integer
emp$service <- as.integer(emp$service)

# ======== EMPLOYEE BOXPLOT ========================
# Check summary
summary(emp)

# Employee full dataset to be merged with work details dataset
emp.full <- emp

# ________________________________________________ MISSING VALUE TREATMENT - emp
# emp.NoNA - NA omitted dataset
emp.NoNA <- na.omit(emp)

# imputing 
emp[is.na(emp$service),9] <- mean(emp.NoNA$service)
emp[is.na(emp$DateofJoin), 5] <- mean(emp.NoNA$DateofJoin)

# emp.NoNA ------------------------------------------------------------
# BOXPLOT WITH OUTLIERS

# View summary of service
summary(emp.NoNA$service)

# View boxplot before removing outliers
boxplot(emp.NoNA$service, horizontal = TRUE, main = 'Service of emps (with outliers)')

# Show values over the boxplot
text(x = summary(emp.NoNA$service)[-4], labels = summary(emp.NoNA$service)[-4], y = 1.4)


# _________________________________ OUTLIER TREATMENT / NA REMOVED DATASET - emp

# Function to remove outliers in employee dataset
fun.emp.outlierTreatment <- function(x) {

  # Get a copy of the dataset
  df <- x
  
  # Get number of records before removing outliers
  beforeLength <- length(x[,1])
  
  # Get quantiles
  boxValues <- as.numeric(fun.getLowerUpper(x$service))
  
  # Select records within the lower and upper levels
  x <- x[(x$service > boxValues[1] & x$service < boxValues[2]),]
  
  # View boxplot after removing outliers
  boxplot(x$service, horizontal = TRUE, main = 'Service of emps (without outliers)')
  
  # Add values over the boxplot
  text(x = summary(x$service)[-4], labels = summary(x$service)[-4], y = 1.4)
  
  # Get number of records after removing outliers
  afterLength <- length(x[,1])
  
  # Calculate outlier percentage
  outPval <- (( beforeLength - afterLength ) / beforeLength ) * 100
  
  # Outlier percentage
  print(paste0('Outlier percentate = ',round(outPval, digits = 2)))
  
  # If the balance number of records after removing outliers is greater than 10%
  # extend the IQR times multiplication from 1.5 to 2.0 and try removing the
  # the outliers again or use the balance.
  if (outPval <= 10) {
    df <- x
  }
  
  return(df)
    
}

# __________________________________ OUTLIER TREATMENT / NA OMITED DATASET - emp 
emp.NoNA <- fun.emp.outlierTreatment(emp.NoNA)

# __________________________________ OUTLIER TREATMENT / IMPUTED DATASET - emp
emp <- fun.emp.outlierTreatment(emp)


# __________________________________________________________ NORMALIZATION - emp

# Normalizing NA omited dataset
emp.NoNA.n <- emp.NoNA
emp.NoNA.n$service <- fun.normalize(emp.NoNA$service)

# Normalizing imputed dataset
emp.n <- emp
emp.n$service <- fun.normalize(emp$service)

# Z-Score
emp.NoNA.z <- emp.NoNA
emp.NoNA.z$service <- fun.z(emp.NoNA$service)

emp.z <- emp
emp.z$service <- fun.z(emp$service)

# Remove garbage
rm(emp.NoNATemp, emp.NoNULL, emp.NULL, emp, emp.NoNA)

# FINAL DATASETS
# emp.NoNA    - NA removed datset
# emp.NoNA.n  - NA removed datset - normalized
# emp.NoNA.z  - NA removed datset - standarized

# emp         - total dataset treated imputation
# emp.n       - total dataset treated imputation - normalized
# emp.z       - total dataset treated imputation - standarized
# 

# ======================================= FIELDS DATA SET ===========================================

# NULL to NA conversion
fld[fld == 'NULL'] <- NA

# Checkin for NAs
length(fld[is.na(fld),1])

# ______________________________________________________ NA LEVEL ANALYSIS - fld
fun.visualize.missing(fld)

fld$EState <- factor(fld$EState)
fld$Division <- factor(fld$Division)
fld$CropType <- factor(fld$CropType)
fld$Field <- factor(fld$Field)
fld$Type <- factor(fld$Type)

fld$NumberOfTree <- as.numeric(fld$NumberOfTree)

# ______________________________________________________ REMOVE DUPLICATES - fld
# Checking for duplicates
length(which(duplicated(fld)))

# Structure level issues fixing
colnames(fld)[1] <- 'Estate'

# Boxplot with raw values
# Area
boxplot(fld$Area, horizontal = TRUE, main = 'fld - Area')

# Number of trees
boxplot(fld$NumberOfTree, horizontal = TRUE, main = 'fld - Number of trees')

# Area & Number of trees
boxplot(fld$Area, fld$NumberOfTree, names = c('Area','Number of Trees'), horizontal=TRUE, main = 'fld - Area & Number of Trees')

# ________________________________________________ MISSING VALUE TREATMENT - fld
fld.NoNA <- na.omit(fld)
fld$NumberOfTree[is.na(fld$NumberOfTree)] <- mean(fld.NoNA$NumberOfTree)

# __________________________________________________________ NORMALIZATION - fld
# Applying Min-Max normalization
# create a new data frame omiting NAs- fld.NoNA
fld.NoNA$Area <- fun.normalize(fld.NoNA$Area)
fld.NoNA$NumberOfTree <- fun.normalize(fld.NoNA$NumberOfTree)


# BOXPLOT WITH OUTLIERS ============================
boxplot(fld.NoNA$Area, fld.NoNA$NumberOfTree, names = c('Area','Number of Trees'), horizontal = TRUE, main = 'fld - Area & Number of Trees')

# Get number of records before removing outliers
beforeLength <- length(fld.NoNA[,1])

print(paste0('Number of observations before removing outliers = ', beforeLength))


# BOXPLOT WITHOUT OUTLIERS =========================
# Removing outliers of 'Area' variable---------------------------------->
fld.NoNAtmp <- fld.NoNA

# Get quantiles
boxValues <- fun.getLowerUpper(fld.NoNA$Area)

# Outlers' lower and upper values
boxValues

# Select records within the lower and upper levels
fld.NoNA <- fld.NoNA[(fld.NoNA$Area > boxValues[1] & fld.NoNA$Area < boxValues[2]),]

# Get number of records after removing outliers
afterLength <- length(fld.NoNA[,1])

print(paste0('Number of observations after removing outliers = ', afterLength))

# Calculate outlier percentage
outPval <- round((( beforeLength - afterLength ) / beforeLength ) * 100, digits = 2)

# Outlier percentage
print(paste0('Percentage of removed observations = ',outPval,'%'))

# View boxplot after removing outliers
boxplot(fld.NoNA$Area, fld.NoNA$NumberOfTree, names = c('Area','Number of Trees'), horizontal = TRUE, main = 'Area & Number of trees (without outliers)')

# Add values over the boxplot
text(x = summary(fld.NoNA$Area)[-4], labels = round(summary(fld.NoNA$Area)[-4], digits = 2), y = 1.5)
text(x = summary(fld.NoNA$NumberOfTree)[-4], labels = round(summary(fld.NoNA$NumberOfTree)[-4], digits = 2), y = 2.5)

#----------------------------------------------------------------------->
# Removing outliers of 'NumberofTrees' variable------------------------->
fld.NoNA <- fld.NoNAtmp

# Get quantiles
boxValues <- fun.getLowerUpper(fld.NoNA$NumberOfTree, iqrTimes = 1)

# Outlers' lower and upper values
boxValues

# Select records within the lower and upper levels
fld.NoNA <- fld.NoNA[(fld.NoNA$NumberOfTree > boxValues[1] & fld.NoNA$NumberOfTree < boxValues[2]),]

# Get number of records after removing outliers
afterLength <- length(fld.NoNA[,1])

print(paste0('Number of observations after removing outliers = ', afterLength))

# Calculate outlier percentage
outPval <- round((( beforeLength - afterLength ) / beforeLength ) * 100, digits = 2)

# Outlier percentage
print(paste0('Percentage of removed observations = ',outPval,'%'))

# View boxplot after removing outliers
boxplot(fld.NoNA$Area, fld.NoNA$NumberOfTree, names = c('Area','Number of Trees'), horizontal = TRUE, main = 'Area & Number of trees (without outliers)')

# Add values over the boxplot
text(x = summary(fld.NoNA$Area)[-4], labels = round(summary(fld.NoNA$Area)[-4], digits = 2), y = 1.5)
text(x = summary(fld.NoNA$NumberOfTree)[-4], labels = round(summary(fld.NoNA$NumberOfTree)[-4], digits = 2), y = 2.5)

# If the balance number of records after removing outliers is greater than 10%
# extend the IQR times multiplication from 1.5 to 2.0 and try removing the
# the outliers again or use the balance.
if (outPval <= 10) {
  
}

#Remove garbage
rm(fld.NoNAtmp)

# ======================================= WORKCODE DATA SET ===========================================
# MISSING VALUE TREATMENT -------------------------------------->

# Check for the summary
summary(wcod)

# NULL to NA conversion
wcod[wcod == 'NULL'] <- NA

# Checkin for NAs
length(wcod[is.na(wcod),1])

# Check for the summary
summary(wcod)

# Fixing invalid boolean conversion


# _____________________________________________________ NA LEVEL ANALYSIS - wcod
fun.visualize.missing(wcod)

# Checking NULL values
wcod[wcod$Type == 'NULL',]

# wcod records with NULL values
wcod.NULL <- wcod[wcod$Type == 'NULL',]

# wcod records without NULL values
wcod <- wcod[wcod$Type != 'NULL',]

# Structure level issues fixing
colnames(wcod)[1] <- 'Work'

# _____________________________________________________ REMOVE DUPLICATES - wcod

# Removing duplicates from wcod ------------

# Get number of records
length(wcod[,1])

# Get number of duplicate records
length(wcod[duplicated(wcod),1])

# Display duplicate records
wcod[duplicated(wcod),]

# Re-assign records except duplicates
wcod <- wcod[!duplicated(wcod),]

# Get number of records after removing duplicates
length(wcod[,1])


# Removing duplicates from wcod.NULL -------
# Get a backup of wcod
wcod.temp <- wcod

# Assign values from wcod.NULL to wcod
wcod <- wcod.NULL

# Get number of records
length(wcod[,1])

# Get number of duplicate records
length(wcod[duplicated(wcod),1])

# Display duplicate records
wcod[duplicated(wcod),]

# Re-assign records except duplicates
wcod <- wcod[!duplicated(wcod),]

# Get number of records after removing duplicates
length(wcod[,1])

# Re-assign records from wcod.temp to wcod
wcod <- wcod.temp

# Remove temporary data frame
rm(wcod.temp)


# ======================================= WORK DETAILS DATA SET ===========================================
# --- MISSING VALUE TREATMENT ---
# Factorization
wdet$Estate <- factor(wdet$Estate)
wdet$Division <- factor(wdet$Division)
wdet$Work <- factor(wdet$Work)

# Convert Quantity and Extra Kilos fld to numeric
wdet$Qty <- as.numeric(wdet$Qty)
wdet$ExtraKilos <- as.numeric(wdet$ExtraKilos)

# Checkin for NAs
length(wdet[is.na(wdet),1])

# NULL to NA conversion
wdet[wdet == 'NULL'] <- NA

# _____________________________________________________ NA LEVEL ANALYSIS - wdet
fun.visualize.missing(wdet)

# Analyzing the latest record in work.detail data frame
tail(wdet[order(wdet$Year, wdet$Month, wdet$Day),])

# Get number of observations
length(wdet[,1])

# _____________________________________________________ REMOVE DUPLICATES - wdet
# Get number of duplicate observations
length(wdet[duplicated(wdet),1])

# Remove duplicates
wdet <- wdet[!duplicated(wdet),]

# Get number of observations after removing duplicates
length(wdet[,1])

# Select only a subset of the full dataset
wdet <- wdet[1:20000,]

# Work details full dataset to be merged with employee full dataset
wdet.full <- wdet

# ----------------------------------------------------
# wdet - OUTPUT DATASET - REMOVED DUPLICATES ---------
# ----------------------------------------------------
# _______________________________________________ MISSING VALUE TREATMENT - wdet
# work details without NA
wdet.NoNA <- wdet[complete.cases(wdet),]

# work details imputed dataset
wdet$Qty[is.na(wdet$Qty)] <- mean(wdet$Qty[!is.na(wdet$Qty)])
wdet$ExtraKilos[is.na(wdet$ExtraKilos)] <- mean(wdet$ExtraKilos[!is.na(wdet$ExtraKilos)])
# -----------------------------------------------------
# OUTPUT DATASETS - MISSING VALUE TREATMENT DATASET ---
# wdet      - dataset without NAs
# wdet.NoNA - dataset with imputed NAs
# -----------------------------------------------------
# _____________________________________________________ OUTLIER TREATMENT - wdet
# Function to remove wdet outliers
# Parameters: dataframe, parm: default = 'q'
fun.wdet.boxplot <- function(x, parm = 'q', iqrTimes = 1.5) {

  beforeLength <- length(x[,1])
  
  if (parm == 'q') {
    boxValues <- fun.getLowerUpper(x$Qty, iqrTimes = iqrTimes) 
  } else {
    boxValues <- fun.getLowerUpper(x$ExtraKilos, iqrTimes = iqrTimes)
  }
  
  x <- x[(x$Qty> boxValues[1] & x$Qty < boxValues[2]),]
  afterLength <- length(x[,1])
  
  outPval <- ((beforeLength - afterLength) / beforeLength) * 100
  
  print(paste0('Number of observations before removing outliers = ', beforeLength))
  print(paste0('Number of observations after removing outliers = ', afterLength))
  
  # Calculate outlier percentage
  outPval <- round((( beforeLength - afterLength ) / beforeLength ) * 100, digits = 2)
  
  # Outlier percentage
  print(paste0('Percentage of removed observations = ',outPval,'%'))
  
  # Boxplot
  boxplot(x$Qty, x$ExtraKilos, horizontal = TRUE, names = c('Quantity','Extra Kilos'), 
          main = 'Boxplot work details / Quantity & Extra Kilos')

  # Add values over the boxplot
  text(x = summary(x$Qty)[-4], labels = round(summary(x$Qty)[-4], digits = 2), y = 1.5)
  text(x = summary(x$ExtraKilos)[-4], labels = round(summary(x$ExtraKilos)[-4], digits = 2), y = 2.5)  
  
  # dataset after outlier removal
  return(x)
    
}

# Applying fun.wdet.boxplot function for all the criteria
# Dataset 1 - Completed set - outliers in Qty
wdet.Result1 <- fun.wdet.boxplot(wdet.NoNA)

# This lines of code is commented to avoid breaking the program run 
# Dataset 2 - Completed set - outliers in Extra Kilos
#wdet.Result2 <- fun.wdet.boxplot(wdet.NoNA, parm = 'e')
 
# Dataset 3 - Imputed set - outliers in Qty
wdet.Result3 <- fun.wdet.boxplot(wdet, iqrTimes = 1.4)

# Dataset 4 - Imputed set - outliers in Extra Kilos
wdet.Result4 <- fun.wdet.boxplot(wdet, parm = 'e', iqrTimes = 1.5)
 
# Results
# wdet.NoNA, q - passed ( 0.89%)
# wdet.NoNA, e - failed - Exception
# wdet, q      - passed ( 0.99%)
# wdet, e      - failed (77.84%)
# _________________________________________________________ NORMALIZATION - wdet

# Dataset without NA
wdet.NoNA.n <- wdet.NoNA
wdet.NoNA.n[8:10] <- as.data.frame(lapply(wdet.NoNA[8:10], fun.normalize))
fun.wdet.boxplot(wdet.NoNA.n)

# Imputed dataset
wdet.n <- wdet
wdet.n[8:10] <- as.data.frame(lapply(wdet[8:10], fun.normalize))
fun.wdet.boxplot(wdet.n)


# _______________________________________________________________ Z-Score - wdet
# Dataset without NA
wdet.NoNA.z <- wdet.NoNA
wdet.NoNA.z[8:10] <- as.data.frame(lapply(wdet.NoNA[8:10], fun.z))

# Imputed dataset
wdet.z <- wdet
wdet.z[8:10] <- as.data.frame(lapply(wdet[8:10], fun.z))

# Remove garbage
rm(wdet.Result1, wdet.Result3, wdet.Result4, wdet, wdet.NoNA)
# --------------------------------------------------------
# FINAL DATASETS
# wdet.NoNA   - dataset without NAs
# wdet.NoNA.n - dataset without NAs - normalized
# wdet.NoNA.z - dataset without NAs - Z-Score

# wdet        - dataset with imputed NAs
# wdet.n      - dataset with imputed NAs - normalized
# wdet.z      - dataset with imputed NAs - z-Score
#
# --------------------------------------------------------

# ======================================= ANALYZING ALL THE DATASETS TO MAKE ASSUMPTIONS ====================
# Strcuture analysis

# ======================================= MERGE DATASETS ====================================================
# FINAL DATASETS - employee ------------------------------------
# emp.NoNA.n  - NA removed datset - normalized
# emp.NoNA.z  - NA removed datset - standarized

# emp.n       - total dataset treated imputation - normalized
# emp.z       - total dataset treated imputation - standarized

# FINAL DATASETS - fields --------------------------------------
# fld         - imputed dataset
# fld.NoNA    - NA removed dataset

# FINAL DATASETS - work code -----------------------------------

# FINAL DATASETS - work details --------------------------------
# wdet.NoNA.n - dataset without NAs - normalized
# wdet.NoNA.z - dataset without NAs - Z-Score

# wdet.n      - dataset with imputed NAs - normalized
# wdet.z      - dataset with imputed NAs - z-Score



# ======================================================================================
# ======================================= MERGED DATASETS ===============================

# Merge work details with employee - Min-Max normalization - NA omitted
estate.NAomt.n <- merge(wdet.NoNA.n, emp.NoNA.n, by = c('Estate','Division','EmpCode'))

# Merge work details with employee - Min-Max normalization - NA imputed
estate.imputed.n <- merge(wdet.n, emp.n, by = c('Estate','Division','EmpCode'))

# Merge work details with employee - z-score normalization - NA omitted
estate.NAomt.z <- merge(wdet.NoNA.z, emp.NoNA.z, by = c('Estate','Division','EmpCode'))

# Merge work details with employee - z-score normalization - NA imputed
estate.imputed.z <- merge(wdet.z, emp.z, by = c('Estate','Division','EmpCode'))

# Merge work details with employee - all records
estate.full <- merge(wdet.full, emp.full, by = c('Estate','Division','EmpCode'))

# ======================================= CREATE TRAINING & TEST DATASETS ====================================================

# -------------------------------------- HOLDOUT METHOD
# 75% - training, 25% - test
# NA omitted - normalized dataset
len <- length(estate.NAomt.n[,1])
lineTo <- round(len * 75 / 100, digits = 0)
estate.NAomt.n.train <- estate.NAomt.n[1:lineTo,c(9,8,16,11)]
estate.NAomt.n.test <- estate.NAomt.n[(lineTo+1):len,c(9,8,16,11)]

# NA omitted - z-score dataset
len <- length(estate.NAomt.z[,1])
lineTo <- round(len * 75 / 100, digits = 0)
estate.NAomt.z.train <- estate.NAomt.z[1:lineTo,c(9,8,16,11)]
estate.NAomt.z.test <- estate.NAomt.z[(lineTo+1):len,c(9,8,16,11)]

# NA imputed - normalized dataset
len <- length(estate.imputed.n[,1])
lineTo <- round(len * 75 / 100, digits = 0)
estate.imputed.n_train <- estate.imputed.n[1:lineTo,c(9,8,16,11)]
estate.imputed.n_test <- estate.imputed.n[(lineTo+1):len,c(9,8,16,11)]

# NA imputed - z-score dataset
len <- length(estate.imputed.z[,1])
lineTo <- round(len * 75 / 100, digits = 0)
estate.imputed.z_train <- estate.imputed.z[1:lineTo,c(9,8,16,11)]
estate.imputed.z_test <- estate.imputed.z[(lineTo+1):len,c(9,8,16,11)]

# Full datasets without omitting NAs
len <- length(estate.full[,1])
lineTo <- round(len * 75 / 100, digits = 0)
estate.full.train <- estate.full[1:lineTo,c(9,8,16,11)]
estate.full.test <- estate.full[(lineTo+1):len,c(9,8,16,11)]


# -------------------------------------- K-FOLD CROSS VALIDATE METHOD
# Applying k-fold method only for estate.NAomt.n dataset
# corssv_kfold funciton's library
library(modelr)

# k = 5, data is divided into 5 sets with training and test
cv <- crossv_kfold(estate.NAomt.n, k = 5)

# Show cv
cv

# Function to predic
fun_pred <- function(model, test_data) {
  data <- as.data.frame(test_data)
  pred <- add_predictions(data, model)
  return(pred)
}

#library(purrr)
#library(dplyr)

# Fit the models
models1 <- map(cv$train, ~lm(Qty ~ NumberofDays + service, data = .))
models2 <- map(cv$train, ~lm(Qty ~ NumberofDays + Gender, data = .))
models3 <-map(cv$train, ~lm(Qty ~ NumberofDays + Gender + service, data = .))

# Returns the predictions
pred1 <- map2_df(models1, cv$test, fun_pred, .id = 'Run')
pred2 <- map2_df(models2, cv$test, fun_pred, .id = 'Run')
pred3 <- map2_df(models3, cv$test, fun_pred, .id = 'Run')

# Calculate Mean Square Error (MSE)
mse1 <- pred1 %>% group_by(Run) %>% summarise(MSE = mean((Qty - pred)^2))
mse2 <- pred2 %>% group_by(Run) %>% summarise(MSE = mean((Qty - pred)^2))
mse3 <- pred3 %>% group_by(Run) %>% summarise(MSE = mean((Qty - pred)^2))

# Analyse
mse1
mse2
mse3

# -------------------------------------- BOOTSTRAP METHOD

# --------------------------------------------/==================\---------------------------------
# ==========================================<  APPLYING ML MODELS  >===============================
# --------------------------------------------\==================/---------------------------------
# ======================================================================================
# ======================================= TRAINING THE MODELS ===============================
# ========= MULTIPLE REGRESSION =========
# more informative scatterplot matrix
install.packages('psych')
library('psych')


# Function to view summary of the model using training datasets
fun_regression <- function(train, test, showPlot = FALSE) {

  # Plots
  if (showPlot) {
    pairs.panels(train) 
  }
  
  # applying all the selected variables in regression model
  est_model <- lm(Qty ~ ., data=train)
  
  # view summary
  print('Model summary')
  print(summary(est_model))
  
  # make predictions
  pred <- predict(est_model, test)
  
  print('test dataset header')
  print(head(test))
  print('predicted dataset header')
  print(head(pred))
  
  # Root Mean Square Error
  rmse_val <- sqrt(mean(pred-test$Qty)^2)
  print(paste0('Root Mean Square Error = ',rmse_val))
  
  # Sum of Square Error
  SSE <- sum((pred-test$Qty)^2)
  print(paste0('Sum of Square Error = ',SSE))
  
  # Sum of Square Total  
  SST <- sum((pred-mean(test$Qty))^2)
  print(paste0('Sum of Square Total = ',SST))
  
  # Return R-squared value
  return(summary(est_model)[8][1])

}

# Dataset 1
fun_regression(estate.NAomt.n.train, estate.NAomt.n.test, showPlot = TRUE)

# Dataset 2
fun_regression(estate.imputed.n_train, estate.imputed.n_test, showPlot = TRUE)

# Dataset 3
fun_regression(estate.NAomt.z.train, estate.NAomt.z.test, showPlot = TRUE)

# Dataset 4
fun_regression(estate.imputed.z_train, estate.imputed.z_test, showPlot = TRUE)

# Dataset 5
fun_regression(estate.full.train, estate.full.test, showPlot = TRUE)


# exploring relationships among features: ( This can be applied on for numerical variables)

# exploring relationships among features
cor(estate.NAomt.n.train[c('Qty','NumberofDays','service')])
cor(estate.NAomt.n.test[c('Qty','NumberofDays','service')])


# ========= NEURAL NETWORKS =========
# One of the libraries used to do neural networks
install.packages('neuralnet')
library(neuralnet)

# Dataset selected to trian neural network
# estate.NAomt.n.train - train dataset
# estate.NAomt.n.test - test dataset

# Function to do neural network train
fun.nn <- function(train, test, hidden = 1, showPLot = FALSE) {

  nn_model <- neuralnet(Qty ~ NumberofDays + service, data = train, 
                        hidden = hidden)
  
  # Plotting
  if(showPLot) {
    plot(nn_model)    
  }
  
  # Model results
  nn_results <- compute(nn_model, test[2:4])
  
  # Predicted quantity
  pred <- nn_results$net.result
  
  # return correlation between prediction and the test values
  return(cor(pred, test$Qty))
  
}

# Call the function and plot neural network.
fun.nn(estate.NAomt.n.train, estate.NAomt.n.test, hidden = 1, showPLot = TRUE)

# Improve model performance
fun.nn(estate.NAomt.n.train, estate.NAomt.n.test, hidden = 5, showPLot = TRUE)

# Improvment analysis

nn.HiddenVal = ''
nn.Correlation = ''

# looping the k value to get a list of k correlation against the k values
for (i in 1:10) {
  
  nn.Correlation[i] = fun.nn(estate.NAomt.n.train, estate.NAomt.n.test, 
                             hidden = i, showPLot = FALSE)
  nn.HiddenVal[i] <- i
}

# turn off all the devices to avoid unnecessary errors
dev.off()

# plot the k value and correlation 
plot(x = nn.HiddenVal, y = nn.Correlation, type = 'l', 
     main = 'Neural Networks - Hidden value and Correlation Analysis')


# ---------------------
#estate.NAomt.n <- estate.NAomt.n[c(9,10,8,14,15,16)]
#wdet.nona.n.emp.nona.n <- wdet.nona.n.emp.nona.n[c(9,10,14,15,16)]
#wdet.nona.n.emp.n <- wdet.nona.n.emp.n[c(9,10,14,15,16)]
#wdet.n.emp.nona.n <- wdet.n.emp.nona.n[c(9,10,14,15,16)]
#wdet.n.emp.n <- wdet.n.emp.n[c(9,10,14,15,16)]
#estate.imputed.n <- estate.imputed.n[c(9,10,8,14,15,16)]

#estate.NAomt.z <- estate.NAomt.z[c(9,10,8,14,15,16)]
#wdet.nona.z.emp.nona.z <- wdet.nona.z.emp.nona.z[c(9,10,14,15,16)]
#wdet.nona.z.emp.z <- wdet.nona.z.emp.z[c(9,10,14,15,16)]
#wdet.z.emp.nona.z <- wdet.z.emp.nona.z[c(9,10,14,15,16)]
#wdet.z.emp.z <- wdet.z.emp.z[c(9,10,14,15,16)]
#estate.imputed.z <- estate.imputed.z[c(9,10,8,14,15,16)]

# Remove garbage
rm(emp.n, emp.NoNA.n, emp.NoNA.z, emp.z, wdet.n, wdet.NoNA.n, wdet.NoNA.z, wdet.z, fld, fld.NoNA, wcod, wcod.NULL)

# View summaries of the cleaned datasets
str(estate.NAomt.n)
str(estate.imputed.n)
str(estate.NAomt.z)
str(estate.imputed.z)

# ========= REGRESSION TREE=========

tree.model <- rpart(Qty ~ ., data = estate.NAomt.n.train, method = 'anova')
rpart.plot(tree.model)

pred <- predict(tree.model, estate.NAomt.n.test)
head(pred)

# ========= KNN ====================
install.packages('class')
library('class')

install.packages('gmodels')
library('gmodels')

# Function to call knn function
fun.knn <- function(estate) {
  
#  c(8,11,16)
  
  estate$Gender <- ifelse(estate$Gender == 'Male',1,0)  
  
  # 75% records as train
  ln <- length(estate[,1])
  rn <- round(ln * 75 / 100, digits = 0)
  
  
  # train set
  train <- estate[1:rn,c(8,16)]
  
  # test set
  test <- estate[(rn + 1):ln,c(8,16)]
  
  train.labels <- estate[1:rn,9]
  test.labels <- estate[(rn + 1):ln,9]
  
  est.k <- ''
  est.ab <- ''
  est.ba <- ''
  
  for (i in 1:30) {
    print(i)
    pred <- knn(train = train, test = test, cl = train.labels, k=i)
    a <- CrossTable(test.labels, pred, prop.chisq = FALSE)
    est.ab[i] <- a$prop.tbl[2,1]
    est.ba[i] <- a$prop.tbl[1,2]
    est.k[i] <- i
  }
  
  est.abba <- data.frame(est.k, est.ab, est.ba, (as.numeric(est.ab) + as.numeric(est.ba)))
  
  colnames(est.abba)[1] <- 'k'
  colnames(est.abba)[2] <- 'col1'
  colnames(est.abba)[3] <- 'col2'
  colnames(est.abba)[4] <- 'tot'
  
  plot(x = est.abba$k, y = est.abba$tot, type = 'l')
  summary(est.abba)

}

# Call function
fun.knn(estate.NAomt.n)
