# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % 02/10/2025 Consistency Check between Phenotype_i and Phenotype_f % 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This script compares the measurements of various traits taken two times for 30 herbarium specimens. The traits measured were categorical and continuous data types. For the traits with categorical measurements, we calculated the percentage of measurements taken twice that remained the same. For traits with continuous measurements we calculated how much change there was between both the first and second measurement as a positive or negative percent change. Afterwards, for each categorical trait, we calculated the mean percent of traits that remained the same. For continuous traits, we also calculated the mean percent change for each continuous trait.
# Set work directory and create objects ----
phenotype.wd <- "C:/Users/gsalas/Documents/PhenotypeConsistencyCheck/"
setwd(phenotype.wd)
# this object is a data frame that takes in the spreadsheet that measured traits for 30 herbarium specimens the first time.
initial_phenotype_measurement_df <- read.csv("butternut_hybrid_spreadsheet_Salas.csv")
# this object is a data frame that takes in the spreadsheet that measured traits for 30 herbarium specimens the second time.
final_phenotype_measurement_df <- read.csv("2025_01_22_blind_consistency_check.csv")
# remove empty rows from both data sets
initial_phenotype_measurement_df <- initial_phenotype_measurement_df[-c(30:999),]
final_phenotype_measurement_df <- final_phenotype_measurement_df[-c(30:999),]
# convert n/a characters which are R does not recognize to NA
initial_phenotype_measurement_df[initial_phenotype_measurement_df == "n/a"] <- NA
final_phenotype_measurement_df[final_phenotype_measurement_df == "n/a"] <- NA

# group the data types into different objects so they are easier to reference later on
# ordinal columns
ordinal_columns <- c(6:16,22,38,44)
# discrete columns 
discrete_columns <- c(39:43)
# continuous columns
continuous_columns <- c(17:21,23:27,28:32,33:37)

# Create a matrix that will store the percent change and true or false statements and soft code the values for the rows and columns
results_matrix <- matrix(nrow = dim(initial_phenotype_measurement_df)[1], ncol = dim(initial_phenotype_measurement_df)[2])

# concatenate column names into the columns object 
columns<-c(colnames(final_phenotype_measurement_df))

# add the column names to the results_matrix
colnames(results_matrix) <- columns

# vectorize the true or false parameters by comparing the lenticel shape columns between both matrices
results_matrix[,ordinal_columns] <- initial_phenotype_measurement_df[,ordinal_columns] == final_phenotype_measurement_df[,ordinal_columns]

# lines 41 - 73  act as a check to identify individuals where the amount (number of times) of measurements taken the first time do not match up with the amount of measurements take the second time. This applies to both continuous and discrete columns. The comparison method we are trying to do that leads up to the percent change calculation will not work when you attempt to compare two different amounts of measurements.
# first create vectors that will store the true/false statements across the rows for each continuous column 
templeaflengths_ <- vector(length = nrow(results_matrix))
tempnutlengths_ <- vector(length = nrow(results_matrix))
tempnutwidth_ <- vector(length = nrow(results_matrix))
tempcatkinlength_ <- vector(length = nrow(results_matrix))
templeafletcount <- vector(length = nrow(results_matrix))
# this for loop iterates through each row of a range of columns and takes the sum of measurements that are NA. This is done for the dataset that measured individuals the first time and second time. This outputs a true or false statement and acts is placed in the ith position of the vector.
for (i in 1:nrow(results_matrix)) {
  templeaflengths_[i] <- sum(is.na(final_phenotype_measurement_df[i,17:21]))==sum(is.na(initial_phenotype_measurement_df[i,17:21]))
  tempnutlengths_[i] <- sum(is.na(final_phenotype_measurement_df[i,23:27]))==sum(is.na(initial_phenotype_measurement_df[i,23:27]))
  tempnutwidth_[i] <- sum(is.na(final_phenotype_measurement_df[i,28:32]))==sum(is.na(initial_phenotype_measurement_df[i,28:32]))
  tempcatkinlength_[i] <- sum(is.na(final_phenotype_measurement_df[i,33:37]))==sum(is.na(initial_phenotype_measurement_df[i,33:37]))
  templeafletcount[i] <- sum(is.na(final_phenotype_measurement_df[i,39:43]))==sum(is.na(initial_phenotype_measurement_df[i,39:43]))
}
# identify which vector position we see the amounts match up
sameLeaflengths_ <- which(templeaflengths_ == TRUE)
samenutlengths <- which(tempnutlengths_ == TRUE)
samenutwidth <- which(tempnutwidth_ == TRUE)
samecatkinlength <- which(tempcatkinlength_ == TRUE)
sameleafletcountamounts <- which(templeafletcount == TRUE)

# for loop that iterates through the various columns and calculates the percent change
for (i in sameLeaflengths_) {
  results_matrix[i, 17:21] <- 100*(sort(as.double(final_phenotype_measurement_df[i,17:21]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,17:21]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,17:21]), na.last = TRUE)
} 

for (i in samenutlengths) {
  results_matrix[i, 23:27] <- 100*(sort(as.double(final_phenotype_measurement_df[i,23:27]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,23:27]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,23:27]), na.last = TRUE)
} 

for (i in samenutwidth) {
  results_matrix[i, 28:32] <- 100*(sort(as.double(final_phenotype_measurement_df[i,28:32]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,28:32]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,28:32]), na.last = TRUE)
} 

for (i in samecatkinlength) {
  results_matrix[i, 33:37] <- 100*(sort(as.double(final_phenotype_measurement_df[i,33:37]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,33:37]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,33:37]), na.last = TRUE)
}
# iterate through each row after sorting discrete columns in ascending order and use true false to compare the leaflet counts  
for (i in sameleafletcountamounts) {
  results_matrix[i, discrete_columns] <- sort(as.double(initial_phenotype_measurement_df[i,discrete_columns]), na.last = TRUE) == sort(as.double(final_phenotype_measurement_df[i,discrete_columns]), na.last = TRUE)
}
# temp_fixforconsistencyCheck <- results_matrix[,continuous_columns]
# write.csv(temp_fixforconsistencyCheck,paste0(phenotype.wd,"tempFixforConsistencyCONTINUOUS.csv"))

# all the true false statements were turned to 1's and 0's after calling the  for loop. so we will want to change the matrix to a data frame.
results_df <- data.frame(results_matrix)
# table of continouus percent change ONLY
continuous_results_df <- results_df[, continuous_columns]
# converting all the 1 and 0 values to TRUE or FALSE with boolean phrase
results_df[,c(ordinal_columns, discrete_columns)] <- results_df[,c(ordinal_columns,discrete_columns)] == 1
final_results_df <- results_df[,sort(c(continuous_columns,discrete_columns,ordinal_columns))]
# save the csv file of the results
# write.csv(results_df, file = paste0(phenotype.wd, "consistencycheckResults.csv"))
# write.csv(final_results_df, file = paste0(phenotype.wd, "consistencycheckResults_V2.csv"))
write.csv(final_results_df, file = paste0(phenotype.wd, "consistencycheckResults_V3.csv"))
# write.csv(continuous_results_df, file = paste0(phenotype.wd, "continuousresultsPercentChange.csv"))
write.csv(continuous_results_df, file = paste0(phenotype.wd, "continuousresultsPercentChangeV2.csv"))

# approaches to creating the table that calculates the percentage of TRUE values that matched after comparing the columns of both tables 
categorical_table_results <- as.data.frame(matrix(nrow = length(ordinal_columns)+length(discrete_columns), ncol = 2))

for (i in 1:length(discrete_columns)) {
  col_index <- discrete_columns[i]  # Get the actual column index
  categorical_table_results[i+13,1] <- colnames(results_df)[col_index]  # Store column name
  categorical_table_results[i+13,2] <- 100 * (sum(results_df[[col_index]], na.rm = TRUE) / sum(!is.na(results_df[[col_index]])))
}

for (j in 1:length(ordinal_columns)) {
  if (j < 14){
    col_index <- ordinal_columns[j]  # Get the actual column index
    categorical_table_results[j,1] <- colnames(results_df)[col_index]  # Store column name
    categorical_table_results[j,2] <- 100 * (sum(results_df[[col_index]], na.rm = TRUE) / sum(!is.na(results_df[[col_index]])))
  }
  else {
    categorical_table_results[j+5,1] <- colnames(results_df)[col_index+6]
    categorical_table_results[j+5,2] <- 100 * (sum(results_df[[col_index+6]], na.rm = TRUE) / sum(!is.na(results_df[[col_index+6]])))
  }
}

categorical_table_results
# write.csv(categorical_table_results,paste0(phenotype.wd, "categoricaltableresults.csv"))
write.csv(categorical_table_results,paste0(phenotype.wd, "categoricaltableresults_V2.csv"))


# continuousTable_results percent change
continuous_table_Average_percentChange <- as.data.frame(matrix(nrow = length(continuous_columns), ncol = 2))
for (i in 1:length(continuous_columns)) {
  col_index <- continuous_columns[i]  # Get the actual column index
  continuous_table_Average_percentChange[i,1] <- colnames(results_df)[col_index]  # Store column name
  continuous_table_Average_percentChange[i,2] <- sum(results_df[[col_index]], na.rm = TRUE) / sum(!is.na(results_df[[col_index]]))
}
write.csv(continuous_table_Average_percentChange,paste0(phenotype.wd, "continuous_table_Average_percentChange.csv"))