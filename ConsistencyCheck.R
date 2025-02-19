# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % 02/10/2025 Consistency Check between Phenotype_i and Phenotype_f % 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# this script outputs a matrix of 30 rows and 50 columns of continuous and categorical
# data as a result of comparing initial and final measurements of two matrices
# Create objects ----
phenotype.wd <- "C:/Users/gsalas/Documents/PhenotypeConsistencyCheck/"
setwd(phenotype.wd)
initial_phenotype_measurement_df <- read.csv("butternut_hybrid_spreadsheet_Salas.csv")
final_phenotype_measurement_df <- read.csv("2025_01_22_blind_consistency_check.csv")
# remove rows from dataset
initial_phenotype_measurement_df <- initial_phenotype_measurement_df[-c(30:999),]
final_phenotype_measurement_df <- final_phenotype_measurement_df[-c(30:999),]
# convert n/a to NA
initial_phenotype_measurement_df[initial_phenotype_measurement_df == "n/a"] <- NA
final_phenotype_measurement_df[final_phenotype_measurement_df == "n/a"] <- NA

# ordinal columns
ordinal_columns <- c(6:16,22,38,44)

# nominal columns 
nominal_columns <- c(39:43)
# continuous columns
continuous_columns <- c(17:21,23:27,28:32,33:37)

# Create a matrix that will store the percent change and true or false statements
# and soft code the values for the rows and columns
results_matrix <- matrix(nrow = dim(initial_phenotype_measurement_df)[1], ncol = dim(initial_phenotype_measurement_df)[2])

# concatenate column names into the columns object 
columns<-c(colnames(final_phenotype_measurement_df))

# add the column names to the results_matrix
colnames(results_matrix) <- columns

# vectorize the true or false parameters by comparing the lenticel shape columns between both matrices
results_matrix[,ordinal_columns] <- initial_phenotype_measurement_df[,ordinal_columns] == final_phenotype_measurement_df[,ordinal_columns]
  
for (i in 1:nrow(initial_phenotype_measurement_df)) {
  results_matrix[i,nominal_columns] <- sort(as.double(initial_phenotype_measurement_df[i,nominal_columns]), na.last = TRUE) == sort(as.double(final_phenotype_measurement_df[i,nominal_columns]), na.last = TRUE)
}




# for loop that iterates through the various columns
for (i in 1:nrow(results_matrix)) {
  results_matrix[i, 17:21] <- 100*(sort(as.double(final_phenotype_measurement_df[i,17:21]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,17:21]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,17:21]), na.last = TRUE)
  results_matrix[i, 23:27] <- 100*(sort(as.double(final_phenotype_measurement_df[i,23:27]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,23:27]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,23:27]), na.last = TRUE)
  results_matrix[i, 28:32] <- 100*(sort(as.double(final_phenotype_measurement_df[i,28:32]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,28:32]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,28:32]), na.last = TRUE)
  results_matrix[i, 33:37] <- 100*(sort(as.double(final_phenotype_measurement_df[i,33:37]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,33:37]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,33:37]), na.last = TRUE)
}  

# all the true false statements were turned to 1's and 0's after calling the  for loop. so we will want to change the matrix to a dataframe.
results_df <- data.frame(results_matrix)

# converting all the 1 and 0 values to TRUE or FALSE with boolean phrase
results_df[,c(ordinal_columns, nominal_columns)] <- results_df[,c(ordinal_columns,nominal_columns)] == 1

# save the csv file of the results
# write.csv(results_df, file = paste0(phenotype.wd, "consistencycheckResults.csv"))

# approaches to creating the table that calculates the percentage of TRUE values that matched after comparing the columns of both tables 
categorical_table_results <- as.data.frame(matrix(nrow = length(ordinal_columns)+length(nominal_columns), ncol = 2))

for (i in 1:length(nominal_columns)) {
  col_index <- nominal_columns[i]  # Get the actual column index
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

# categorical_table_results[19,1] <- colnames(results_df)[44]  # Store column name
# categorical_table_results[19,2] <- 100 * (sum(results_df[[44]], na.rm = TRUE) / sum(!is.na(results_df[[44]])))

# write.csv(categorical_table_results,paste0(phenotype.wd, "categoricaltableresults.csv"))


# continuousTable_results percent change
continuous_table_Average_percentChange <- as.data.frame(matrix(nrow = length(continuous_columns), ncol = 2))
for (i in 1:length(continuous_columns)) {
  col_index <- continuous_columns[i]  # Get the actual column index
  continuous_table_Average_percentChange[i,1] <- colnames(results_df)[col_index]  # Store column name
  continuous_table_Average_percentChange[i,2] <- sum(results_df[[col_index]], na.rm = TRUE) / sum(!is.na(results_df[[col_index]]))
}
write.csv(continuous_table_Average_percentChange,paste0(phenotype.wd, "continuous_table_Average_percentChange.csv"))