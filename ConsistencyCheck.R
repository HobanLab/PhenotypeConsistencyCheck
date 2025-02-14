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
# # data frame to matrix
# initial_phenotype_measurement_matrix <-  data.matrix(initial_phenotype_measurement_df)
# final_phenotype_measurement_matrix <- data.matrix(final_phenotype_measurement_df)
initial_vector_temp <- as.vector(as.matrix(initial_phenotype_measurement_df))

# ordinal columns
ordinal_columns <- c(6:16,22,38:44)
# continuous columns
continuous_columns <- c(17:21,23:27,28:32,33:37)
# Create a matrix that will store the percent change and true or false statements
# and soft code the values for the rows and columns
results_matrix <- matrix(nrow = dim(initial_phenotype_measurement_df)[1], ncol = dim(initial_phenotype_measurement_df)[2])
# concatenate column names into the columns object 
columns<-c(colnames(final_phenotype_measurement_df))
# add the column names to the results_matrix
colnames(results_matrix) <- columns

# double check the column names appear in the results matrix
colnames(results_matrix)


# vectorize the true or false parameters by comparing the lenticel shape columns between both matrices
results_matrix[,ordinal_columns] <- initial_phenotype_measurement_df[,ordinal_columns] == final_phenotype_measurement_df[,ordinal_columns]


# for loop that iterates through the various columns
for (i in 1:29) {
  results_matrix[i, 17:21] <- sort(as.double(initial_phenotype_measurement_df[i,17:21]), na.last = TRUE) - sort(as.double(final_phenotype_measurement_df[i,17:21]), na.last = TRUE)
  results_matrix[i, 23:27] <- sort(as.double(initial_phenotype_measurement_df[i,23:27]), na.last = TRUE) - sort(as.double(final_phenotype_measurement_df[i,23:27]), na.last = TRUE)
  results_matrix[i, 28:32] <- sort(as.double(initial_phenotype_measurement_df[i,28:32]), na.last = TRUE) - sort(as.double(final_phenotype_measurement_df[i,28:32]), na.last = TRUE)
  results_matrix[i, 33:37] <- sort(as.double(initial_phenotype_measurement_df[i,33:37]), na.last = TRUE) - sort(as.double(final_phenotype_measurement_df[i,33:37]), na.last = TRUE)
}  

# all the true false statements were turned to 1's and 0's after calling the  for loop. so we will want to change the matrix to a dataframe.
results_df <- data.frame(results_matrix)

# converting all the 1 and 0 values to TRUE or FALSE with boolean phrase
results_df[,ordinal_columns] <- results_df[,ordinal_columns] == 1

# save the csv file of the results
write.csv(results_df, file = paste0(phenotype.wd, "consistencycheckResults.csv"))

# approaches to creating the true/false table IN PROGRESS
sum(results_df[,6], na.rm = TRUE)