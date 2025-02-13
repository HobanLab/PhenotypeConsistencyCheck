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

# ordinal columns
ordinal_columns <- c(6,7,11:16,22,38:44)
# continuous columns
continuous_columns <- c(17:21,23:27,28:32,33:37)
# Create a matrix that will store the percent change and true or false statements
# and soft code the values for the rows and columns
results_matrix <- matrix(nrow = dim(initial_phenotype_measurement_df)[1], ncol = dim(initial_phenotype_measurement_df)[2])
columns<-c(colnames(final_phenotype_measurement_df))
colnames(results_matrix) <- columns

# check the column names of both matrices
colnames(results_matrix)


# testing the true or false parameters by comparing the lenticel shape columns between both matrices
results_matrix[,ordinal_columns] <- initial_phenotype_measurement_df[,ordinal_columns] == final_phenotype_measurement_df[,ordinal_columns]

# testing the measurement differences for continuous data
results_matrix[4, 33:37] <- sort(as.double(initial_phenotype_measurement_df[4,33:37]), na.last = TRUE) - sort(as.double(final_phenotype_measurement_df[4,33:37]), na.last = TRUE)

# loop through the various columns 
for (i in 1:29) {
  results_matrix[i, 17:21] <- sort(as.double(initial_phenotype_measurement_df[i,17:21]), na.last = TRUE) - sort(as.double(final_phenotype_measurement_df[i,17:21]), na.last = TRUE)
  results_matrix[i, 23:27] <- sort(as.double(initial_phenotype_measurement_df[i,23:27]), na.last = TRUE) - sort(as.double(final_phenotype_measurement_df[i,23:27]), na.last = TRUE)
  results_matrix[i, 28:32] <- sort(as.double(initial_phenotype_measurement_df[i,28:32]), na.last = TRUE) - sort(as.double(final_phenotype_measurement_df[i,28:32]), na.last = TRUE)
  results_matrix[i, 33:37] <- sort(as.double(initial_phenotype_measurement_df[i,33:37]), na.last = TRUE) - sort(as.double(final_phenotype_measurement_df[i,33:37]), na.last = TRUE)
}  

results_df <- data.frame(results_matrix)

results_df[,ordinal_columns]<- results_df[,ordinal_columns] == 1

# continuous_results_matrix <- results_matrix[,continuous_columns]
# 
# ordinal_results_matrix <- matrix(nrow = dim(initial_phenotype_measurement_df)[1], ncol = dim(initial_phenotype_measurement_df)[2])
# columns<-c(colnames(final_phenotype_measurement_df))
# colnames(ordinal_results_matrix) <- columns
# ordinal_results_matrix[,ordinal_columns] <- initial_phenotype_measurement_df[,ordinal_columns] == final_phenotype_measurement_df[,ordinal_columns]
# ordinal_results_matrix <- ordinal_results_matrix[,ordinal_columns]
# 
# combined_results_matrix <- cbind(ordinal_results_matrix, continuous_results_matrix)
# 
# 
# combined_results_matrix
# 
# 
# 
# results_matrix[,17]
# View(results_matrix[,16:30])
