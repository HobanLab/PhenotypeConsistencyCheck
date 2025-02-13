# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % 02/10/2025 Consistency Check between Phenotype_i and Phenotype_f % 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# this script outputs a matrix of 30 rows and 50 columns of continuous and categorical
# data as a result of comparing initial and final measurements of two matrices
install.packages("wrapr")
library(wrapr)
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
continuous_columns <- c(17:21,23:37)
# Create a matrix that will store the percent change and true or false statements
# and soft code the values for the rows and columns
results_matrix <- matrix(nrow = dim(initial_phenotype_measurement_df)[1], ncol = dim(initial_phenotype_measurement_df)[2])
columns<-c(colnames(final_phenotype_measurement_df))
colnames(results_matrix) <- columns

# check the column names of both matrices
colnames(results_matrix)


# testing the true or false parameters by comparing the lenticel shape columns between both matrices
results_matrix[,ordinal_columns] <- initial_phenotype_measurement_df[,ordinal_columns] == final_phenotype_measurement_df[,ordinal_columns]


# # outer loop iterates through rows
# for (x in 1:29) { 
#   # inner loop iterates through columns
#   for (y in ordinal_columns) {
#     results_matrix[x,y]<- initial_phenotype_measurement_matrix[x,y] == final_phenotype_measurement_matrix[x,y]
#   }
# }
# results_matrix[1:29,ordinal_columns]



# results_matrix[1,c(17:21)] <- c(initial_phenotype_measurement_df[1,c(17:21)]) - sort.int(c(final_phenotype_measurement_df[1,c(17:21)]),na.last= NA)
# 
# 
# initial_phenotype_measurement_df[[17]][1] - final_phenotype_measurement_df[[17]][1]
# 
# results_matrix[,c(17:21)] <- sort(initial_phenotype_measurement_matrix[,c(17:21)]) - sort(final_phenotype_measurement_matrix[,c(17:21)])
# 
# View(results_matrix[1:29,c(ordinal_columns,17)])
# 
# for (i in 1:29) {
#     results_matrix[i,17] <- sort(initial_phenotype_measurement_matrix[i,17]) - sort(final_phenotype_measurement_matrix[i,17])
# }
# 
# 
# 
# results_matrix[,17]
# View(results_matrix[,16:30])
