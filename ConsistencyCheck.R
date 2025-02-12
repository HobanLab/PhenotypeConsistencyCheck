# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % 02/10/2025 Consistency Check between Phenotype_i and Phenotype_f % 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# this script outputs a matrix of 30 rows and 50 columns of continuous and categorical
# data as a result of comparing initial and final measurements of two matrices
# Create objects ----
phenotype.wd <- "C:/Users/gsalas/Documents/PhenotypeConsistencyCheck/"
setwd(phenotype.wd)
initial_phenotype_measurement_matrix <- read.csv("butternut_hybrid_spreadsheet_Salas.csv")
final_phenotype_measurement_matrix <- read.csv("2025_01_22_blind_consistency_check.csv")
# ordinal columns
ordinal_columns <- c(6,7,11:16)
continuous_columns <- c(17:21,23:36,38:42)
# continous columns
# Create a matrix that will store the percent change and true or false statements
# and soft code the values for the rows and columns
results_matrix <- matrix(nrow = dim(initial_phenotype_measurement_matrix)[1], ncol = dim(initial_phenotype_measurement_matrix)[2])


# check the column names of both matrices
dimnames(initial_phenotype_measurement_matrix) 
dimnames(final_phenotype_measurement_matrix)

View(initial_phenotype_measurement_matrix)
View(final_phenotype_measurement_matrix)


# testing the true or false parameters by comparing the lenticel shape columns between both matrices
initial_phenotype_measurement_matrix[,6] == final_phenotype_measurement_matrix[,6]

results_matrix[,6] <- initial_phenotype_measurement_matrix[1:29,6] == final_phenotype_measurement_matrix[1:29,6]

# outer loop iterates through rows
for (x in 1:29) { 
  # inner loop iterates through columns
  for (y in ordinal_columns) {
    results_matrix[x,y]<- initial_phenotype_measurement_matrix[x,y] == final_phenotype_measurement_matrix[x,y]
  }
}
results_matrix[1:29,ordinal_columns]

for (i in 1:29) {
  for (j in continuous_columns) {
    results_matrix[i,j]<- initial_phenotype_measurement_matrix[i,j] - final_phenotype_measurement_matrix[i,j]
  }
}
results_matrix[1:29,]
View(results_matrix)
