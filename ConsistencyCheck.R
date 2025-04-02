# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % 02/10/2025 Consistency Check between Phenotype_i and Phenotype_f % 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# install.packages("stringr")
# library(stringr)

# This script compares the measurements of various traits taken two times for 30 herbarium specimens. The traits measured were categorical and continuous data types. For the traits with categorical measurements, we calculated the percentage of measurements taken twice that remained the same. For traits with continuous measurements we calculated how much change there was between both the first and second measurement as a positive or negative percent change. Afterwards, for each categorical trait, we calculated the mean percent of traits that remained the same. For continuous traits, we also calculated the mean percent change for each continuous trait.
# Set work directory and create objects ----
phenotype.wd <- "C:/Users/gsalas/Documents/PhenotypeMeasurements/"
setwd(phenotype.wd)
# this object is a data frame that takes in the spreadsheet that measured traits for 30 herbarium specimens the first time.
initial_phenotype_measurement_df <- read.csv("2025_03_05_Updated_Spreadsheet_Blindcheck_1.csv", na.strings = c("Unsure/in between", "Evenly split", "Trait not present", "Trait not present ", "Trait not measurable/ obscured or image quality issue", "Trait to early to measure", "Trait too early to measure", "Trait not measurable/obscured or image quality issue"))
# this object is a data frame that takes in the spreadsheet that measured traits for 30 herbarium specimens the second time.
final_phenotype_measurement_df <- read.csv("2025_03_20_Updated_Spreadsheet_Blindcheck_2.csv", na.strings = c("Unsure/in between", "Evenly split", "Trait not present", "Trait not present ", "Trait not measurable/ obscured or image quality issue", "Trait to early to measure", "Trait too early to measure", "Trait not measurable/obscured or image quality issue"))
# x <- c("Evenly split", "Trait not present", "Trait not measurable/ obscured or image quality issue", "Trait to early to measure", "Trait too early to measure", "Trait not measurable/obscured or image quality issue")
# remove empty rows from both data sets
initial_phenotype_measurement_df <- initial_phenotype_measurement_df[-c(16:999),]
final_phenotype_measurement_df <- final_phenotype_measurement_df[-c(16:999),]
# str_detect(initial_phenotype_measurement_df$Lenticel.shape.0..Most.or.all.lenticels.are.small..round..white..and.abundant..evenly.distributed..If.elongated..the.elongation.is.perpendicular.to.the.branch..1..Moderate.number.of.lenticels.are.small..round..white..and.abundant.lenticels..with.patchy.distribution..where.moderate.is.20.50...If.elongated..the.elongation.is.parallel.to.the.branch..2..More.than.50..of.lenticels.are.large..tan..corky.lenticels.with.a.patchy.distribution..Many.are.dash.shaped.and.elongated.parallel.to.the.branch..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure., "Trait*")

# testssub <- apply(initial_phenotype_measurement_df, 2, function(x) str_detect(x,"Trait*"))
# apply(initial_phenotype_measurement_df, 2, function(x) str_replace_all(x, "Trait not present", "NA"))

# convert characters which R does not recognize to NA
# initial_phenotype_measurement_df[initial_phenotype_measurement_df == "Evenly split", "Trait not present", "Trait not measurable/ obscured or image quality issue", "Trait to early to measure", "Trait too early to measure", "Trait not measurable/obscured or image quality issue"] <- NA
# final_phenotype_measurement_df[final_phenotype_measurement_df == "n/a"] <- NA
# apply(initial_phenotype_measurement_df, 2, function(x) str_detect(x,"Trait*"))
# group the data types into different objects so they are easier to reference later on
# ordinal columns
ordinal_columns <- c(6:15,21,37,48)
# discrete columns 
discrete_columns <- c(38:42)
# continuous columns
continuous_columns <- c(16:20,22:36,43:47)

# Create a matrix that will store the percent change and percent of true statements and soft code the values for the rows and columns
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
templeafscarangle <- vector(length = nrow(results_matrix))
# this for loop iterates through each row of a range of columns and takes the sum of measurements that are NA. This is done for the dataset that measured individuals the first time and second time. This outputs a true or false statement and acts is placed in the ith position of the vector.
for (i in 1:nrow(results_matrix)) {
  templeaflengths_[i] <- sum(is.na(final_phenotype_measurement_df[i,16:20]))==sum(is.na(initial_phenotype_measurement_df[i,16:20]))
  tempnutlengths_[i] <- sum(is.na(final_phenotype_measurement_df[i,22:26]))==sum(is.na(initial_phenotype_measurement_df[i,22:26]))
  tempnutwidth_[i] <- sum(is.na(final_phenotype_measurement_df[i,27:31]))==sum(is.na(initial_phenotype_measurement_df[i,27:31]))
  tempcatkinlength_[i] <- sum(is.na(final_phenotype_measurement_df[i,32:36]))==sum(is.na(initial_phenotype_measurement_df[i,32:36]))
  templeafletcount[i] <- sum(is.na(final_phenotype_measurement_df[i,38:42]))==sum(is.na(initial_phenotype_measurement_df[i,38:42]))
  templeafscarangle[i] <- sum(is.na(final_phenotype_measurement_df[i,43:47]))==sum(is.na(initial_phenotype_measurement_df[i,43:47]))
}
# identify which vector position we see the amounts match up
sameLeaflengths_ <- which(templeaflengths_ == TRUE)
samenutlengths <- which(tempnutlengths_ == TRUE)
samenutwidth <- which(tempnutwidth_ == TRUE)
samecatkinlength <- which(tempcatkinlength_ == TRUE)
sameleafletcountamounts <- which(templeafletcount == TRUE)
sameleafscarangle <- which(templeafscarangle == TRUE)

# for loop that iterates through the various columns and calculates the percent change
# for (i in sameLeaflengths_) {
#   results_matrix[i, 16:20] <- 100*(sort(as.double(final_phenotype_measurement_df[i,16:20]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,16:20]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,16:20]), na.last = TRUE)
# } 
# 
# for (i in samenutlengths) {
#   results_matrix[i, 22:26] <- 100*(sort(as.double(final_phenotype_measurement_df[i,22:26]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,22:26]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,22:26]), na.last = TRUE)
# } 
# 
# for (i in samenutwidth) {
#   results_matrix[i, 27:31] <- 100*(sort(as.double(final_phenotype_measurement_df[i,27:31]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,27:31]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i, 27:31]), na.last = TRUE)
# } 
# 
# for (i in samecatkinlength) {
#   results_matrix[i, 32:36] <- 100*(sort(as.double(final_phenotype_measurement_df[i,32:36]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,32:36]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,32:36]), na.last = TRUE)
# }
# 
# for (i in sameleafscarangle) {
#   results_matrix[i, 43:47] <- 100*(sort(as.double(final_phenotype_measurement_df[i,43:47]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,43:47]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,43:47]), na.last = TRUE)
# }
for (i in sameLeaflengths_) {
  results_matrix[i, 16:20] <- 100*((as.double(final_phenotype_measurement_df[i,16:20])) - (as.double(initial_phenotype_measurement_df[i,16:20])))/(as.double(initial_phenotype_measurement_df[i,16:20]))
} 

for (i in samenutlengths) {
  results_matrix[i, 22:26] <- 100*((as.double(final_phenotype_measurement_df[i,22:26])) - (as.double(initial_phenotype_measurement_df[i,22:26])))/(as.double(initial_phenotype_measurement_df[i,22:26]))
} 

for (i in samenutwidth) {
  results_matrix[i, 27:31] <- 100*((as.double(final_phenotype_measurement_df[i,27:31])) - (as.double(initial_phenotype_measurement_df[i,27:31])))/(as.double(initial_phenotype_measurement_df[i,27:31]))
} 

for (i in samecatkinlength) {
  results_matrix[i, 32:36] <- 100*((as.double(final_phenotype_measurement_df[i,32:36])) - (as.double(initial_phenotype_measurement_df[i,32:36])))/(as.double(initial_phenotype_measurement_df[i,32:36]))
}

for (i in sameleafscarangle) {
  results_matrix[i, 43:47] <- 100*((as.double(final_phenotype_measurement_df[i,43:47])) - (as.double(initial_phenotype_measurement_df[i,43:47])))/(as.double(initial_phenotype_measurement_df[i,43:47]))
}
# iterate through each row after sorting discrete columns in ascending order and use true false to compare the leaflet counts  
for (i in sameleafletcountamounts) {
  results_matrix[i, discrete_columns] <- (as.double(initial_phenotype_measurement_df[i,discrete_columns])) == (as.double(final_phenotype_measurement_df[i,discrete_columns]))
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
write.csv(final_results_df, file = paste0(phenotype.wd, "consistencycheckResults_usingV2protocol.csv"))
# write.csv(continuous_results_df, file = paste0(phenotype.wd, "continuousresultsPercentChange.csv"))
write.csv(continuous_results_df, file = paste0(phenotype.wd, "continuousresultsPercentChange_usingV2protocol.csv"))

# approaches to creating the table that calculates the percentage of TRUE values that matched after comparing the columns of both tables 
categorical_table_results <- as.data.frame(matrix(nrow = length(ordinal_columns)+length(discrete_columns), ncol = 2))

for (i in 1:length(discrete_columns)) {
  col_index <- discrete_columns[i]  # Get the actual column index
  categorical_table_results[i+12,1] <- colnames(results_df)[col_index]  # Store column name
  categorical_table_results[i+12,2] <- 100 * (sum(results_df[[col_index]], na.rm = TRUE) / sum(!is.na(results_df[[col_index]])))
}

for (j in 1:length(ordinal_columns)) {
  if (j < 13){
    col_index <- ordinal_columns[j]  # Get the actual column index
    categorical_table_results[j,1] <- colnames(results_df)[col_index]  # Store column name
    categorical_table_results[j,2] <- 100 * (sum(results_df[[col_index]], na.rm = TRUE) / sum(!is.na(results_df[[col_index]])))
  }
  else {
    categorical_table_results[j+5,1] <- colnames(results_df)[col_index+11]
    categorical_table_results[j+5,2] <- 100 * (sum(results_df[[col_index+11]], na.rm = TRUE) / sum(!is.na(results_df[[col_index+11]])))
  }
}

categorical_table_results
# write.csv(categorical_table_results,paste0(phenotype.wd, "categoricaltableresults.csv"))
write.csv(categorical_table_results,paste0(phenotype.wd, "categoricaltableresults_usingV2protocolFIXED.csv"))


# continuousTable_results percent change
continuous_table_Average_percentChange <- as.data.frame(matrix(nrow = length(continuous_columns), ncol = 2))
for (i in 1:length(continuous_columns)) {
  col_index <- continuous_columns[i]  # Get the actual column index
  continuous_table_Average_percentChange[i,1] <- colnames(results_df)[col_index]  # Store column name
  continuous_table_Average_percentChange[i,2] <- sum(results_df[[col_index]], na.rm = TRUE) / sum(!is.na(results_df[[col_index]]))
}
write.csv(continuous_table_Average_percentChange,paste0(phenotype.wd, "continuous_table_Average_percentChange_usingV2protocol.csv"))