# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % 02/10/2025 Consistency Check between Phenotype_i and Phenotype_f % ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This script compares the measurements of various traits taken two times for 30 herbarium specimens. The traits measured were categorical and continuous data types. For the traits with categorical measurements, we calculated the percentage of measurements taken twice that remained the same. For traits with continuous measurements we calculated how much change there was between both the first and second measurement as a positive or negative percent change. Afterwards, for each categorical trait, we calculated the mean percent of traits that remained the same. For continuous traits, we also calculated the mean percent change for each continuous trait.
# install packages & library to change column names
install.packages("tidyverse")
library(dplyr)
# Set work directory and create objects ----
phenotype.wd <- "~/GitHub/PhenotypeConsistencyCheck/"
setwd(phenotype.wd)
# convert characters which R does not recognize to NA ----
# this object is a data frame that takes in the spreadsheet that measured traits for 30 herbarium specimens the first time.
initial_phenotype_measurement_df <- read.csv(paste0(phenotype.wd,"Consistency_Check_Sheets/V2_Protocol/2025_03_05_Updated_Spreadsheet_Blindcheck_1.csv"), na.strings = c("Unsure/in between", "Evenly split", "Trait not present", "Trait not present ", "Trait not measurable/ obscured or image quality issue", "Trait to early to measure", "Trait too early to measure", "Trait not measurable/obscured or image quality issue"), stringsAsFactors = FALSE) %>%
  rename(
    Lenticel_shape = Lenticel.shape.0..Most.or.all.lenticels.are.small..round..white..and.abundant..evenly.distributed..If.elongated..the.elongation.is.perpendicular.to.the.branch..1..Moderate.number.of.lenticels.are.small..round..white..and.abundant.lenticels..with.patchy.distribution..where.moderate.is.20.50...If.elongated..the.elongation.is.parallel.to.the.branch..2..More.than.50..of.lenticels.are.large..tan..corky.lenticels.with.a.patchy.distribution..Many.are.dash.shaped.and.elongated.parallel.to.the.branch..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
    Dormant_terminal_bud = Dormant.terminal.bud.If.the.terminal.bud.is.growing.DO.NOT.measure..Choose.one.of.the.following.0..Elongated..slender..conical.and.tan.colored.1..Broadest.at.the.base..less.elongated..and.slightly.green.colored.2..Stout..pyramid.shaped..green.or.yellow.green.in.color.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
    Thickness_of_twig = Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue,
    Color_of_twig = Color.of.twig.Dark.olive.green.to.red.brown.Tan.to.brown.Tan.or.light.green.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue,
    Hair_texture_of_twig = Hair.Texture.of.twig.Some..or.no..hairs.Patchy.hairs.Abundant.hairs.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
    Leaf_scar = Leaf.scar.0..Top.edge.of.most.leaf.scars.is.straight.or.slightly.arched.1..Top.edge.of.some.leaf.scars.has.a.small.descending..V..shaped.notched..an.arch..and.side.edges.are.lobed.2..Top.edge.of.most.or.all.leaf.scars.has.a.clear.descending..V..shaped.notch.and.side.edges.are.lobed.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
    Nut_shape_and_texture = Nut.shape.texture.if.present..without.husk....0..Cylindrical.nut..round.in.cross.section..with.thin..sharp.corrugations..The.suture.seam.is.not.easily.distinguished.from.the.longitudinal.ridges.1..Slightly.asymmetrical.nut..with.noticeable.valleys.between.longitudinal.ridges.2..Asymmetric..diamond.shaped.or.flattened.nut..with.dull.or.sparse.corrugations..The.suture.seam.is.easily.identified.and.forms.the.widest.part.of.the.nut.s.body.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
    Pith_color = Pith.color.0..Dark.chocolate.brown.1..Light.brown.2..tan.honey.brown.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
    Length_of_leaves = Leaves.0..Most.leaves.less.than.45.72.cm.long.1..Many.leaves.45.72.cm.or.longer.Evenly.split.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
    Length_of_catkins = Catkin.length.score.0...Shorter.than.11.43.cm.1..Betweeen.11.43...13.97.cm..2..Longer.than.13.97.cm.Evenly.split.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue
  )
final_phenotype_measurement_df <- read.csv(paste0(phenotype.wd,"Consistency_Check_Sheets/V2_Protocol/2025_03_20_Updated_Spreadsheet_Blindcheck_2.csv"), na.strings = c("Unsure/in between", "Evenly split", "Trait not present", "Trait not present ", "Trait not measurable/ obscured or image quality issue", "Trait to early to measure", "Trait too early to measure", "Trait not measurable/obscured or image quality issue"), stringsAsFactors = FALSE) %>%
  rename(
    Lenticel_shape = Lenticel.shape.0..Most.or.all.lenticels.are.small..round..white..and.abundant..evenly.distributed..If.elongated..the.elongation.is.perpendicular.to.the.branch..1..Moderate.number.of.lenticels.are.small..round..white..and.abundant.lenticels..with.patchy.distribution..where.moderate.is.20.50...If.elongated..the.elongation.is.parallel.to.the.branch..2..More.than.50..of.lenticels.are.large..tan..corky.lenticels.with.a.patchy.distribution..Many.are.dash.shaped.and.elongated.parallel.to.the.branch..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
    Dormant_terminal_bud = Dormant.terminal.bud.If.the.terminal.bud.is.growing.DO.NOT.measure..Choose.one.of.the.following.0..Elongated..slender..conical.and.tan.colored.1..Broadest.at.the.base..less.elongated..and.slightly.green.colored.2..Stout..pyramid.shaped..green.or.yellow.green.in.color.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
    Thickness_of_twig = Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue,
    Color_of_twig = Color.of.twig.Dark.olive.green.to.red.brown.Tan.to.brown.Tan.or.light.green.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue,
    Hair_texture_of_twig = Hair.Texture.of.twig.Some..or.no..hairs.Patchy.hairs.Abundant.hairs.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
    Leaf_scar = Leaf.scar.0..Top.edge.of.most.leaf.scars.is.straight.or.slightly.arched.1..Top.edge.of.some.leaf.scars.has.a.small.descending..V..shaped.notched..an.arch..and.side.edges.are.lobed.2..Top.edge.of.most.or.all.leaf.scars.has.a.clear.descending..V..shaped.notch.and.side.edges.are.lobed.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
    Nut_shape_and_texture = Nut.shape.texture.if.present..without.husk....0..Cylindrical.nut..round.in.cross.section..with.thin..sharp.corrugations..The.suture.seam.is.not.easily.distinguished.from.the.longitudinal.ridges.1..Slightly.asymmetrical.nut..with.noticeable.valleys.between.longitudinal.ridges.2..Asymmetric..diamond.shaped.or.flattened.nut..with.dull.or.sparse.corrugations..The.suture.seam.is.easily.identified.and.forms.the.widest.part.of.the.nut.s.body.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
    Pith_color = Pith.color.0..Dark.chocolate.brown.1..Light.brown.2..tan.honey.brown.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
    Length_of_leaves = Leaves.0..Most.leaves.less.than.45.72.cm.long.1..Many.leaves.45.72.cm.or.longer.Evenly.split.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
    Length_of_catkins = Catkin.length.score.0...Shorter.than.11.43.cm.1..Betweeen.11.43...13.97.cm..2..Longer.than.13.97.cm.Evenly.split.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue
  )

# remove empty rows from both data sets ----
initial_phenotype_measurement_df <- initial_phenotype_measurement_df[-c(16:999),]
final_phenotype_measurement_df <- final_phenotype_measurement_df[-c(16:999),]

# group the data types into different objects so they are easier to reference later on ----
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

# lines 64 - 87  act as a check to identify individuals where the amount (number of times) of measurements taken the first time do not match up with the amount of measurements take the second time. This applies to both continuous and discrete columns. The comparison method we are trying to do that leads up to the percent change calculation will not work when you attempt to compare two different amounts of measurements.
# first create vectors that will store the true/false statements across the rows for each continuous column 
templeaflengths_ <- vector(length = nrow(results_matrix))
tempnutlengths_ <- vector(length = nrow(results_matrix))
tempnutwidth_ <- vector(length = nrow(results_matrix))
tempcatkinlength_ <- vector(length = nrow(results_matrix))
templeafletcount <- vector(length = nrow(results_matrix))
templeafscarangle <- vector(length = nrow(results_matrix))
# Since we measured each individual twice, we want to compare measurements if the number of times we measured match up for each trait. This for loop iterates through each row (specimen) of a range of 5 columns (traits) and takes the sum of measurements that are NA. This is done for the dataset that measured individuals the first time and second time. This outputs a true or false statement and is placed in the ith position of the vector.
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

# lines 89 - 112: for loop that iterates through the various columns and calculates the percent change
for (i in sameLeaflengths_) {
  results_matrix[i, 16:20] <- 100*(sort(as.double(final_phenotype_measurement_df[i,16:20]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,16:20]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,16:20]), na.last = TRUE)
}

for (i in samenutlengths) {
  results_matrix[i, 22:26] <- 100*(sort(as.double(final_phenotype_measurement_df[i,22:26]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,22:26]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,22:26]), na.last = TRUE)
}

for (i in samenutwidth) {
  results_matrix[i, 27:31] <- 100*(sort(as.double(final_phenotype_measurement_df[i,27:31]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,27:31]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i, 27:31]), na.last = TRUE)
}

for (i in samecatkinlength) {
  results_matrix[i, 32:36] <- 100*(sort(as.double(final_phenotype_measurement_df[i,32:36]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,32:36]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,32:36]), na.last = TRUE)
}

for (i in sameleafscarangle) {
  results_matrix[i, 43:47] <- 100*(sort(as.double(final_phenotype_measurement_df[i,43:47]), na.last = TRUE) - sort(as.double(initial_phenotype_measurement_df[i,43:47]), na.last = TRUE))/sort(as.double(initial_phenotype_measurement_df[i,43:47]), na.last = TRUE)
}
# iterate through each row after sorting discrete columns in ascending order and use true false to compare the leaflet counts
for (i in sameleafletcountamounts) {
    results_matrix[i, discrete_columns] <- sort(as.double(initial_phenotype_measurement_df[i,discrete_columns]), na.last = TRUE) == sort(as.double(final_phenotype_measurement_df[i,discrete_columns]), na.last = TRUE)
}


# all the true false statements were turned to 1's and 0's after calling the  for loop. so we will want to change the matrix to a data frame.
results_df <- data.frame(results_matrix)
# table of continouus percent change ONLY
continuous_results_df <- results_df[, continuous_columns]
# converting all the 1 and 0 values to TRUE or FALSE with boolean phrase
results_df[,c(ordinal_columns, discrete_columns)] <- results_df[,c(ordinal_columns,discrete_columns)] == 1
final_results_df <- results_df[,sort(c(continuous_columns,discrete_columns,ordinal_columns))]
# save the csv file of the results
# write.csv(final_results_df, file = paste0(phenotype.wd, "Consistency_Check_Results/Comparisons/consistencycheckResults_usingV2protocolSORTED.csv"))

# approaches to creating the table that calculates the percentage of TRUE values that matched after comparing the columns of both tables 
# create a data frame that will store the calculations of the percentage for orinal & disrecete columns
categorical_table_results <- as.data.frame(matrix(nrow = length(ordinal_columns)+length(discrete_columns), ncol = 2))

# calculate the percentage for ordinal values. this for loop iterates through the discrete columns object and outputs the percentage calculations into the positions 13 - 17 in the categorical_table_results dataframe 
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

# calculate the percentage for discrete values. this for loop iterates through the discrete columns object and outputs the percentage calculations into the positions 13 - 17 in the categorical_table_results dataframe 
for (i in 1:length(discrete_columns)) {
  # Get the actual column index
  col_index <- discrete_columns[i]  
  # Store column name starting at the 13th row
  categorical_table_results[i+12,1] <- colnames(results_df)[col_index]
  # store the percentage calculation 
  categorical_table_results[i+12,2] <- 100 * (sum(results_df[[col_index]], na.rm = TRUE) / sum(!is.na(results_df[[col_index]])))
}


categorical_table_results
# write.csv(categorical_table_results,paste0(phenotype.wd, "Consistency_Check_Results/Stats/Percentages/categoricalpercentages_usingV2protocolSORTED.csv"))

# calculate the average percentage. this is different than the previous percent change calculation we did because for each trait, we will calculate the mean of the percent change across individuals.
# continuousTable_results percent change
continuous_table_Average_percentChange <- as.data.frame(matrix(nrow = length(continuous_columns), ncol = 2))
# calculate the average percent change. this for loop iterates through the continuous columns object and outputs the percent change calculations into the ith position of the categorical_table_results dataframe
for (i in 1:length(continuous_columns)) {
  col_index <- continuous_columns[i]  # Get the actual column index
  continuous_table_Average_percentChange[i,1] <- colnames(results_df)[col_index]  # Store column name
  continuous_table_Average_percentChange[i,2] <- sum(results_df[[col_index]], na.rm = TRUE) / sum(!is.na(results_df[[col_index]]))
}
# write.csv(continuous_table_Average_percentChange,paste0(phenotype.wd, "Consistency_Check_Results/Stats/Average_Percent_Change/continuous_Average_percentChange_usingV2protocolSORTED.csv"))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % 04/02 Calculate the mean  % ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# The goal of this portion of the script is to randomly sample 5 ordinal columns that calculate the overall phenotype score for butternut hybridity, take the sum of this sample, record the score, repeat this process 100 times, and calculate the mean. 

# update ordinal columns object to not include the score output in the random sampling
ordinal_columns_withoutScorecolumn <- c(6:11,13,15,21,37)
best_categorical_traits <- c(7,9, 11, 15, 37)

# creating a named vector: assign numeric values to twig characteristics  
thickness_column <- c("Slender" = 0, "Stout"= 1)
color_column <- c("Dark olive gree to red brown" = 0, "Tan to brown" = 1, "Tan or light green" = 2)
trichomes_column <- c("Some (or no) hairs" = 0, "Patchy hairs" = 1, "Abundant hairs" = 2)

# lines 184 - 190: each value indexed in initial_phenotype_df is looked up in each twig characteristic vector and returned as the corresponding value.
initial_phenotype_measurement_df$Thickness_of_twig <- thickness_column[initial_phenotype_measurement_df$Thickness_of_twig]
initial_phenotype_measurement_df$Color_of_twig <- color_column[initial_phenotype_measurement_df$Color_of_twig]
initial_phenotype_measurement_df$Hair_texture_of_twig <- trichomes_column[initial_phenotype_measurement_df$Hair_texture_of_twig]

final_phenotype_measurement_df$Thickness_of_twig <- thickness_column[final_phenotype_measurement_df$Thickness_of_twig]
final_phenotype_measurement_df$Color_of_twig <- color_column[final_phenotype_measurement_df$Color_of_twig]
final_phenotype_measurement_df$Hair_texture_of_twig <- trichomes_column[final_phenotype_measurement_df$Hair_texture_of_twig]

# find the sum for both data frames and add a new column to each for the total sum
initial_phenotype_measurement_df$Total_score <- rowSums(initial_phenotype_measurement_df[,ordinal_columns_withoutScorecolumn], na.rm = TRUE)
final_phenotype_measurement_df$Total_score <- rowSums(final_phenotype_measurement_df[,ordinal_columns_withoutScorecolumn], na.rm = TRUE)

# create a new vector that will store the mean scores after randomly sampling 5 individuals

# create a vector that stores the specimen number
Specimen <- vector(length = 15)
for (i in 1:15) {
  Specimen[i] <- paste0("Specimen ", i)
}
# create a vector that stores the specimen number names in column 1 and the scores that will fill in column 2
initial_ordinal_phenotypemeasurements <- matrix(nrow =length(1:nrow(initial_phenotype_measurement_df[1])), 
                                                ncol = 1, dimnames = list(Specimen,
                                                                          c("Mean score")))
final_ordinal_phenotypemeasurements <- matrix(nrow =length(1:nrow(final_phenotype_measurement_df[1])), 
                                              ncol = 1, dimnames = list(Specimen,
                                                                        c("Mean score")))

# Loop over each row
for (j in 1:15) {
  # Reset vector for this row
  test_vector_store_sums <- numeric(100)
  
  for (i in 1:100) {
    # Randomly sample 5 columns from the ordinal columns
    random_ordinal_columns <- sample(ordinal_columns_withoutScorecolumn, 5, replace = FALSE)
    
    # Sum values in that row and the sampled columns
    test_vector_store_sums[i] <- sum(initial_phenotype_measurement_df[j, random_ordinal_columns], na.rm = TRUE)
  }
  
  # Store the mean of the 100 sampled sums into the output matrix
  initial_ordinal_phenotypemeasurements[j,] <- mean(test_vector_store_sums)
}

# Loop over each row
for (j in 1:15) {
  # Reset vector for this row
  test_vector_store_sums <- numeric(100)
  
  for (i in 1:100) {
    # Randomly sample 5 columns from the ordinal columns
    random_ordinal_columns <- sample(ordinal_columns_withoutScorecolumn, 5, replace = FALSE)
    
    # Sum values in that row and the sampled columns
    test_vector_store_sums[i] <- sum(final_phenotype_measurement_df[j, random_ordinal_columns], na.rm = TRUE)
  }
  
  # Store the mean of the 100 sampled sums into the output matrix
  final_ordinal_phenotypemeasurements[j,] <- mean(test_vector_store_sums)
}


# write.csv(initial_ordinal_phenotypemeasurements, paste0(phenotype.wd, "Consistency_Check_Results/Comparisons/initial_ordinal_measurements100reps.csv"))
# write.csv(final_ordinal_phenotypemeasurements, paste0(phenotype.wd, "Consistency_Check_Results/Comparisons/final_ordinal_measurements100reps.csv"))

##
# create final table that shows the four approaches to calculate the hybridity score ---- 
##
initial_ordinal_SumvsMeanphenotypemeasurements <- matrix(nrow =length(1:nrow(initial_phenotype_measurement_df[1])), 
                                                         ncol = 4, dimnames = list(Specimen,
                                                                                   c("Sum of All Traits", "Mean of Randomly Sampled Traits", "Sum of Best Traits", "Sum of Purdue Traits")))
final_ordinal_SumvsMeanphenotypemeasurements <- matrix(nrow =length(1:nrow(initial_phenotype_measurement_df[1])), 
                                                       ncol = 4, dimnames = list(Specimen,
                                                                                 c("Sum of All Traits", "Mean of Randomly Sampled Traits", "Sum of Best Traits", "Sum of Purdue Traits")))

initial_ordinal_SumvsMeanphenotypemeasurements[,1] <- rowSums(initial_phenotype_measurement_df[,ordinal_columns_withoutScorecolumn], na.rm = TRUE)
initial_ordinal_SumvsMeanphenotypemeasurements[,2] <- initial_ordinal_phenotypemeasurements[,1]
initial_ordinal_SumvsMeanphenotypemeasurements[,3] <- rowSums(initial_phenotype_measurement_df[,best_categorical_traits], na.rm = TRUE)


final_ordinal_SumvsMeanphenotypemeasurements[,1] <- rowSums(final_phenotype_measurement_df[,ordinal_columns_withoutScorecolumn], na.rm = TRUE)
final_ordinal_SumvsMeanphenotypemeasurements[,2] <- final_ordinal_phenotypemeasurements[,1]
# find the sum for both data frames and add a new column to each for the total sum
final_ordinal_SumvsMeanphenotypemeasurements[,3] <- rowSums(final_phenotype_measurement_df[,best_categorical_traits], na.rm = TRUE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % Approach to calculating using the Purdue protocol %----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ordinal_columns_exceptTwigandScoreStandardCheck <- initial_phenotype_measurement_df[,c(6, 7,11, 13, 15, 21, 37)]
dormantTwigcolumnsStandardCheck <- initial_phenotype_measurement_df[,c(8:10)]
purdue_traitsStandardCheck <- round(rowSums(ordinal_columns_exceptTwigandScoreStandardCheck, na.rm = TRUE) + rowMeans(dormantTwigcolumnsStandardCheck, na.rm = TRUE), digits = 2)

initial_ordinal_SumvsMeanphenotypemeasurements[,4] <- purdue_traitsStandardCheck

ordinal_columns_exceptTwigandScoreBlindCheck <- final_phenotype_measurement_df[,c(6, 7,11, 13, 15, 21, 37)]
dormantTwigcolumnsBlindCheck <- final_phenotype_measurement_df[,c(8:10)]
purdue_traitsBlindCheck <- round(rowSums(ordinal_columns_exceptTwigandScoreBlindCheck, na.rm = TRUE) + rowMeans(dormantTwigcolumnsBlindCheck, na.rm = TRUE), digits = 2)


final_ordinal_SumvsMeanphenotypemeasurements[,4] <- purdue_traits


write.csv(initial_ordinal_SumvsMeanphenotypemeasurements, paste0(phenotype.wd,"Consistency_Check_Results/Comparisons/StandardCheck_sumvsmeanvsbesttraits.csv"))
write.csv(final_ordinal_SumvsMeanphenotypemeasurements, paste0(phenotype.wd,"Consistency_Check_Results/Comparisons/BlindCheck_sumvsmeanvsbesttraits.csv"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#% 04/17/2025 Data visualization of GBIF specimens %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# install.packages("sf")
# install.packages("rnaturalearth")
# install.packages("maps")
# library(sf)
# library(ggplot2)
# library(rnaturalearth)
# library(maps)
# 
# testdf <- 
# 
# initial_phenotype_measurement_df_maps <- initial_phenotype_measurement_df[-c(2,10,16:999),]
# final_phenotype_measurement_df_maps <- final_phenotype_measurement_df[-c(2,10,16:999),]
# 
# test_sf <- st_as_sf(initial_phenotype_measurement_df_maps, coords = c('Latitude', 'Longitude'))
# 
# test_sf <- st_set_crs(test_sf, crs = "WGS84")
# 
# ecoregionmap <- sf::st_read(
#   dsn = paste0(phenotype.wd, "Shapefiles/NA_CEC_Eco_Level3/NA_CEC_Eco_Level3.shp")
# )
# 
# plot(
#   x = ecoregionmap[,1] 
# )
# 
# usa <- map_data("usa")
# head(usa)
# df = head(usa)
# map(database = "world")
# text(x = initial_phenotype_measurement_df_maps$Latitude, y = initial_phenotype_measurement_df_maps$Latitude, col = "Red")
# 
# base_map <- map_data("usa")
# 
# p <- ggplot() + coord_fixed() +
#   xlab("") + ylab("")
# base_usa <- p + geom_polygon(data = ecoregionmap, aes(x = long, y = lat, group = group))
# 
# map_data <-
#   base_usa +
#   geom_point(data = initial_phenotype_measurement_df_maps,
#              aes(x=Longitude, y=Latitude), colour = "Deep Pink")
# 
# 
# 
# #####
# #
# #####
# # install.packages("rgbif")
# # install.packages("remotes")
# # install.packages("terra")
# # remotes::install_github("ropensci/rnaturalearthhires")
# # remotes::install_github("8Ginette8/gbif.range")
# # library(gbif.range)
# # library(terra)
# # 
# obs.pt <- get_gbif(sp_name = "Juglans cinerea")
# USA <- rnaturalearth::ne_countries (type = "countries", returnclass = "sv")
# terra::plot(USA, col = "#bcbddc")
# # points(obs.pt[, c("decimalLongitude, decimalLatitude")], pch = 20, col = "#99340470", cex = 1.5)
# points(obs.pt[, c("decimalLongitude","decimalLatitude")], pch = 20, col = "#99340470", cex = 1.5)
# 
# get_status("Juglans cinerea", all = FALSE)
# 
# eco.terra <- read_bioreg(bioreg_name = "eco_terra", save_dir = NULL)
# 
# range.butternut <- get_range(occ_coord = obs.pt,
#                              bioreg = eco.terra,
#                              bioreg_name = "ECO_NAME",
#                              degrees_outlier = .5,
#                              clustered_points_outlier = 20 )
# terra::plot(countries, col = '#bcbddc')
# terra::plot(range.butternut$rangeOutput, col = "#238b45", add = TRUE, axes = FALSE, legend = FALSE)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Install packages if needed
# install.packages(c("rgbif", "terra", "rnaturalearth", "rnaturalearthdata"))
# remotes::install_github("ropensci/rnaturalearthhires")
# remotes::install_github("8Ginette8/gbif.range")
# 
# # Load libraries
# library(gbif.range)
# library(rgbif)
# library(terra)
# library(rnaturalearth)
# library(rnaturalearthhires)
# 
# # Download GBIF points for Juglans cinerea
# obs.pt <- get_gbif(sp_name = "Juglans cinerea")
# 
# # Optional: Preview or clean GBIF data
# obs.pt <- obs.pt[!is.na(obs.pt$decimalLongitude) & !is.na(obs.pt$decimalLatitude), ]
# 
# # Filter for points in Eastern US only (approx long < -80 and lat > 30)
# obs.pt <- subset(obs.pt, decimalLongitude < -70 & decimalLongitude > -95 & 
#                    decimalLatitude > 30 & decimalLatitude < 50)
# 
# # Read in your own dataset (replace "mydata.csv" with your actual file)
# # Make sure it has columns: latitude and longitude (or rename accordingly)
# my_pts <- initial_phenotype_measurement_df_maps
# my_pts <- my_pts[!is.na(my_pts$Longitude) & !is.na(my_pts$Latitude), ]
# 
# # Convert to 'SpatVector' for plotting
# obs_vect <- vect(obs.pt, geom = c("decimalLongitude", "decimalLatitude"), crs = "ESPG:4326")
# 
# my_vect  <- vect(my_pts[, c("longitude", "latitude")], crs = "EPSG:4326")
# 
# 
# 
# 
# install.packages(c("sf", "ggplot2", "dplyr", "readr", "rnaturalearth", "rnaturalearthdata", "rmapshaper"))
# 
# library(sf)
# library(ggplot2)
# library(dplyr)
# library(readr)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(rmapshaper)
# initial_phenotype_measurement_df <- read.csv(paste0(phenotype.wd,"Consistency_Check_Sheets/V2_Protocol/2025_03_05_Updated_Spreadsheet_Blindcheck_1.csv"), na.strings = c("Unsure/in between", "Evenly split", "Trait not present", "Trait not present ", "Trait not measurable/ obscured or image quality issue", "Trait to early to measure", "Trait too early to measure", "Trait not measurable/obscured or image quality issue"), stringsAsFactors = FALSE) %>%
#   rename(
#     Lenticel_shape = Lenticel.shape.0..Most.or.all.lenticels.are.small..round..white..and.abundant..evenly.distributed..If.elongated..the.elongation.is.perpendicular.to.the.branch..1..Moderate.number.of.lenticels.are.small..round..white..and.abundant.lenticels..with.patchy.distribution..where.moderate.is.20.50...If.elongated..the.elongation.is.parallel.to.the.branch..2..More.than.50..of.lenticels.are.large..tan..corky.lenticels.with.a.patchy.distribution..Many.are.dash.shaped.and.elongated.parallel.to.the.branch..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
#     Dormant_terminal_bud = Dormant.terminal.bud.If.the.terminal.bud.is.growing.DO.NOT.measure..Choose.one.of.the.following.0..Elongated..slender..conical.and.tan.colored.1..Broadest.at.the.base..less.elongated..and.slightly.green.colored.2..Stout..pyramid.shaped..green.or.yellow.green.in.color.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
#     Thickness_of_twig = Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue,
#     Color_of_twig = Color.of.twig.Dark.olive.green.to.red.brown.Tan.to.brown.Tan.or.light.green.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue,
#     Hair_texture_of_twig = Hair.Texture.of.twig.Some..or.no..hairs.Patchy.hairs.Abundant.hairs.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
#     Leaf_scar = Leaf.scar.0..Top.edge.of.most.leaf.scars.is.straight.or.slightly.arched.1..Top.edge.of.some.leaf.scars.has.a.small.descending..V..shaped.notched..an.arch..and.side.edges.are.lobed.2..Top.edge.of.most.or.all.leaf.scars.has.a.clear.descending..V..shaped.notch.and.side.edges.are.lobed.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
#     Nut_shape_and_texture = Nut.shape.texture.if.present..without.husk....0..Cylindrical.nut..round.in.cross.section..with.thin..sharp.corrugations..The.suture.seam.is.not.easily.distinguished.from.the.longitudinal.ridges.1..Slightly.asymmetrical.nut..with.noticeable.valleys.between.longitudinal.ridges.2..Asymmetric..diamond.shaped.or.flattened.nut..with.dull.or.sparse.corrugations..The.suture.seam.is.easily.identified.and.forms.the.widest.part.of.the.nut.s.body.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
#     Pith_color = Pith.color.0..Dark.chocolate.brown.1..Light.brown.2..tan.honey.brown.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
#     Length_of_leaves = Leaves.0..Most.leaves.less.than.45.72.cm.long.1..Many.leaves.45.72.cm.or.longer.Evenly.split.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
#     Length_of_catkins = Catkin.length.score.0...Shorter.than.11.43.cm.1..Betweeen.11.43...13.97.cm..2..Longer.than.13.97.cm.Evenly.split.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue
#   )
# initial_phenotype_measurement_df_maps <- initial_phenotype_measurement_df[-c(2,10,16:999),]
# 
# my_pts <- initial_phenotype_measurement_df_maps
# # crs refers to the coordinate system which is WGS84
# my_pts_sf <- st_as_sf(my_pts, coords = c("Longitude","Latitude"), crs = 4326)
# 
# usa <- ne_states(country = "United States of America", returnclass = "sf") %>%
#   filter(!name %in% c("Hawaii", "Alaska", "Puerto Rico"))
# ecoregions <- st_read(paste0(phenotype.wd, "Shapefiles/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp"))
# ecoregions <- st_transform(ecoregions, crs = 4326)
# 
# # Optional: simplify to speed up plotting
# ecoregions_simplified <- ms_simplify(ecoregions, keep = 0.05)
# 
# 
# ggplot() +
#   geom_sf(data = ecoregions_simplified, aes(fill = NA), color = "gray60", size = 0.2) +
#   geom_sf(data = usa, fill = NA, color = "black") +
#   geom_sf(data = my_pts_sf, color = "#d95f0e", size = 2, shape = 21, fill = "#d95f0e") +
#   coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
#   theme_minimal() +
#   labs(title = "My Specimen Points Over EPA Level III Ecoregions",
#        subtitle = "Contiguous United States")

# # ggplot() +
# #   geom_sf(data = ecoregions_simplified, aes(fill = US_L3NAME), color = "white", size = 0.2, alpha = 0.8) +
# #   geom_sf(data = usa, fill = NA, color = "black", size = 0.3) +
# #   geom_sf(data = my_pts_sf, color = "black", fill = "#d95f0e", size = 3, shape = 21, stroke = 0.4) +
# #   scale_fill_viridis_d(option = "C", guide = guide_legend(title = "EPA Level III Ecoregions")) +
# #   coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
# #   labs(title = "Specimen Points within EPA Level III Ecoregions",
# #        subtitle = "Overlay of field/herbarium data on ecological regions of the U.S.",
# #        caption = "Base map: EPA Level III | Points: Your data") +
# #   theme_minimal(base_size = 12) +
# #   theme(
# #     legend.position = "bottom",
# #     legend.key.height = unit(0.3, "cm"),
# #     legend.key.width = unit(2.5, "cm"),
# #     plot.title = element_text(face = "bold", size = 15),
# #     plot.subtitle = element_text(size = 12)
# #   )
# 

# install packages & library to change column names
install.packages("tidyverse")
install.packages("rgbif")
install.packages("remotes")
install.packages("terra")
install.packages("sf")
install.packages("rnaturalearth")
remotes::install_github("ropensci/rnaturalearthhires")
remotes::install_github("8Ginette8/gbif.range")
library(gbif.range)
library(rgbif)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(rnaturalearth)
library(dplyr)
# Set work directory and create objects ----
phenotype.wd <- "~/GitHub/PhenotypeConsistencyCheck/"
setwd(phenotype.wd)
initial_phenotype_measurement_df <- read.csv(paste0(phenotype.wd,"Consistency_Check_Sheets/V2_Protocol/2025_03_05_Updated_Spreadsheet_Blindcheck_1.csv"), na.strings = c("Unsure/in between", "Evenly split", "Trait not present", "Trait not present ", "Trait not measurable/ obscured or image quality issue", "Trait to early to measure", "Trait too early to measure", "Trait not measurable/obscured or image quality issue"), stringsAsFactors = FALSE) %>%
  rename(
    Lenticel_shape = Lenticel.shape.0..Most.or.all.lenticels.are.small..round..white..and.abundant..evenly.distributed..If.elongated..the.elongation.is.perpendicular.to.the.branch..1..Moderate.number.of.lenticels.are.small..round..white..and.abundant.lenticels..with.patchy.distribution..where.moderate.is.20.50...If.elongated..the.elongation.is.parallel.to.the.branch..2..More.than.50..of.lenticels.are.large..tan..corky.lenticels.with.a.patchy.distribution..Many.are.dash.shaped.and.elongated.parallel.to.the.branch..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
    Dormant_terminal_bud = Dormant.terminal.bud.If.the.terminal.bud.is.growing.DO.NOT.measure..Choose.one.of.the.following.0..Elongated..slender..conical.and.tan.colored.1..Broadest.at.the.base..less.elongated..and.slightly.green.colored.2..Stout..pyramid.shaped..green.or.yellow.green.in.color.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
    Thickness_of_twig = Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue,
    Color_of_twig = Color.of.twig.Dark.olive.green.to.red.brown.Tan.to.brown.Tan.or.light.green.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue,
    Hair_texture_of_twig = Hair.Texture.of.twig.Some..or.no..hairs.Patchy.hairs.Abundant.hairs.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
    Leaf_scar = Leaf.scar.0..Top.edge.of.most.leaf.scars.is.straight.or.slightly.arched.1..Top.edge.of.some.leaf.scars.has.a.small.descending..V..shaped.notched..an.arch..and.side.edges.are.lobed.2..Top.edge.of.most.or.all.leaf.scars.has.a.clear.descending..V..shaped.notch.and.side.edges.are.lobed.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
    Nut_shape_and_texture = Nut.shape.texture.if.present..without.husk....0..Cylindrical.nut..round.in.cross.section..with.thin..sharp.corrugations..The.suture.seam.is.not.easily.distinguished.from.the.longitudinal.ridges.1..Slightly.asymmetrical.nut..with.noticeable.valleys.between.longitudinal.ridges.2..Asymmetric..diamond.shaped.or.flattened.nut..with.dull.or.sparse.corrugations..The.suture.seam.is.easily.identified.and.forms.the.widest.part.of.the.nut.s.body.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
    Pith_color = Pith.color.0..Dark.chocolate.brown.1..Light.brown.2..tan.honey.brown.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
    Length_of_leaves = Leaves.0..Most.leaves.less.than.45.72.cm.long.1..Many.leaves.45.72.cm.or.longer.Evenly.split.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
    Length_of_catkins = Catkin.length.score.0...Shorter.than.11.43.cm.1..Betweeen.11.43...13.97.cm..2..Longer.than.13.97.cm.Evenly.split.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue
  )
initial_phenotype_measurement_df_maps <- initial_phenotype_measurement_df[-c(30:999),]

butternut_gbif <- occ_search(
  scientificName = "Juglans cinerea",
  hasCoordinate = TRUE,
)

# Extract just the data
butternut_df <- butternut_gbif$data %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))

# Convert to sf object
butternut_sf <- st_as_sf(
  butternut_df,
  coords = c("decimalLongitude", "decimalLatitude"),
  # this is the coordinate system which is WGS84
  crs = 4326
)

my_pts <- initial_phenotype_measurement_df_maps
my_pts_clean <- my_pts %>%
  filter(!is.na(Longitude) & !is.na(Latitude))
my_pts_sf <- st_as_sf(my_pts_clean, coords = c("Longitude", "Latitude"), crs = 4326)


# my_pts_sf <- st_as_sf(my_pts, coords = c("Longitude", "Latitude"), crs = 4326)

usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Hawaii", "Alaska", "Puerto Rico"))

ggplot() +
  geom_sf(data = usa, fill = "lightgray", color = "gray") +
  # geom_sf(data = butternut_sf, color = "#993404", size = 1, alpha = 0.5) +
  geom_sf(data = my_pts_sf, color = "#0570b0", fill = "#92c5de", size = 2, shape = 21, stroke = 1.2) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +  # zoom to eastern US
  theme_minimal() +
  labs(
    title = "Butternut (Juglans cinerea) Map"
  )

###
#
###
# Convert numeric months to factor with month names for plotting
initial_phenotype_measurement_df$Month <- factor(initial_phenotype_measurement_df$Month, levels = 1:12, labels = month.name)

# Count and plot
month_table <- table(initial_phenotype_measurement_df$Month)

barplot(month_table,
        las = 2,
        col = "skyblue",
        main = "Number of Individuals per Month",
        ylab = "Count",
        xlab = "Month")

initial_phenotype_measurement_df$Decade <- floor(initial_phenotype_measurement_df$Year / 10) * 10

decade_table <- table(initial_phenotype_measurement_df$Decade)

barplot(decade_table,
        col = "red",
        main = "Number of Individuals by Decade",
        ylab = "Count",
        xlab = "Decade")
# 
# 
# 
# 
# 
# phenotype.wd <- "~/GitHub/PhenotypeConsistencyCheck/"
# setwd(phenotype.wd)
# initial_phenotype_measurement_df <- read.csv(paste0(phenotype.wd,"Consistency_Check_Sheets/V2_Protocol/2025_03_05_Updated_Spreadsheet_Blindcheck_1.csv"), na.strings = c("Unsure/in between", "Evenly split", "Trait not present", "Trait not present ", "Trait not measurable/ obscured or image quality issue", "Trait to early to measure", "Trait too early to measure", "Trait not measurable/obscured or image quality issue"), stringsAsFactors = FALSE) %>%
#   rename(
#     Lenticel_shape = Lenticel.shape.0..Most.or.all.lenticels.are.small..round..white..and.abundant..evenly.distributed..If.elongated..the.elongation.is.perpendicular.to.the.branch..1..Moderate.number.of.lenticels.are.small..round..white..and.abundant.lenticels..with.patchy.distribution..where.moderate.is.20.50...If.elongated..the.elongation.is.parallel.to.the.branch..2..More.than.50..of.lenticels.are.large..tan..corky.lenticels.with.a.patchy.distribution..Many.are.dash.shaped.and.elongated.parallel.to.the.branch..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
#     Dormant_terminal_bud = Dormant.terminal.bud.If.the.terminal.bud.is.growing.DO.NOT.measure..Choose.one.of.the.following.0..Elongated..slender..conical.and.tan.colored.1..Broadest.at.the.base..less.elongated..and.slightly.green.colored.2..Stout..pyramid.shaped..green.or.yellow.green.in.color.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
#     Thickness_of_twig = Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue,
#     Color_of_twig = Color.of.twig.Dark.olive.green.to.red.brown.Tan.to.brown.Tan.or.light.green.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue,
#     Hair_texture_of_twig = Hair.Texture.of.twig.Some..or.no..hairs.Patchy.hairs.Abundant.hairs.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
#     Leaf_scar = Leaf.scar.0..Top.edge.of.most.leaf.scars.is.straight.or.slightly.arched.1..Top.edge.of.some.leaf.scars.has.a.small.descending..V..shaped.notched..an.arch..and.side.edges.are.lobed.2..Top.edge.of.most.or.all.leaf.scars.has.a.clear.descending..V..shaped.notch.and.side.edges.are.lobed.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
#     Nut_shape_and_texture = Nut.shape.texture.if.present..without.husk....0..Cylindrical.nut..round.in.cross.section..with.thin..sharp.corrugations..The.suture.seam.is.not.easily.distinguished.from.the.longitudinal.ridges.1..Slightly.asymmetrical.nut..with.noticeable.valleys.between.longitudinal.ridges.2..Asymmetric..diamond.shaped.or.flattened.nut..with.dull.or.sparse.corrugations..The.suture.seam.is.easily.identified.and.forms.the.widest.part.of.the.nut.s.body.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure.,
#     Pith_color = Pith.color.0..Dark.chocolate.brown.1..Light.brown.2..tan.honey.brown.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
#     Length_of_leaves = Leaves.0..Most.leaves.less.than.45.72.cm.long.1..Many.leaves.45.72.cm.or.longer.Evenly.split.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure,
#     Length_of_catkins = Catkin.length.score.0...Shorter.than.11.43.cm.1..Betweeen.11.43...13.97.cm..2..Longer.than.13.97.cm.Evenly.split.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue
#   )
# initial_phenotype_measurement_df_maps <- initial_phenotype_measurement_df[-c(30:999),]
# 
# butternut_gbif <- occ_search(
#   scientificName = "Juglans cinerea",
#   hasCoordinate = TRUE,
# )
# 
# # Extract just the data
# butternut_df <- butternut_gbif$data %>%
#   filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))
# 
# # Convert to sf object
# butternut_sf <- st_as_sf(
#   butternut_df,
#   coords = c("decimalLongitude", "decimalLatitude"),
#   crs = 4326
# )
# 
# my_pts <- initial_phenotype_measurement_df_maps
# 
# my_pts_sf <- st_as_sf(my_pts, coords = c("Longitude", "Latitude"), crs = 4326)
# my_pts_sf <- my_pts %>%
#   filter(!is.na(Longitude) & !is.na(Latitude)) %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
# plot(my_pts_sf)
# 
# usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
#   filter(!name %in% c("Hawaii", "Alaska", "Puerto Rico"))
# 
# ggplot() +
#   geom_sf(data = usa, fill = "lightgray", color = "gray") +
#   geom_sf(data = butternut_sf, color = "#993404", size = 1, alpha = 0.5) +
#   # geom_sf(data = my_pts_sf, color = "#0570b0", fill = "#92c5de", size = 2, shape = 21, stroke = 1.2) +
#   coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +  # zoom to eastern US
#   theme_minimal() +
#   labs(
#     title = "Butternut (*Juglans cinerea*) Distribution",
#     subtitle = "GBIF records (brown) with overlay of research points (blue)",
#     caption = "Source: GBIF.org & personal data"
#   )
# 
# 
# 
# 
# 
# # ecoregions <- st_read(paste0(phenotype.wd, "Shapefiles/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp"))
# # plot(st_geometry(ecoregions))
# # 
# # gbif_data <- get_gbif("Juglans cinerea")
# # gbif_sf <- gbif_data %>%
# #   dplyr::filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) %>%
# #   st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
# # 
# # unique(ecoregions$US_L3NAME)
# # 
# # eastern_eco <- ecoregions %>%
# #   filter(st_coordinates(st_centroid(geometry))[,1] > -100)
# # 
# # # Ensure coordinate systems match
# # gbif_sf <- st_transform(gbif_sf, st_crs(eastern_eco))
# # 
# # gbif_native_sf <- gbif_sf[st_within(gbif_sf, eastern_eco, sparse = FALSE), ]
# # 
# # ggplot() +
# #   geom_sf(data = ecoregions, fill = "grey", color = "darkgrey", size = 0.2) +
# #   geom_sf(data = gbif_native_sf, color = "#e6550d", size = 2, alpha = 0.7) +
# #   coord_sf(xlim = c(-100, -65), ylim = c(25, 50)) +
# #   theme_minimal() +
# #   labs(title = "Native Range GBIF Points for Juglans cinerea",
# #        subtitle = "Over EPA Level III Ecoregions")
# 
