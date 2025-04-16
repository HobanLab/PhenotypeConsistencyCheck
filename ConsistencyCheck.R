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
