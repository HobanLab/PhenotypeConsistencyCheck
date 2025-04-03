# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % 02/10/2025 Consistency Check between Phenotype_i and Phenotype_f % 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This script compares the measurements of various traits taken two times for 30 herbarium specimens. The traits measured were categorical and continuous data types. For the traits with categorical measurements, we calculated the percentage of measurements taken twice that remained the same. For traits with continuous measurements we calculated how much change there was between both the first and second measurement as a positive or negative percent change. Afterwards, for each categorical trait, we calculated the mean percent of traits that remained the same. For continuous traits, we also calculated the mean percent change for each continuous trait.
# Set work directory and create objects ----
phenotype.wd <- "~/GitHub/PhenotypeConsistencyCheck/"
setwd(phenotype.wd)
# convert characters which R does not recognize to NA ----
# this object is a data frame that takes in the spreadsheet that measured traits for 30 herbarium specimens the first time.
initial_phenotype_measurement_df <- read.csv(paste0(phenotype.wd,"Consistency_Check_Sheets/V2_Protocol/2025_03_05_Updated_Spreadsheet_Blindcheck_1.csv"), na.strings = c("Unsure/in between", "Evenly split", "Trait not present", "Trait not present ", "Trait not measurable/ obscured or image quality issue", "Trait to early to measure", "Trait too early to measure", "Trait not measurable/obscured or image quality issue"))
# this object is a data frame that takes in the spreadsheet that measured traits for 30 herbarium specimens the second time.
final_phenotype_measurement_df <- read.csv(paste0(phenotype.wd, "Consistency_Check_Sheets/V2_Protocol/2025_03_20_Updated_Spreadsheet_Blindcheck_2.csv"), na.strings = c("Unsure/in between", "Evenly split", "Trait not present", "Trait not present ", "Trait not measurable/ obscured or image quality issue", "Trait to early to measure", "Trait too early to measure", "Trait not measurable/obscured or image quality issue"))
# remove empty rows from both data sets ----
initial_phenotype_measurement_df <- initial_phenotype_measurement_df[-c(16:999),]
final_phenotype_measurement_df <- final_phenotype_measurement_df[-c(16:999),]

initial_phenotype_measurement_df$Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue <- as.numeric(factor(initial_phenotype_measurement_df$Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue, levels = c("Slender", "Stout"), labels = c(1,2)))-1
initial_phenotype_measurement_df$Color.of.twig.Dark.olive.green.to.red.brown.Tan.to.brown.Tan.or.light.green.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue <- as.numeric(factor(initial_phenotype_measurement_df$Color.of.twig.Dark.olive.green.to.red.brown.Tan.to.brown.Tan.or.light.green.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue, levels = c("Dark olive gree to red brown", "Tan to brown", "Tan or light green"), labels = c(1,2,3)))-1
initial_phenotype_measurement_df$Hair.Texture.of.twig.Some..or.no..hairs.Patchy.hairs.Abundant.hairs.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure <- as.numeric(factor(initial_phenotype_measurement_df$Hair.Texture.of.twig.Some..or.no..hairs.Patchy.hairs.Abundant.hairs.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue.Trait.to.early.to.measure, levels = c("Some (or no) hairs", "Patchy hairs", "Abundant hairs"), labels = c(1,2,3)))-1

# ordinal columns
ordinal_columns <- c(6:11,13,15,21,37)
# # discrete columns 
# discrete_columns <- c(38:42)
# # continuous columns
# continuous_columns <- c(16:20,22:36,43:47)

initial_phenotype_measurement_df$total_score <- rowSums(initial_phenotype_measurement_df[,ordinal_columns], na.rm = TRUE)

c(ordinal_columns)
# initial_phenotype_measurement_df$Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue

# initial_phenotype_measurement_df$Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue
# 
# test <- c("slender" = 0, "stout" = 1)
# initial_phenotype_measurement_df$Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue <- test[initial_phenotype_measurement_df$Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue]

# initial_phenotype_measurement_df$Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue <- test[initial_phenotype_measurement_df$Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue]
# 
# replace(temp, c("Slender","Slender","Slender","Slender","Slender","Slender"), c(2,10,11,13,14,15))
# 
# initial_phenotype_measurement_df$Color.of.twig.Dark.olive.green.to.red.brown.Tan.to.brown.Tan.or.light.green.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue[1:10]
# as.numeric(initial_phenotype_measurement_df$Color.of.twig.Dark.olive.green.to.red.brown.Tan.to.brown.Tan.or.light.green.Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue)
# initial_phenotype_measurement_df$Thickness.of.twig.Stout..thick..hulky..Slender..thin..Unsure.in.between.Trait.not.present.Trait.not.measurable..obscured.or.image.quality.issue <- ifelse() 1
