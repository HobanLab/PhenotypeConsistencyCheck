# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % 02/10/2025 Consistency Check between Phenotype_i and Phenotype_f % 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
install.packages("tidyverse")
library(dplyr)
# This script compares the measurements of various traits taken two times for 30 herbarium specimens. The traits measured were categorical and continuous data types. For the traits with categorical measurements, we calculated the percentage of measurements taken twice that remained the same. For traits with continuous measurements we calculated how much change there was between both the first and second measurement as a positive or negative percent change. Afterwards, for each categorical trait, we calculated the mean percent of traits that remained the same. For continuous traits, we also calculated the mean percent change for each continuous trait.
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

thickness_column <- c("Slender" = 0, "Stout"= 1)
color_column <- c("Dark olive gree to red brown" = 0, "Tan to brown" = 1, "Tan or light green" = 2)
trichomes_column <- c("Some (or no) hairs" = 0, "Patchy hairs" = 1, "Abundant hairs" = 2)

initial_phenotype_measurement_df$Thickness_of_twig <- thickness_column[initial_phenotype_measurement_df$Thickness_of_twig]
initial_phenotype_measurement_df$Color_of_twig <- color_column[initial_phenotype_measurement_df$Color_of_twig]
initial_phenotype_measurement_df$Hair_texture_of_twig <- trichomes_column[initial_phenotype_measurement_df$Hair_texture_of_twig]

# ordinal columns
ordinal_columns <- c(6:11,13,15,21,37)


# test_vector_store_sums <- vector(length = 100)
# 
# initial_ordinal_phenotypemeasurements <- matrix(nrow = length(colnames(initial_phenotype_measurement_df)[ordinal_columns]), ncol = 1, dimnames = list(colnames(initial_phenotype_measurement_df)[ordinal_columns],
#                                                                                                                            c("Mean score")))
# random_ordinal_columns <- sample(ncol(initial_phenotype_measurement_df[ordinal_columns]), 5, replace = FALSE)
# 
# test_vector_store_sums[6] <- sum(initial_ordinal_phenotypemeasurements[1,random_ordinal_columns], na.rm = TRUE)
# 
# test_vector_store_sums
# 
# initial_ordinal_phenotypemeasurements$MeanScore[1] <- mean(test_vector_store_sums)
# 
# test_vector_store_sums <- vector(length = 100)
# initial_ordinal_phenotypemeasurements <- matrix(nrow = length(colnames(initial_phenotype_measurement_df)[ordinal_columns]), ncol = 1, dimnames = list(colnames(initial_phenotype_measurement_df)[ordinal_columns],
#                                                                                                                                                       c("Mean score")))
# 
# for (i in 1:100) {
#   for (j in 1:10) {
#     random_ordinal_columns <- sample(ncol(initial_phenotype_measurement_df[ordinal_columns]), 5, replace = FALSE)
#     test_vector_store_sums[i] <- sum(initial_ordinal_phenotypemeasurements[j,random_ordinal_columns], na.rm = TRUE)
#     initial_ordinal_phenotypemeasurements$MeanScore[j] <- mean(test_vector_store_sums) 
#   }
# }
# sum(test_vector_store_sums)/6

Specimen <- vector(length = 15)
for (i in 1:15) {
  Specimen[i] <- paste0("Specimen ", i)
  
}

initial_ordinal_phenotypemeasurements <- matrix(nrow =length(1:nrow(initial_phenotype_measurement_df[1])), 
                                                ncol = 1, dimnames = list(Specimen,
                                                                          c("Mean score")))


# Loop over each row
for (j in 1:15) {
  # Reset vector for this row
  test_vector_store_sums <- numeric(100)
  
  for (i in 1:100) {
    # Randomly sample 5 columns from the ordinal columns
    random_ordinal_columns <- sample(ordinal_columns, 5, replace = FALSE)
    
    # Sum values in that row and the sampled columns
    test_vector_store_sums[i] <- sum(initial_phenotype_measurement_df[j, random_ordinal_columns], na.rm = TRUE)
  }
  
  # Store the mean of the 100 sampled sums into the output matrix
  initial_ordinal_phenotypemeasurements[j,] <- mean(test_vector_store_sums)
}


# initial_phenotype_measurement_df$Total_score <- rowSums(initial_phenotype_measurement_df[,random_columns], na.rm = TRUE)
# 
# temp_rows <- sample(1:nrow(initial_phenotype_measurement_df), 5, replace = FALSE)[i]
# 
# 
# for (i in 1:100) {
#   random_columns[i] <- sample(1:ncol(initial_phenotype_measurement_df[,ordinal_columns]), 5, replace = FALSE)
#   # rowSums(initial_phenotype_measurement_df[,ordinal_columns], na.rm = TRUE)
# }
# c(ordinal_columns)
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
