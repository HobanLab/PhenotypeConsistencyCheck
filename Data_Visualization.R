#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#% 04/17/2025 Data visualization of GBIF specimens % ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
initial_phenotype_measurement_df_maps <- initial_phenotype_measurement_df[-c(36:999),]
# occ_search is the function that requires parameters to extract individuals from GBIF.
# limit tells the function what the limit of occurrences to return, with a default of 500. We put 5000 as a placeholder because while the amount of individuals in GBIF that are preserved specimens is <2000, we want to make sure we leave enough room to get more individuals.
# hasCoordinate requires T/F so it knows whether or not to include individuals with or without coordintes
# basisofRecords is set to Preserved specimen since we are only interested in herbarium specimens
# mediaType we limited the  search to include specimens that have images only, although this can be changed

butternut_gbif <- occ_search(
  limit = 5000,
  scientificName = "Juglans cinerea",
  hasCoordinate = TRUE,
  basisOfRecord = "PRESERVED_SPECIMEN",
  mediaType = "StillImage"
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

usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Hawaii", "Alaska", "Puerto Rico"))

png(file="~/GitHub/PhenotypeConsistencyCheck/Plots/2025_04_28_subsampleOverlayedwithGBIF_map.png")
ggplot() +
  geom_sf(data = usa, fill = "lightgray", color = "gray") +
  geom_sf(data = butternut_sf, color = "#993404", size = 1, alpha = 0.5) +
  geom_sf(data = my_pts_sf, color = "#0570b0", fill = "#92c5de", size = 2, shape = 21, stroke = 1.2) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +  # zoom to eastern US
  theme_minimal() +
  labs(
    title = "Butternut (Juglans cinerea) Map"
  )
dev.off()
###
# Convert numeric months to factor with month names for plotting ----
###
initial_phenotype_measurement_df <- initial_phenotype_measurement_df[-c(36:999),]
initial_phenotype_measurement_df$Month <- factor(initial_phenotype_measurement_df$Month, levels = 1:12, labels = month.name)

# Barplot visualizations for all subsample of specimens we collected
# By month
month_table <- table(initial_phenotype_measurement_df$Month)
png(file="~/GitHub/PhenotypeConsistencyCheck/Plots/2025_05_02_subsampleMonthbarplot.png")
barplot(month_table,
        las = 2,
        col = "skyblue",
        main = "Number of Individuals per Month",
        ylab = "Count",
        xlab = "Month")
dev.off()

# By decade
initial_phenotype_measurement_df$Decade <- floor(initial_phenotype_measurement_df$Year / 10) * 10
decade_table <- table(initial_phenotype_measurement_df$Decade)
png(file="~/GitHub/PhenotypeConsistencyCheck/Plots/2025_05_02_subsampleDecadebarplot.png")
barplot(decade_table,
        col = "red",
        main = "Number of Individuals by Decade",
        ylab = "Count",
        xlab = "Decade")
dev.off()

# Barplot visualizations but for all GBIF occurrences not just the ones we collected
# By month
butternut_df$month <- factor(butternut_df$month, levels = 1:12, labels = month.name)
gbif_monthTable <- table(butternut_df$month)
png(file="~/GitHub/PhenotypeConsistencyCheck/Plots/2025_05_02_TOTALsampleMonthbarplot.png")
barplot(gbif_monthTable,
        las = 2,
        col = "green",
        main = "All J. cinerea GBIF records: Number of Individuals per Month",
        ylab = "Count",
        xlab = "Month")
dev.off()

# By decade
butternut_df$Decade <- floor(butternut_df$year / 10) * 10
gbif_decade_table <- table(butternut_df$Decade)
png(file="~/GitHub/PhenotypeConsistencyCheck/Plots/2025_05_02_TOTALsampleDecadebarplot.png")
barplot(gbif_decade_table,
        col = "orange",
        main = "All J. cinerea GBIF records: Number of Individuals by Decade",
        ylab = "Count",
        xlab = "Decade")
dev.off()
