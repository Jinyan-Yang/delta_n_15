# install neonUtilities - can skip if already installed
#install.packages("neonUtilities")
# load neonUtilities
library(tidyverse)
library(neonUtilities)

leaf_iso <- loadByProduct(dpID="DP1.10026.001")
# names(leaf_iso)
#data1 <- leaf_iso$cfc_chemistrySubsampling
spatial_data <- leaf_iso$cfc_fieldData %>%
  dplyr::select(sampleID, sampleType, 
                decimalLongitude, decimalLatitude,
                scientificName, plantStatus)

raw_iso <- leaf_iso$cfc_carbonNitrogen %>%
  left_join(spatial_data) %>%
  dplyr::select(siteID:sampleID,sampleType,
                d15N, nitrogenPercent,
                decimalLongitude, decimalLatitude,
                scientificName, plantStatus)

table(raw_iso$sampleType)
table(raw_iso$siteID)
summary(raw_iso)

write.csv(raw_iso, "data/neon_15N_data.csv")

neon_15N <- read.csv("cache/neon_15N_data.csv")
