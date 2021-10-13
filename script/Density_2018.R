# Load packages
library(tidyverse)

setwd("~/Documents/bstats/Parasite_Density")

# Load data 
lab_file_2018 <- read_excel("data/Lab_file_2018.xlsx")
DHS_2018 <- read_dta("data/NGPR7AFL.DTA")

names(DHS_2018)
DHS_2018 <- rename(DHS_2018, barcode = hml34)
names(lab_file_2018)
lab_file_2018 <- rename(lab_file_2018, barcode = BARCODE, state = STATE)
str(lab_file_2018)

# Data cleaning - changing observations with stage and species data superimposed
unique(lab_file_2018$Species)
lab_file_2018[which(lab_file_2018$Stage == "PF"), "Species"] <- lab_file_2018[which(lab_file_2018$Stage == "PF"), "Stage"]
lab_file_2018[which(lab_file_2018$Stage == "PF,PO"), "Species"] <- lab_file_2018[which(lab_file_2018$Stage == "PF,PO"), "Stage"]
lab_file_2018[which(lab_file_2018$Stage == "PF,PM"), "Species"] <- lab_file_2018[which(lab_file_2018$Stage == "PF,PM"), "Stage"]
lab_file_2018[which(lab_file_2018$Stage == "PM"), "Species"] <- lab_file_2018[which(lab_file_2018$Stage == "PM"), "Stage"]
lab_file_2018[which(lab_file_2018$Stage == "PO"), "Species"] <- lab_file_2018[which(lab_file_2018$Stage == "PO"), "Stage"]
lab_file_2018[which(lab_file_2018$`barcode` == "A2E7N"), ]
lab_file_2018[which(lab_file_2018$`barcode` == "A2M2Q"), ]
lab_file_2018[lab_file_2018$barcode == "Y2C8L", ]$Species <- "PF,PM"
lab_file_2018[lab_file_2018$barcode == "Z6Z4K", ]$Species <- "PF,PM"
lab_file_2018[lab_file_2018$barcode == "T9A4H", ]$Species <- "PF,PM"
lab_file_2018[lab_file_2018$barcode == "U5R6X", ]$Species <- "PF,PM"
lab_file_2018[lab_file_2018$barcode == "Q7J5M", ]$Species <- "PF,PO"
lab_file_2018$Species <- na_if(lab_file_2018$Species, "N/A")
lab_file_2018[which(lab_file_2018$`barcode` == "W7Q5Z"), ] # Need to remove T

# Altering the class of a variable 
DHS_2018$barcode <- as.character(DHS_2018$barcode)
# Merging the data sets
table_2018 <- left_join(lab_file_2018, DHS_2018, by = c("barcode" = "barcode"))
table_2018 <- dplyr::select(table_2018, barcode = barcode, state = state, zone_lab = Zone, 
                                    stage = Stage, species = Species, final_asexual = final_asexual, 
                                    final_sexual = final_sexual, age_bands = hv105, sex = hv104, geo_zone = hv024, 
                                    shstate = shstate, smear_result = hml32)
# Checking for duplicate barcodes 
table_2018[which(table_2018$`barcode` == "A4Q1V"), ]
table_2018[which(table_2018$`barcode` == "J0F2Q"), ]
table_2018[which(table_2018$`barcode` == "U1F9K"), ]
table_2018[which(table_2018$`barcode` == "V2N6R"), ]
table_2018[which(table_2018$`barcode` == "R5A2Y"), ]
# Removing duplicates barcodes
df_2018 <- table_2018 %>% arrange(barcode)
df_2018 <- df_2018[-c(165, 3443, 7668, 8084, 6654), ]
# Removing observations that are not recorded as positive or negative in hml32 (final result of blood smear in DHS file)
df_2018 <- df_2018 %>% arrange(smear_result)
df_2018 <- df_2018[-c(7822:9956), ]
# Altering observations that do not match the DHS file (final_asexual result is 2 when DHS states negative for malaria)
df_2018[which(df_2018$`final_asexual` == 2), ]
df_2018[which(df_2018$`final_sexual` == 2), ]
df_2018[df_2018$barcode == "F3Y2J", ]$final_asexual <- 0
df_2018[df_2018$barcode == "F7V8Q", ]$final_asexual <- 0
df_2018[df_2018$barcode == "F9I3Z", ]$final_asexual <- 0
df_2018[df_2018$barcode == "J4X3O", ]$final_asexual <- 0
df_2018[df_2018$barcode == "L2W0K", ]$final_asexual <- 0
df_2018[df_2018$barcode == "N1V2M", ]$final_asexual <- 0
df_2018[df_2018$barcode == "P0O3H", ]$final_asexual <- 0
df_2018[df_2018$barcode == "S0K9M", ]$final_asexual <- 0
df_2018[df_2018$barcode == "U0R4Q", ]$final_asexual <- 0
df_2018[df_2018$barcode == "U6B2E", ]$final_asexual <- 0
df_2018[df_2018$barcode == "F3Y2J", ]$final_sexual <- 0
df_2018[df_2018$barcode == "F7V8Q", ]$final_sexual <- 0
df_2018[df_2018$barcode == "F9I3Z", ]$final_sexual <- 0
df_2018[df_2018$barcode == "J4X3O", ]$final_sexual <- 0
df_2018[df_2018$barcode == "L2W0K", ]$final_sexual <- 0
df_2018[df_2018$barcode == "N1V2M", ]$final_sexual <- 0
df_2018[df_2018$barcode == "P0O3H", ]$final_sexual <- 0
df_2018[df_2018$barcode == "S0K9M", ]$final_sexual <- 0
df_2018[df_2018$barcode == "U0R4Q", ]$final_sexual <- 0
df_2018[df_2018$barcode == "U6B2E", ]$final_sexual <- 0
