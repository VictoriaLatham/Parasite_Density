# Load packages
library(tidyverse)

setwd("~/Documents/bstats/Parasite_Density")

# Load data 
lab_file_2018 <- read_excel("data/Lab_file_2018.xlsx")
DHS_2018 <- read_dta("data/NGPR7AFL.DTA")

names(DHS_2018)
DHS_2018 <- rename(DHS_2018, barcode = hml32)
names(Lab_file_2018)
lab_file_2018 <- rename(lab_file_2018, barcode = BARCODE, state = STATE)
str(Lab_file_2018)

# Data cleaning - changing observations with stage and species data superimposed
lab_file_2018[which(lab_file_2018$Stage == "PF"), "Species"] <- lab_file_2018[which(lab_file_2018$Stage == "PF"), "Stage"]
lab_file_2018[which(lab_file_2018$Stage == "PF,PO"), "Species"] <- lab_file_2018[which(lab_file_2018$Stage == "PF,PO"), "Stage"]
lab_file_2018[which(lab_file_2018$Stage == "PM"), "Species"] <- lab_file_2018[which(lab_file_2018$Stage == "PM"), "Stage"]
lab_file_2018[which(lab_file_2018$Stage == "PO"), "Species"] <- lab_file_2018[which(lab_file_2018$Stage == "PO"), "Stage"]
lab_file_2018[which(lab_file_2018$`barcode` == "A2E7N"), ]
lab_file_2018[lab_file_2018$barcode == "Y2C8L", ]$Species <- "PF,PM"
lab_file_2018[lab_file_2018$barcode == "Z6Z4K", ]$Species <- "PF,PM"



