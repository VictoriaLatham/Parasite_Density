###### --------- Parasite density --------- ######
  ###### --------- Vicki Latham --------- ######
    ###### --------- 6/10/2021 --------- ######




####  -------------------   STAGE 1 : LOADING AND CLEANING  -------------------    ####

# Load packages
library(tidyverse)
library(ggplot2)
library(survival)
library(tableone)
# install.packages("psych")  
library("psych")
# install.packages("ggplot2")
library(ggplot2)
# install.packages("rcompanion")
library("rcompanion")
# install.packages("DescTools")  
library("DescTools")
library("Publish")
# install.packages("ggpubr")
# install.packages("haven")
library("ggpubr")
library(pacman)
p_load(readxl, readr, epitrix, rio, stringr, dplyr, fastLink, janitor, ggplot2, reshape2, knitr, officer, gdtools, tibble, tidyr, tidyverse, lubridate, forcats, epitools, rlist, flextable, naniar)
p_loaded()

setwd("~/Documents/bstats/Parasite_Density")

# Load data 
lab_file_2018 <- read_excel("data/Lab_file_2018.xlsx")
DHS_2018 <- read_dta("data/NGPR7AFL.DTA")

# Data cleaning 
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
# Change two observations with species recorded as T
df_2018$species <- na_if(df_2018$species, "T")

# Removing observations that are positive for asexual parasite or sexual but the DHS file is negative AND observations that are positive in DHS file but negative in lab file for both asexual/sexual
df_2018 <- df_2018 %>% filter(
  !((smear_result == 0 & final_asexual > 0 | smear_result == 0 & final_sexual > 0) |
    (smear_result == 0 & final_asexual > 0 & !smear_result == 0 & final_sexual > 0)),
  !(smear_result == 1 & (final_asexual == 0 & final_sexual == 0))
)

# Checking class of variables and changing class
str(df_2018)
df_2018 <- df_2018 %>% mutate(
  age_bands = as.numeric(age_bands)
) 




####  -------------------   STAGE 2 : SUBSETTING AND CREATING VARIABLES  -------------------    ####

# Selecting only positive and only negative individuals
only_positive <- df_2018 %>% filter(
  smear_result == 1
)
only_negative <- df_2018 %>% filter(
  smear_result == 0
)

# Creating asexual_nozero, sexual_nozero, asexual_grp, asexual_grp_nozero variables
df_2018 <- df_2018 %>% mutate(
  asexual_nozero = na_if(df_2018$final_asexual, 0),
  sexual_nozero = na_if(df_2018$final_sexual, 0),
  asexual_grp = case_when(final_asexual == 0 ~ 0,
                          final_asexual >= 1 & final_asexual < 999 ~ 1,
                          final_asexual >= 1000 & final_asexual < 9999 ~ 2,
                          final_asexual >= 10000 & final_asexual < 1000000000 ~ 3),
  asexual_grp_nozero = case_when(asexual_nozero >= 1 & asexual_nozero < 999 ~ 1,
                                 asexual_nozero >= 1000 & asexual_nozero < 9999 ~ 2,
                                 asexual_nozero >= 10000 & asexual_nozero < 1000000000 ~ 3),
  asexual_prev = case_when(final_asexual == 0 ~ 0,
                           final_asexual >= 1 ~ 1),
  sexual_prev = case_when(final_sexual == 0 ~ 0,
                          final_sexual >= 1 ~ 1)
)


####  -------------------   STAGE 3 : BASIC ANALYSIS  -------------------    ####


# Baseline characteristics 
myVars <- c("geo_zone", "age_bands", "sex", "species", "sexual_prev", "asexual_prev", "final_asexual" , "asexual_grp_nozero", "asexual_grp", "asexual_nozero", "sexual_nozero")
catVars <- c("geo_zone", "age_bands", "sex", "species", "sexual_prev", "asexual_prev", "asexual_grp_nozero", "asexual_grp")
tab2 <- CreateTableOne(vars = myVars, data = df_2018, factorVars = catVars)
tab2
print(tab2, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
summary(tab2)
df_2018 %>% tabyl(sex)
summary(df_2018$asexual_nozero)

# Overall geometric mean 
ci.mean(df_2018$asexual_nozero, statistic="geometric", na.rm=T)

# Geometric mean for each sex
sex_gmean_2018 <- ci.mean (asexual_nozero~sex, data=df_2018, statistic = "geometric", na.rm=TRUE)
print(sex_gmean_2018)
# Mann-Whitney test
wilcox.test(asexual_nozero ~ sex, data = df_2018)

# Percentage of children of different ages with each level of parasite density
df_2018 %>% tabyl(age_bands, asexual_grp) %>%
  adorn_totals() %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns()
df_2018 %>% tabyl(age_bands, asexual_grp_nozero, show_missing_levels = F, show_na = FALSE) %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns()

# Geometric mean for each geo_zone
zone_gmean_2018 <- ci.mean (asexual_nozero~geo_zone, data=df_2018, statistic = "geometric", na.rm=TRUE)
print(zone_gmean_2018)
# Plotting the above results
zone_data_2018 <- as.data.frame(zone_gmean_2018)
zone_data_2018 <- zone_data_2018 %>% mutate(
  geo_zone = case_when(geo_zone == 1 ~ "NC",
                       geo_zone == 2 ~ "NE",
                       geo_zone == 3 ~ "NW",
                       geo_zone == 4 ~ "SE",
                       geo_zone == 5 ~ "SS",
                       geo_zone == 6 ~ "SW"))
(zone <- ggplot(zone_data_2018) +
  geom_bar( aes(x=geo_zone, y=geomean), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=geo_zone, ymin=lower, ymax=upper), width=0.4, colour="orange", alpha=0.9, size=1.3)  + 
  labs( x = "Geopolitical zone", y = "Geometric mean asexual parasite density (parasites/μL)") +
  theme_classic())
# Kruskal-Wallis test
kruskal.test(asexual_nozero ~ geo_zone, data = df_2018)
# Pairwise comparisons with adjustment for multiple comparisons
pairwise.wilcox.test(df_2018$asexual_nozero, df_2018$geo_zone, p.adjust.method = "BH")

# Geometric mean for each age group
age_gmean_2018 <- ci.mean (asexual_nozero~age_bands, 
                           data=df_2018, statistic = "geometric", na.rm=TRUE)
# Plotting this data
age_data_2018 <- as.data.frame(age_gmean_2018)
(age <- ggplot(age_data_2018) +
  geom_bar( aes(x=age_bands, y=geomean), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=age_bands, ymin=lower, ymax=upper), width=0.4, colour="orange", alpha=0.9, size=1.3)  + 
  labs( x = "Age (years)", y = "Geometric mean asexual parasite density (parasites/μL)") +
  theme_classic())
# Kruskal-Wallis test
kruskal.test(asexual_nozero ~ age_bands, data = df_2018)
# Pairwise comparisons using an adjustment for multiple comparisons
pairwise.wilcox.test(df_2018$asexual_nozero, df_2018$age_bands, p.adjust.method = "BH")

# Plot age and geo_zone graphs together
ggarrange(age, zone,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


####  -------------------   STAGE 4 : MAPS  -------------------    ####

maps <- read.csv("state_density_2018.csv")
bc <- c(0, 1000, 2000, 3000, 4000, 5000, 6000)
map_ng(data = maps, x = Asexual_density, breaks = bc, col = 'YlOrRd',
       categories = c("0-1000 parasites/μL", "1001-2000 parasites/μL", 
                      "2001-3000 parasites/μL", "3001-4000 parasites/μL", 
                      "4001-5000 parasites/μL", "5001-6000 parasites/μL"))


####  -------------------   STAGE 5 : CORRELATION ANALYSIS  -------------------    ####

# Loading data 
prev_density_2018 <- read.csv("state_density_prev_2018.csv")
prev_density_2018 <- prev_density_2018 %>% mutate(
  lower_95_CI_prev = (lower_95_CI_prev*100),
  upper_95_CI_prev = (upper_95_CI_prev*100)
)
# Plotting parasite density and prevalence with error bars
ggplot(prev_density_2018) +
  geom_point(aes(x=Asexual_prev, y=Asexual_density)) +
  geom_pointrange(aes(x=Asexual_prev, xmin=lower_95_CI_prev, 
                      xmax=upper_95_CI_prev, y=Asexual_density, 
                      ymin=lower_95_CI_density, ymax=upper_95_CI_density)) +
  geom_errorbarh(aes(x=Asexual_prev, xmin=lower_95_CI_prev, 
                     xmax=upper_95_CI_prev, , y=Asexual_density, 
                     ymin=lower_95_CI_density, ymax=upper_95_CI_density)) +
  labs( y = "Geometric mean asexual parasite density (parasites/μL)", 
        x = "Malaria asexual parasite prevalence (%)") +
  theme(axis.title = element_text(face="bold", size=13))




#--- Get summary statistics of our choice grouped by region
df_2018 %>% filter(as.logical(asexual_prev)) %>% 
      group_by(shstate) %>% 
      CI(df_2018$asexual_prev, ci=0.95)

ll <- function(x) t.test(x)$conf.int[[1]] # Lower 95% CI of mean
ul <- function(x) t.test(x)$conf.int[[2]]
df_2018 %>% select(shstate as) %>% tbl_summary(
  by = shstate,
  statistic = 
)


####  -------------------   STAGE 6 : SEXUAL ANALYSIS  -------------------    ####


# Sexual prevalence in age groups
df_2018 %>% tabyl(age_bands, sexual_prev) %>%
  adorn_totals() %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns()
# Sexual prevalence in males/females
df_2018 %>% tabyl(sex, sexual_prev) %>%
  adorn_totals() %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns()
# Chi-squared test
chisq.test(df_2018$sex, df_2018$sexual_prev)
# Sexual prevalence in geo_zones
df_2018 %>% tabyl(geo_zone, sexual_prev) %>%
  adorn_totals() %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns()

# Sexual density overview
summary(df_2018$sexual_nozero)
# Overall geometric mean SEXUAL
ci.mean(df_2018$sexual_nozero, statistic="geometric", na.rm=T)


# Geometric mean SEXUAL for each sex
sex_gmean_2018 <- ci.mean (sexual_nozero~sex, data=df_2018, statistic = "geometric", na.rm=TRUE)
print(sex_gmean_2018)
# Mann-Whitney test
wilcox.test(sexual_nozero ~ sex, data = df_2018)

# Geometric mean SEXUAL for each geo_zone
zone_gmean_2018 <- ci.mean (sexual_nozero~geo_zone, data=df_2018, statistic = "geometric", na.rm=TRUE)
print(zone_gmean_2018)
# Kruskal-Wallis test
kruskal.test(sexual_nozero ~ geo_zone, data = df_2018)
# Pairwise comparisons with adjustment for multiple comparisons
pairwise.wilcox.test(df_2018$sexual_nozero, df_2018$geo_zone, p.adjust.method = "BH")

# Geometric mean SEXUAL for each age group
age_gmean_2018 <- ci.mean (sexual_nozero ~ age_bands, 
                           data=df_2018, statistic = "geometric", na.rm=TRUE)
print(age_gmean_2018)
# Kruskal-Wallis test
kruskal.test(sexual_nozero ~ age_bands, data = df_2018)
# Pairwise comparisons using an adjustment for multiple comparisons
pairwise.wilcox.test(df_2018$sexual_nozero, df_2018$age_bands, p.adjust.method = "BH")


####  -------------------   STAGE 6 : SPECIES ANALYSIS  -------------------    ####

df_2018 %>% tabyl(species, asexual_prev) %>%
  adorn_totals() %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns()
df_2018 %>% tabyl(species, sexual_prev) %>%
  adorn_totals() %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns()
df_2018 %>% tabyl(species, smear_result) %>%
  adorn_totals() %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns()
df_2018 %>% tabyl(species, sex) %>%
  adorn_totals() %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns()

# Geometric mean for each species
species_gmean_2018 <- ci.mean (asexual_nozero ~ species, 
                           data=df_2018, statistic = "geometric", na.rm=TRUE)
print(species_gmean_2018)
# Kruskal-Wallis test
kruskal.test(asexual_nozero ~ species, data = df_2018)



sex_prev_state <- df_2018 %>% tabyl(shstate, sexual_prev) %>%
  adorn_totals() %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1)
asex_prev_state <- df_2018 %>% tabyl(shstate, asexual_prev) %>%
  adorn_totals() %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1)
asex_prev_state <- as.data.frame(asex_prev_state)
asex_prev_state <- rename(asexul_prev_state, asexual_prev = 2)

state_prev <- state_prev %>% mutate(
  Asexual_prev = (Asexual_prev*100)
)
write.csv(df_2018, file = "df_2018.csv")
write.csv(state_prev, file = "state_prev.csv")



