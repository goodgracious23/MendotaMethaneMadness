# GHG DATA CLEANING - Mendota Methane Madness
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

#Read in all of the data sets
ghg = read.csv("mendota_ch4_ghg_2021.csv")
profiles = read.csv("mendota_ch4_profiles_2021.csv")
sediment = read.csv("mendota_ch4_sediment_2021.csv")

#Subset the GHG data for further processing
atmo = ghg %>%
  filter(type == "Atmosphere")

core_ghg = ghg %>%
  filter(type == "Core")
core = left_join(core_ghg, sediment, by = c('site', 'doy', 'sedDepth'))
#Problems with joining these two data sets...

incubation = ghg %>%
  filter(type == "Incubation")


