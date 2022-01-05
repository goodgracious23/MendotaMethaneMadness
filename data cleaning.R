# GHG DATA CLEANING - Mendota Methane Madness
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

#Read in all of the data sets
ghg = read.csv("mendota_ch4_ghg_2021.csv")
pigments = read.csv("mendota_ch4_pigments_2021.csv")
profiles = read.csv("mendota_ch4_profiles_2021.csv")
sediment = read.csv("mendota_ch4_sediment_2021.csv")
ss = read.csv("mendota_ch4_suspendedsolids_2021.csv")

#Subset the GHG data for further processing
atmo = ghg %>%
  filter(type == "Atmosphere")

water = ghg %>%
  filter(type =="Water")

core = ghg %>%
  filter(type == "Core")

incubation = ghg %>%
  filter(type == "Incubation")