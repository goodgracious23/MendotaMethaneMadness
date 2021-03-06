#Water CH4 Analysis - Mendota Methane Madness
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

#======================
### Constants, Equations, and Functions ----

# Atmospheric Pressure at Mendota
# Madison, WI = 0.99 atm

#======================
# Henry's Law Constants (KH) (units = M/atm) (units = mol / L / atm)
### Used to calculate dissolved gas concentration in a liquid based on equilibrium with a gaseous headspace

# KH = concentration / partial pressure
KH_ch4 = 0.00142
KH_n2o = 0.024

# Temperature-dependence constants for KH (unitless)
KH_td_ch4 = 1600
KH_td_n2o = 2600

#======================
#_Ideal Gas Law
### Used to calculate concentration of a gas based on its partial pressure
# [gas] = (P/RT)*(10^6 umol/mol)
# units = umol/L

ideal_gas_law = function(pp, temp) {
  conc = (pp / (0.0821 * (273.15 + temp))) * 10^6
  return(conc)
}

#============================
# Read in the data and munge
ghg = read.csv("mendota_ch4_ghg_2021.csv")
profiles = read.csv("mendota_ch4_profiles_2021.csv")

#Subset out the water values and take the mean of replicates
water = ghg %>%
  filter(type == "Water") %>%
  #Remove the sites that we only sampled a couple of times
  filter(!(site == "Y3" | site == "G3" | site == "PB3")) %>%
  filter(!(omit=="Y")) %>%
  select(-analysis_date, -export, -GC_batch_id, -GC_sample_id, 
         -date, -time, -sedDepth, -rep) %>%
  group_by(site, doy, waterDepth) %>%
  #calculate mean ppm from duplicate samples
  summarize(ch4 = mean(CH4_FID, na.rm = TRUE),
            n2o = mean(N2O_ECD, na.rm = TRUE)) %>%
  ungroup()

#Subset out only the surface water values
surface_ghg = water %>%
  filter(waterDepth == "Surface")

#Subset the atmosphere sample and take average by DOY
atmo = ghg %>%
  filter(type == "Atmosphere") %>%
  #remove extraneous columns
  select(-analysis_date, -export, -GC_batch_id, -GC_sample_id, 
         -date, -site, -time, -sedDepth, -waterDepth, -rep) %>%
  group_by(doy) %>%
  summarize(atmo_ch4_GC = mean(CH4_FID, na.rm = TRUE),
            atmo_n2o_GC = mean(N2O_ECD, na.rm = TRUE)) %>%
  ungroup()

#Subset the ysi profiles to surface values only
surface_ysi = profiles %>%
  filter(depth == 0)

water_ghg1 = left_join(surface_ghg, surface_ysi, by = c("site", "doy"))
water_ghg = left_join(water_ghg1, atmo, by = c("doy"))

#============================
## Concentration of dissolved gases in equilibrated water sample (units = uM)

syr_conc = water_ghg %>%
  # calculate partial pressures of gases in headspace (units = atmosphere)
  # convert measured syringe values from ppm to atm
  mutate(pch4_head = ch4 / 10^6 * 0.99,
         pn2o_head = n2o / 10^6 * 0.99) %>%
  # temperature-corrected Henry's Law constants (KH) based on pond surface water temperatures
  mutate(tKH_ch4 = 
           KH_ch4 * exp(KH_td_ch4 * ((1 / (temp + 273.15)) - (1 / 298.15))),
         tKH_n2o = 
           KH_n2o * exp(KH_td_n2o * ((1 / (temp + 273.15)) - (1 / 298.15)))) %>%
  # concentration of gases in equilibrated headspace (units = uM)
  mutate(ch4_head = ideal_gas_law(pch4_head, temp),
         n2o_head = ideal_gas_law(pn2o_head, temp)) %>%
  # concentration of dissolved gases in equilibrated aqueous sample (units = uM)
  mutate(ch4_aq = tKH_ch4 * pch4_head * 10^6,
         n2o_aq = tKH_n2o * pn2o_head * 10^6) %>%
  # total amount of gas in syringe (units = umol) 
  # 0.03 L of water and air in syringe for sampling
  mutate(ch4_tot_umol = (ch4_aq * 0.03) + (ch4_head * 0.03),
         n2o_tot_umol = (n2o_aq * 0.03) + (n2o_head * 0.03))

#==============================
# Atmosphere concentration (units = uM)
#  to correct for gas present in headspace prior to equilibration
atm_conc = water_ghg %>%
  mutate(pch4_atmo = atmo_ch4_GC / 10^6 * 0.99,
         pn2o_atmo = atmo_n2o_GC / 10^6 * 0.99) %>%
  # Ideal Gas Law (units = uM)
  mutate(ch4_atmo = ideal_gas_law(pch4_atmo, 25),
         n2o_atmo = ideal_gas_law(pn2o_atmo, 25))

#==============================
#Calculate the concentration of dissolved gas in the sample

lake_conc = syr_conc %>%
  # add atmosphere concentrations to syringe concentrations
  left_join(atm_conc %>% select(doy, site, ch4_atmo, n2o_atmo), 
            by = c("doy", "site")) %>%
  # original gas concentration dissolved in lake water (units = uM)
  mutate(ch4_lake_surface = (ch4_tot_umol - (ch4_atmo * 0.03)) / 0.03,
         n2o_lake_surface = (n2o_tot_umol - (n2o_atmo * 0.03)) / 0.03)
lake_conc = as.data.frame(lake_conc) %>%
  select(-waterDepth, -depth, -depth_code, 
         -ch4, -n2o, -atmo_ch4_GC:-n2o_atmo) %>%
  rename(surface_temp = temp,
         surface_DO_mgL = DO_mgL,
         surface_DO_sat = DO_sat,
         surface_SpCond = SpCond)


#============================
# BOTTOM WATER SAMPLES
#============================
# Read in the data and munge
ghg = read.csv("mendota_ch4_ghg_2021.csv")
profiles = read.csv("mendota_ch4_profiles_2021.csv")

#Subset out the water values and take the mean of replicates
water = ghg %>%
  filter(type == "Water") %>%
  #Remove the sites that we only sampled a couple of times
  filter(!(site == "Y3" | site == "G3" | site == "PB3")) %>%
  filter(!(omit=="Y")) %>%
  select(-analysis_date, -export, -GC_batch_id, -GC_sample_id, 
         -date, -time, -sedDepth, -rep) %>%
  group_by(site, doy, waterDepth) %>%
  #calculate mean ppm from duplicate samples
  summarize(ch4 = mean(CH4_FID, na.rm = TRUE),
            n2o = mean(N2O_ECD, na.rm = TRUE)) %>%
  ungroup()

#Subset out only the surface water values
bottom_ghg = water %>%
  filter(waterDepth == "Bottom")

#Subset the atmosphere sample and take average by DOY
atmo = ghg %>%
  filter(type == "Atmosphere") %>%
  #remove extraneous columns
  select(-analysis_date, -export, -GC_batch_id, -GC_sample_id, 
         -date, -site, -time, -sedDepth, -waterDepth, -rep) %>%
  group_by(doy) %>%
  summarize(atmo_ch4_GC = mean(CH4_FID, na.rm = TRUE),
            atmo_n2o_GC = mean(N2O_ECD, na.rm = TRUE)) %>%
  ungroup()

#Subset the ysi profiles to surface values only
bottom_ysi = profiles %>%
  filter(depth_code == "bottom")

water_ghg1 = left_join(bottom_ghg, bottom_ysi, by = c("site", "doy"))
water_ghg = left_join(water_ghg1, atmo, by = c("doy"))

#============================
## Concentration of dissolved gases in equilibrated water sample (units = uM)

syr_conc = water_ghg %>%
  # calculate partial pressures of gases in headspace (units = atmosphere)
  # convert measured syringe values from ppm to atm
  mutate(pch4_head = ch4 / 10^6 * 0.99,
         pn2o_head = n2o / 10^6 * 0.99) %>%
  # temperature-corrected Henry's Law constants (KH) based on pond surface water temperatures
  mutate(tKH_ch4 = 
           KH_ch4 * exp(KH_td_ch4 * ((1 / (temp + 273.15)) - (1 / 298.15))),
         tKH_n2o = 
           KH_n2o * exp(KH_td_n2o * ((1 / (temp + 273.15)) - (1 / 298.15)))) %>%
  # concentration of gases in equilibrated headspace (units = uM)
  mutate(ch4_head = ideal_gas_law(pch4_head, temp),
         n2o_head = ideal_gas_law(pn2o_head, temp)) %>%
  # concentration of dissolved gases in equilibrated aqueous sample (units = uM)
  mutate(ch4_aq = tKH_ch4 * pch4_head * 10^6,
         n2o_aq = tKH_n2o * pn2o_head * 10^6) %>%
  # total amount of gas in syringe (units = umol) 
  # 0.03 L of water and air in syringe for sampling
  mutate(ch4_tot_umol = (ch4_aq * 0.03) + (ch4_head * 0.03),
         n2o_tot_umol = (n2o_aq * 0.03) + (n2o_head * 0.03))

#==============================
# Atmosphere concentration (units = uM)
#  to correct for gas present in headspace prior to equilibration
atm_conc = water_ghg %>%
  mutate(pch4_atmo = atmo_ch4_GC / 10^6 * 0.99,
         pn2o_atmo = atmo_n2o_GC / 10^6 * 0.99) %>%
  # Ideal Gas Law (units = uM)
  mutate(ch4_atmo = ideal_gas_law(pch4_atmo, 25),
         n2o_atmo = ideal_gas_law(pn2o_atmo, 25))

#==============================
#Calculate the concentration of dissolved gas in the sample

bottom_lake_conc = syr_conc %>%
  # add atmosphere concentrations to syringe concentrations
  left_join(atm_conc %>% select(doy, site, ch4_atmo, n2o_atmo), 
            by = c("doy", "site")) %>%
  # original gas concentration dissolved in lake water (units = uM)
  mutate(ch4_lake_bottom = (ch4_tot_umol - (ch4_atmo * 0.03)) / 0.03,
         n2o_lake_bottom = (n2o_tot_umol - (n2o_atmo * 0.03)) / 0.03)
bottom_lake_conc = as.data.frame(bottom_lake_conc) %>%
  select(-waterDepth, -depth_code, 
         -ch4, -n2o, -atmo_ch4_GC:-n2o_atmo) %>%
  rename(bottom_temp = temp,
         bottom_DO_mgL = DO_mgL,
         bottom_DO_sat = DO_sat,
         bottom_SpCond = SpCond,
         bottom_depth = depth)

lake_ch4 = left_join(lake_conc, bottom_lake_conc, by = c("site", "doy"))
write.csv(lake_ch4, file = "Lake_Concentrations_Profiles.csv")
