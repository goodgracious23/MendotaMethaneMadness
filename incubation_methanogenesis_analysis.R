#METHANOGENESIS ANALYSIS - Mendota Methane Madness
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(ggridges)) install.packages('ggridges')
library(ggridges)

#============================
#Calculate Sediment Physical Characteristics
sed_phys = read.csv("mendota_ch4_sediment_2021.csv")

#Bulk Density (g/cm3)
sed_phys$bulk_density = (sed_phys$dry_mass - sed_phys$vial_mass) / sed_phys$sed_vol

#Porosity (g/mL)
sed_phys$porosity = ((sed_phys$wet_mass - sed_phys$vial_mass) - 
                       (sed_phys$dry_mass - sed_phys$vial_mass)) / sed_phys$sed_vol

#Percent organic matter
sed_phys$orgmatter = ((sed_phys$rewet_dry_mass - sed_phys$transfertin_mass) - 
                        (sed_phys$ashed_mass - sed_phys$transfertin_mass))*100

#Didn't take wet_mass for week 1 of sampling. Substituted in median porosity for same site and sediment depth from the whole season for calculations moving forward
#Removed the organic matter values that were less than 0% after checking original recorded measurements
por_replace = sed_phys %>%
  group_by(site, sedDepth) %>%
  summarize(med_porosity = median(porosity, na.rm = TRUE)) %>%
  ungroup()

bd_replace = sed_phys %>%
  group_by(site, sedDepth) %>%
  summarize(med_bd = median(bulk_density, na.rm = TRUE)) %>%
  ungroup()

sed_phys1 = left_join(sed_phys, por_replace, by = c("site", "sedDepth")) %>%
  mutate(porosity = case_when(is.na(.$porosity) ~ med_porosity, TRUE ~ porosity)) %>%
  select(-med_porosity) %>%
  mutate(orgmatter = case_when(.$orgmatter < 0 ~ -9999, TRUE ~ orgmatter)) %>%
  na_if(., -9999)

sed_phys2 = left_join(sed_phys1, bd_replace, by = c("site", "sedDepth")) %>%
  mutate(bulk_density = case_when(is.na(.$bulk_density) ~ med_bd, TRUE ~ bulk_density)) %>%
  select(-med_bd)
  


#================================
#Subset incubation data and munge
ghg = read.csv("mendota_ch4_ghg_2021.csv")
inc_raw = ghg %>%
  filter(type == "Incubation") %>%
  filter(!(site == "Y3" | site == "G3" | site == "PB3")) %>%
  select(-analysis_date, -export, -GC_batch_id, -GC_sample_id, -sedDepth, - date,
         -CO2_FID, -CO2_TCD, -CH4_TCD, -N2O_ECD) %>%
  pivot_wider(id_cols = c(doy, site), 
              names_from = c(time, rep), 
              values_from = CH4_FID)

#Subset the physical sediment data to the surface data
sed_surface = sed_phys2 %>%
  filter(sedDepth == 1) %>%
  select(-vial_mass, -sed_vol:-notes)

inc_sed = left_join(inc_raw, sed_surface, by = c("doy", "site")) %>%
  rename(t1_ch4_A = t1_A,
         t1_ch4_B = t1_B,
         t2_ch4_A = t2_A,
         t2_ch4_B = t2_B)

#Read in the incubation times, volumes, and temperatures
inc_time = read.csv("mendota_ch4_incubation_times_2021.csv") %>%
  pivot_wider(id_cols = c(doy, site), 
              names_from = c(
              rep), 
              values_from = c(t1_hours:inc_temp)) %>%
  select(-vol_sed_B, -vol_water_B, -inc_temp_B) %>%
  rename(vol_sed = vol_sed_A,
         vol_water = vol_water_A,
         inc_temp = inc_temp_A)

incubations = left_join(inc_sed, inc_time, by = c("doy", "site"))
  
#===============================================
# METHANOGENESIS POTENTIAL

KH_ch4 = 0.00142 # KH = concentration / partial pressure
KH_td_ch4 = 1600 # Temperature-dependence constants for KH (unitless)

#_Ideal Gas Law
### Used to calculate concentration of a gas based on its partial pressure
# [gas] = (P/RT)*(10^6 umol/mol) ; units = umol/L

ideal_gas_law = function(pp, temp) {
  conc = (pp / (0.0821 * (273.15 + temp))) * 10^6
  return(conc)
}

methano_samples = incubations %>%
  # calculate partial pressure of gases (units = atm) in the headspace sample from the incubation container
  mutate(pch4_t1_A = t1_ch4_A / 10^6 * 0.99,
         pch4_t1_B = t1_ch4_B / 10^6 * 0.99,
         pch4_t2_A = t2_ch4_A / 10^6 * 0.99,
         pch4_t2_B = t2_ch4_B / 10^6 * 0.99) %>%
  # ideal gas law to get final concentration (units = uM) in the headspace sample from the incubation container
  mutate(ch4_t1_A_head = ideal_gas_law(pch4_t1_A, inc_temp),
         ch4_t1_B_head = ideal_gas_law(pch4_t1_B, inc_temp),
         ch4_t2_A_head = ideal_gas_law(pch4_t2_A, inc_temp),
         ch4_t2_B_head = ideal_gas_law(pch4_t2_B, inc_temp))


#_AQUEOUS SAMPLE 
#  concentration of gases dissolved in water at end of incubation,
#  in equilibrium with bottle headspace

methano_samples = methano_samples %>%
  # temperature-corrected Henry's Law constants (tKH)
  mutate(tKH_ch4 = KH_ch4 * exp(KH_td_ch4 * ((1 / (inc_temp + 273.15)) - (1 / 298.15)))) %>%
  # concentration dissolved in water in the incubation container (units = uM)
  mutate(ch4_aq_t1_A = tKH_ch4 * pch4_t1_A * 10^6,
         ch4_aq_t1_B = tKH_ch4 * pch4_t1_B * 10^6,
         ch4_aq_t2_A = tKH_ch4 * pch4_t2_A * 10^6,
         ch4_aq_t2_B = tKH_ch4 * pch4_t2_B * 10^6)

methano_samples = methano_samples %>%
  # calculate total aqueous volume in bottle using porosity data
  # aqueous volume = water sample + aqueous portion of sediment sample (units = mL)
  mutate(vol_aq = vol_water + (vol_sed * porosity)) %>%
  # calculate total sample mass in each assay bottle using sediment bulk density
  # vol_aq = water mass (since 1 cm^3 = 1 g)
  # need to convert volumes back to ml (i.e. cm^3) to calculate mass
  mutate(mass_sed = vol_sed * bulk_density,
         mass_slurry = vol_aq + mass_sed) %>%
  #Calculate the volume of headspace in the incubation container (units = mL)
  mutate(vol_head = 125 - (vol_sed + vol_water))

##__Methane Production Rate (units = umol g-1 h-1)
methano_samples = methano_samples %>%
  # total amount of gases in bottle at end of incubation (headspace + dissolved) (units = umol)
  # converted vol_aq and vol_head to liters from mL (see above units)
  mutate(ch4_tot_umol_t1_A = (ch4_aq_t1_A * vol_aq/1000) + (ch4_t1_A_head * vol_head/1000),
         ch4_tot_umol_t1_B = (ch4_aq_t1_B * vol_aq/1000) + (ch4_t1_B_head * vol_head/1000),
         ch4_tot_umol_t2_A = (ch4_aq_t2_A * vol_aq/1000) + (ch4_t2_A_head * vol_head/1000),
         ch4_tot_umol_t2_B = (ch4_aq_t2_B * vol_aq/1000) + (ch4_t2_B_head * vol_head/1000)) %>%
  # hourly rate of production per gram of slurry mass (units = umol g-1 h-1)
  mutate(ch4_rate_t1_A = ch4_tot_umol_t1_A / mass_slurry / t1_hours_A,
         ch4_rate_t1_B = ch4_tot_umol_t1_B / mass_slurry / t1_hours_B,
         ch4_rate_t2_A = ch4_tot_umol_t2_A / mass_slurry / t2_hours_A,
         ch4_rate_t2_B = ch4_tot_umol_t2_B / mass_slurry / t2_hours_B)

#Summarize the values
sum_methanogenesis = methano_samples %>%
  group_by(site, doy) %>%
  summarize(t1_rate = mean(c(ch4_rate_t1_A, ch4_rate_t1_B), na.rm = TRUE),
            t2_rate = mean(c(ch4_rate_t2_A, ch4_rate_t2_B), na.rm = TRUE),
            mean_rate = mean(c(ch4_rate_t1_A, ch4_rate_t1_B,
                               ch4_rate_t2_A, ch4_rate_t2_B), na.rm = TRUE)) %>%
  ungroup()
sum_methanogenesis = as.data.frame(sum_methanogenesis)

# write.csv(sum_methanogenesis, file = "Methanogenesis_Summary.csv")
