#PORE WATER METHANE ANALYSIS - Mendota Methane Madness
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

#============================
#Calculate Sediment Physical Characteristics
sed_phys = read.csv("mendota_ch4_sediment_2021.csv")

#Bulk Density (g/cm3)
sed_phys$bulk_density = (sed_phys$dry_mass - sed_phys$vial_mass) / sed_phys$sed_vol

#Porosity (g/mL)
sed_phys$porosity = ((sed_phys$wet_mass - sed_phys$vial_mass) - 
                       (sed_phys$dry_mass - sed_phys$vial_mass)) / sed_phys$sed_vol

#Percent organic matter
sed_phys$orgmatter = (((sed_phys$rewet_dry_mass - sed_phys$transfertin_mass) - 
                        (sed_phys$ashed_mass - sed_phys$transfertin_mass))/
  (sed_phys$rewet_dry_mass - sed_phys$transfertin_mass))  *100

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
  mutate(porosity = case_when(is.na(.$porosity) ~ med_porosity, 
                              TRUE ~ porosity)) %>%
  select(-med_porosity) %>%
  mutate(orgmatter = case_when(.$orgmatter < 0 ~ -9999, 
                               TRUE ~ orgmatter)) %>%
  na_if(., -9999)

sed_phys2 = left_join(sed_phys1, bd_replace, by = c("site", "sedDepth")) %>%
  mutate(bulk_density = case_when(is.na(.$bulk_density) ~ med_bd, 
                                  TRUE ~ bulk_density)) %>%
  select(-med_bd, -vial_mass, -sed_vol:-notes) 


#================================
#Subset core data and munge
ghg = read.csv("mendota_ch4_ghg_2021.csv")
core_raw = ghg %>%
  filter(type == "Core") %>%
  filter(!(site == "Y3" | site == "G3" | site == "PB3")) %>%
  select(-analysis_date, -export, -GC_batch_id, -GC_sample_id, 
         -date, - time, -waterDepth, - rep,
         -CO2_FID, -CO2_TCD, -CH4_TCD, -N2O_ECD) %>%
  rename(ch4_ppm = CH4_FID)

#joined core data with sediment physical characteristics
core_sed = left_join(core_raw, sed_phys2, by = c("site", "doy", "sedDepth"))


#================================
#calculate pore water concentrations

KH_ch4 = 0.00142 # KH = concentration / partial pressure
KH_td_ch4 = 1600 # Temperature-dependence constants for KH (unitless)

#_Ideal Gas Law-----------
### Used to calculate concentration of a gas based on its partial pressure
# [gas] = (P/RT)*(10^6 umol/mol) ; units = umol/L

ideal_gas_law = function(pp, temp) {
  conc = (pp / (0.0821 * (273.15 + temp))) * 10^6
  return(conc)
}

pore_water = core_sed %>%
  # calculate partial pressures of gases in headspace (units = atmosphere)
  # convert measured syringe values from ppm to atm
  mutate(pch4_head = ch4_ppm / 10^6 * 0.99) %>%
  
  # temperature-corrected Henry's Law constants (KH) based on pond surface water temperatures
  mutate(tKH_ch4 = KH_ch4 * exp(KH_td_ch4 * ((1 / (25 + 273.15)) - (1 / 298.15)))) %>%
  
  # concentration of gases in equilibrated headspace (units = uM)
  mutate(ch4_head = ideal_gas_law(pch4_head, 25)) %>%
  
  # concentration of dissolved gases in equilibrated aqueous sample (units = uM)
  mutate(ch4_aq = tKH_ch4 * pch4_head * 10^6) %>%
  
  # the volume of water that came from the salt solution + pore water (units = mL)
  mutate(vol_poresol = (70 - ch4_sedvol) + (ch4_sedvol * porosity)) %>%
  mutate(vol_porewater = (ch4_sedvol * porosity)) %>%
  
  # total amount of gas in syringe (units = umol) 
  # vol_poresol is the volume of aqueous solution, - 20 mL for the added headspace 
  mutate(ch4_tot_umol = (ch4_aq * ((vol_poresol - 20)/1000)) + (ch4_head * 0.02)) %>%
  
  # original gas concentration dissolved in lake water (units = uM)
  # after accounting for the dilution with salt solution
  mutate(ch4_porewater = (ch4_tot_umol / ((vol_poresol - 20)/1000)) * 
           (vol_poresol/vol_porewater))
### NEED TO STILL ACCOUNT FOR THE CH4 in the salt solution and subtract from this total, but this is good enough for JASM abstract submission

# write.csv(pore_water, file = "core_methane_analysis.csv")

# boxplot(pore_water$orgmatter ~ pore_water$site, las = 2)

