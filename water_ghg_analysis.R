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
  select(-analysis_date, -export, -GC_batch_id, -GC_sample_id, 
         -date, -time, -sedDepth, -rep) %>%
  group_by(site, doy, waterDepth) %>%
  #calculate mean ppm from duplicate samples
  summarize(ch4 = mean(CH4_FID, na.rm = TRUE),
            co2 = mean(CO2_TCD, na.rm = TRUE),
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
            atmo_co2_GC = mean(CO2_TCD, na.rm = TRUE),
            atmo_n2o_GC = mean(N2O_ECD, na.rm = TRUE)) %>%
  ungroup()

#Subset the ysi profiles to surface values only
surface_ysi = profiles %>%
  filter(depth == 0) %>%
  select(-time)

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
         
         tKH_co2 = 
           KH_co2 * exp(KH_td_co2 * ((1 / (temp + 273.15)) - (1 / 298.15))),
         
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
         pco2_atmo = atmo_co2_GC / 10^6 * 0.99,
         pn2o_atmo = atmo_n2o_GC / 10^6 * 0.99) %>%
  # Ideal Gas Law (units = uM)
  mutate(ch4_atmo = ideal_gas_law(pch4_atmo, 25),
         co2_atmo = ideal_gas_law(pco2_atmo, 25),
         n2o_atmo = ideal_gas_law(pn2o_atmo, 25))

#==============================
#Calculate the concentration of dissolved gas in the sample
lake_conc = syr_conc %>%
  # add atmosphere concentrations to syringe concentrations
  left_join(atm_conc %>% select(doy, ends_with("atmo"))) %>%
  # add original lake water partial pressure from above 
  left_join(lake_co2) %>%
  # original gas concentration dissolved in lake water (units = uM)
  mutate(ch4_lake = (ch4_tot_umol - (ch4_atmo * vol_air)) / vol_water,
         n2o_lake = (n2o_tot_umol - (n2o_atmo * vol_air)) / vol_water,
         co2_lake = tKH_co2 * pco2_aq * 10^6) %>%
  # drop unnecessary variables from calculations
  select(sample_id:doy, surface_temp, ends_with("lake"), ends_with("atmo"), starts_with("tKH"))








#=============================================================

  #Make the site names something another human being would understand
  mutate(site = replace(site, site=="S0", "Sixmile Inlet"),
         site = replace(site, site=="Y0", "Yahara Inlet"),
         site = replace(site, site=="Y1", "Yahara Estuary\nNearshore"),
         site = replace(site, site=="Y2", "Yahara Estuary\nOffshore"),
         site = replace(site, site=="G1", "Yahara Estuary\nNearshore REF"),
         site = replace(site, site=="G2", "Yahara Estuary\nOffshore REF"),
         site = replace(site, site=="PB0", "PB Inlet"),
         site = replace(site, site=="PB1", "PB Estuary\nNearshore"),
         site = replace(site, site=="PB2", "PB Estuary\nOffshore"),
         site = replace(site, site=="CFL1", "PB Estuary\nNearshore REF"),
         site = replace(site, site=="CFL2", "PB Estuary\nOffshore REF"))

#Order the sites in a way that makes sense for comparison
water$site <- factor(water$site, 
                     levels = c("PB Estuary\nOffshore REF",
                                "PB Estuary\nOffshore",
                                "PB Estuary\nNearshore REF",
                                "PB Estuary\nNearshore",
                                "PB Inlet",
                                "Yahara Estuary\nOffshore REF",
                                "Yahara Estuary\nOffshore",
                                "Yahara Estuary\nNearshore REF",
                                "Yahara Estuary\nNearshore",
                                "Yahara Inlet",
                                "Sixmile Inlet"))

#Subset just the surface water data
surface = water %>%
  filter(waterDepth == "Surface") 
surface = as.data.frame(surface)

#Subset just the bottom water data
bottom = water %>%
  filter(waterDepth == "Bottom")
bottom = as.data.frame(bottom)

#Color palette for visualization
offshore = rgb(34,87,126, max = 255, alpha = 200)
nearshore = rgb(77,148,168, max = 255, alpha = 200)
inlet = rgb(149,209,204, max = 255, alpha = 200)

#======================================================
# Surface Methane
windows(height = 9, width = 6.5)
ggplot(surface, aes(x = ch4, y = site, fill = site)) + 
  geom_density_ridges2(scale = 2, 
                       linetype = 0, 
                       panel_scaling = TRUE) +
  scale_fill_cyclical(values = c(offshore, offshore, 
                                 nearshore, nearshore, inlet, 
                                 offshore, offshore, 
                                 nearshore, nearshore, inlet, inlet)) +
  theme_ridges() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = 'Surface Water Methane') +
  xlab(label = 'Methane Concentration (ppm)') +
  ylab(label = '')

#======================================================
# Surface Carbon Dioxide
windows(height = 9, width = 6.5)
ggplot(surface, aes(x = co2, y = site, fill = site)) + 
  geom_density_ridges2(scale = 2, 
                       linetype = 0, 
                       panel_scaling = TRUE) +
  scale_fill_cyclical(values = c(offshore, offshore, 
                                 nearshore, nearshore, inlet, 
                                 offshore, offshore, 
                                 nearshore, nearshore, inlet, inlet)) +
  theme_ridges() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = 'Surface Water CO2') +
  xlab(label = 'CO2Concentration (ppm)') +
  ylab(label = '')
