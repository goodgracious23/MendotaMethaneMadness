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

sed_phys1 = left_join(sed_phys, por_replace, by = c("site", "sedDepth")) %>%
  mutate(porosity = case_when(is.na(.$porosity) ~ med_porosity, TRUE ~ porosity)) %>%
  select(-med_porosity) %>%
  mutate(orgmatter = case_when(.$orgmatter < 0 ~ -9999, TRUE ~ orgmatter)) %>%
  na_if(., -9999)
  


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
sed_surface = sed_phys1 %>%
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

# KH = concentration / partial pressure
KH_ch4 = 0.00142

# Temperature-dependence constants for KH (unitless)
KH_td_ch4 = 1600

#_Ideal Gas Law
### Used to calculate concentration of a gas based on its partial pressure
# [gas] = (P/RT)*(10^6 umol/mol) ; units = umol/L

ideal_gas_law = function(pp, temp) {
  conc = (pp / (0.0821 * (273.15 + temp))) * 10^6
  return(conc)
}

methano_samples = incubations %>%
  # calculate partial pressure of gases (units = atm)
  mutate(pch4_t1_A = t1_ch4_A / 10^6 * 0.99,
         pch4_t1_B = t1_ch4_B / 10^6 * 0.99,
         pch4_t2_A = t2_ch4_A / 10^6 * 0.99,
         pch4_t2_B = t2_ch4_B / 10^6 * 0.99) %>%
  # ideal gas law to get final concentration (units = uM)
  mutate(ch4_t1_A_head = ideal_gas_law(pch4_t1_A, inc_temp),
         ch4_t1_B_head = ideal_gas_law(pch4_t1_B, inc_temp),
         ch4_t2_A_head = ideal_gas_law(pch4_t2_A, inc_temp),
         ch4_t2_B_head = ideal_gas_law(pch4_t2_B, inc_temp))

methano_samples = methano_samples %>%
  # temperature-corrected Henry's Law constants (tKH)
  mutate(tKH_ch4 = KH_ch4 * exp(KH_td_ch4 * ((1 / (inc_temp + 273.15)) - (1 / 298.15)))) %>%
  # concentration dissolved in water sample (units = uM)
  mutate(ch4_aq_t1_A = tKH_ch4 * pch4_t1_A * 10^6,
         ch4_aq_t1_B = tKH_ch4 * pch4_t1_B * 10^6,
         ch4_aq_t2_A = tKH_ch4 * pch4_t2_A * 10^6,
         ch4_aq_t2_B = tKH_ch4 * pch4_t2_B * 10^6)

methano_samples = methano_samples %>%
  mutate(vol_aq = vol_water + (vol_sed * porosity)) %>%
  mutate(mass_sed = vol_sed * bulk_density * 1000,
         mass_slurry = vol_aq + mass_sed)

##__Methane Production Rate (units = umol g-1 h-1)
methano_samples = methano_samples %>%
  mutate(vol_head = 125 - (vol_sed + vol_water)) %>%
  # total amount of gases in bottle at end of incubation (headspace + dissolved) (units = umol)
  mutate(ch4_tot_umol_t1_A = (ch4_aq_t1_A * vol_aq/1000) + (ch4_t1_A_head * vol_head),
         ch4_tot_umol_t1_B = (ch4_aq_t1_B * vol_aq/1000) + (ch4_t1_B_head * vol_head),
         ch4_tot_umol_t2_A = (ch4_aq_t2_A * vol_aq/1000) + (ch4_t2_A_head * vol_head),
         ch4_tot_umol_t2_B = (ch4_aq_t2_B * vol_aq/1000) + (ch4_t2_B_head * vol_head)) %>%
  # hourly rate of production per gram of slurry mass (units = umol g-1 h-1)
  mutate(ch4_rate_t1_A = ch4_tot_umol_t1_A / mass_slurry / t1_hours_A,
         ch4_rate_t1_B = ch4_tot_umol_t1_B / mass_slurry / t1_hours_B,
         ch4_rate_t2_A = ch4_tot_umol_t2_A / mass_slurry / t2_hours_A,
         ch4_rate_t2_B = ch4_tot_umol_t2_B / mass_slurry / t2_hours_B)


#=============================================================
# Let's do some plotting now
#=============================================================
#Color palette for visualization
offshore = rgb(34,87,126, max = 255, alpha = 200)
nearshore = rgb(77,148,168, max = 255, alpha = 200)
inlet = rgb(149,209,204, max = 255, alpha = 200)

#========================================
# Quickly summarize the reps and time points for JASM abstract
sum_methanogenesis = methano_samples %>%
  group_by(site, doy) %>%
  summarize(mean_methanogenesis = mean(c(ch4_rate_t1_A,
                                         ch4_rate_t1_B,
                                         ch4_rate_t2_A,
                                         ch4_rate_t2_B), na.rm = TRUE)) %>%
  ungroup()
sum_methanogenesis = as.data.frame(sum_methanogenesis)

windows(height = 4, width = 8)
par(mfrow = c(1,2), omi = c(0.3,0.5,0.3,0.2), mai = c(0.5,0.5,0,0))

plot(sum_methanogenesis[sum_methanogenesis$site=="S0", "doy"],
     sum_methanogenesis[sum_methanogenesis$site=="S0", "mean_methanogenesis"],
     pch = 15, cex = 1.5, lwd = 4, type = "o", col = inlet,
     ylim = c(0,0.42),
     xlab = "", ylab = "")
mtext(side = 2, line = 3, "Methanogenesis (uM g-1 d-1)", font = 2)
mtext(side = 3, line = 0.5, "Yahara Estuary", font = 2)
points(sum_methanogenesis[sum_methanogenesis$site=="Y0", "doy"],
       sum_methanogenesis[sum_methanogenesis$site=="Y0", "mean_methanogenesis"],
       pch = 19, lwd = 4, type = "o", col = inlet)
points(sum_methanogenesis[sum_methanogenesis$site=="Y1", "doy"],
       sum_methanogenesis[sum_methanogenesis$site=="Y1", "mean_methanogenesis"],
       pch = 19, lwd = 4,  type = "o", col = nearshore)
points(sum_methanogenesis[sum_methanogenesis$site=="Y2", "doy"],
       sum_methanogenesis[sum_methanogenesis$site=="Y2", "mean_methanogenesis"],
       pch = 19, lwd = 4, type = "o", col = offshore)
points(sum_methanogenesis[sum_methanogenesis$site=="G1", "doy"],
       sum_methanogenesis[sum_methanogenesis$site=="G1", "mean_methanogenesis"],
       pch = 19, lwd = 4, lty = 3, type = "o", col = nearshore)
points(sum_methanogenesis[sum_methanogenesis$site=="G2", "doy"],
       sum_methanogenesis[sum_methanogenesis$site=="G2", "mean_methanogenesis"],
       pch = 19, lwd = 4, lty = 3, type = "o", col = offshore)

plot(sum_methanogenesis[sum_methanogenesis$site=="PB0", "doy"],
     sum_methanogenesis[sum_methanogenesis$site=="PB0", "mean_methanogenesis"],
     pch = 15, cex = 1.5, lwd = 4, type = "o", col = inlet,
     ylim = c(0,0.42),
     xlab = "", ylab = "Methanogenesis Rate (uM)")
mtext(side = 3, line = 0.5, "Pheasant Branch Estuary", font = 2)
points(sum_methanogenesis[sum_methanogenesis$site=="PB1", "doy"],
       sum_methanogenesis[sum_methanogenesis$site=="PB1", "mean_methanogenesis"],
       pch = 19, lwd = 4,  type = "o", col = nearshore)
points(sum_methanogenesis[sum_methanogenesis$site=="PB2", "doy"],
       sum_methanogenesis[sum_methanogenesis$site=="PB2", "cmean_methanogenesis"],
       pch = 19, lwd = 4, type = "o", col = offshore)
points(sum_methanogenesis[sum_methanogenesis$site=="CFL1", "doy"],
       sum_methanogenesis[sum_methanogenesis$site=="CFL1", "mean_methanogenesis"],
       pch = 19, lwd = 4, lty = 3, type = "o", col = nearshore)
points(sum_methanogenesis[sum_methanogenesis$site=="CFL2", "doy"],
       sum_methanogenesis[sum_methanogenesis$site=="CFL2", "mean_methanogenesis"],
       pch = 19, lwd = 4, lty = 3, type = "o", col = offshore)


#===============================================
sum_ch4_potential = sum_methanogenesis %>%
  #Make the site names something another human being would understand
  mutate(site = replace(site, site=="S0", "Sixmile Inlet"),
         site = replace(site, site=="Y0", "Yahara Inlet"),
         site = replace(site, site=="Y1", "Yahara Estuary\nNearshore"),
         site = replace(site, site=="Y2", "Yahara Estuary\nOffshore"),
         site = replace(site, site=="G1", "Yahara Estuary\nNearshore REF"),
         site = replace(site, site=="G2", "Yahara Estuary\nOffshore REF"),
         site = replace(site, site=="PB0", "Pheasant Branch\nInlet"),
         site = replace(site, site=="PB1", "Pheasant Branch\nEstuary Nearshore"),
         site = replace(site, site=="PB2", "Pheasant Branch\nEstuary Offshore"),
         site = replace(site, site=="CFL1", "Pheasant Branch Estuary\nNearshore REF"),
         site = replace(site, site=="CFL2", "Pheasant Branch Estuary\nOffshore REF"))
#Order the sites in a way that makes sense for comparison
sum_ch4_potential$site <- factor(sum_ch4_potential$site, 
                           levels = c("Pheasant Branch Estuary\nOffshore REF",
                                      "Pheasant Branch\nEstuary Offshore",
                                      "Pheasant Branch Estuary\nNearshore REF",
                                      "Pheasant Branch\nEstuary Nearshore",
                                      "Pheasant Branch\nInlet",
                                      "Yahara Estuary\nOffshore REF",
                                      "Yahara Estuary\nOffshore",
                                      "Yahara Estuary\nNearshore REF",
                                      "Yahara Estuary\nNearshore",
                                      "Yahara Inlet",
                                      "Sixmile Inlet"))

windows(height = 9, width = 6.5)
ggplot(sum_ch4_potential, aes(x = mean_methanogenesis, y = site, fill = site)) + 
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
  labs(title = 'Methanogenesis Potential') +
  xlab(label = 'uM/g/h') +
  ylab(label = '')



