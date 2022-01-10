#Water CH4 Analysis - Mendota Methane Madness
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(ggridges)) install.packages('ggridges')
library(ggridges)

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
  mutate(ch4_lake = (ch4_tot_umol - (ch4_atmo * 0.03)) / 0.03,
         n2o_lake = (n2o_tot_umol - (n2o_atmo * 0.03)) / 0.03)
lake_conc = as.data.frame(lake_conc)


#=============================================================
# Let's do some plotting now
#=============================================================
#Color palette for visualization
offshore = rgb(34,87,126, max = 255, alpha = 200)
nearshore = rgb(77,148,168, max = 255, alpha = 200)
inlet = rgb(149,209,204, max = 255, alpha = 200)

#======================================================
# Surface Methane Time Series
windows(height = 6, width = 8)
par(mfrow = c(2,2), omi = c(0.3,0.5,0.3,0.2), mai = c(0.5,0.5,0,0))

plot(lake_conc[lake_conc$site=="S0", "doy"],
     lake_conc[lake_conc$site=="S0", "ch4_lake"],
     pch = 15, cex = 1.5, lwd = 4, type = "o", col = inlet,
     ylim = c(-0.5,8),
     xlab = "", ylab = "")
mtext(side = 2, line = 3, "Methane (uM)", font = 2)
mtext(side = 3, line = 0.5, "Yahara Estuary", font = 2)
polygon(c(135,280,280,135), c(-1,-1, 0.047, 0.047), 
        col = "gray70", border = NA); box()
points(lake_conc[lake_conc$site=="Y0", "doy"],
       lake_conc[lake_conc$site=="Y0", "ch4_lake"],
       pch = 19, lwd = 4, type = "o", col = inlet)
points(lake_conc[lake_conc$site=="Y1", "doy"],
       lake_conc[lake_conc$site=="Y1", "ch4_lake"],
       pch = 19, lwd = 4,  type = "o", col = nearshore)
points(lake_conc[lake_conc$site=="Y2", "doy"],
       lake_conc[lake_conc$site=="Y2", "ch4_lake"],
       pch = 19, lwd = 4, type = "o", col = offshore)
points(lake_conc[lake_conc$site=="G1", "doy"],
       lake_conc[lake_conc$site=="G1", "ch4_lake"],
       pch = 19, lwd = 4, lty = 3, type = "o", col = nearshore)
points(lake_conc[lake_conc$site=="G2", "doy"],
       lake_conc[lake_conc$site=="G2", "ch4_lake"],
       pch = 19, lwd = 4, lty = 3, type = "o", col = offshore)

plot(lake_conc[lake_conc$site=="PB0", "doy"],
     lake_conc[lake_conc$site=="PB0", "ch4_lake"],
     pch = 15, cex = 1.5, lwd = 4, type = "o", col = inlet,
     ylim = c(-0.5,8),
     xlab = "", ylab = "Methane (uM)")
mtext(side = 3, line = 0.5, "Pheasant Branch Estuary", font = 2)
polygon(c(135,280,280,135), c(-1,-1, 0.047, 0.047), 
        col = "gray70", border = NA); box()
points(lake_conc[lake_conc$site=="PB1", "doy"],
       lake_conc[lake_conc$site=="PB1", "ch4_lake"],
       pch = 19, lwd = 4,  type = "o", col = nearshore)
points(lake_conc[lake_conc$site=="PB2", "doy"],
       lake_conc[lake_conc$site=="PB2", "ch4_lake"],
       pch = 19, lwd = 4, type = "o", col = offshore)
points(lake_conc[lake_conc$site=="CFL1", "doy"],
       lake_conc[lake_conc$site=="CFL1", "ch4_lake"],
       pch = 19, lwd = 4, lty = 3, type = "o", col = nearshore)
points(lake_conc[lake_conc$site=="CFL2", "doy"],
       lake_conc[lake_conc$site=="CFL2", "ch4_lake"],
       pch = 19, lwd = 4, lty = 3, type = "o", col = offshore)

#======================================================
# Surface Nitrous Oxide Time Series
plot(lake_conc[lake_conc$site=="S0", "doy"],
     lake_conc[lake_conc$site=="S0", "n2o_lake"],
     pch = 19, lwd = 4, type = "o", col = inlet,
     ylim = c(-0.01, 0.20),
     xlab = "", ylab = "")
mtext(side = 2, line = 3, "Nitrous Oxide (uM)", font = 2)
mtext(side = 1, line = 3, "Day of Year", font = 2)
polygon(c(135,280,280,135), c(-1,-1, 0.0189, 0.0189), 
        col = "gray70", border = NA); box()
points(lake_conc[lake_conc$site=="S0", "doy"],
       lake_conc[lake_conc$site=="S0", "n2o_lake"],
       pch = 19, lwd = 4, type = "o", col = inlet)
points(lake_conc[lake_conc$site=="Y0", "doy"],
       lake_conc[lake_conc$site=="Y0", "n2o_lake"],
       pch = 19, lwd = 4, type = "o", col = inlet)
points(lake_conc[lake_conc$site=="Y1", "doy"],
       lake_conc[lake_conc$site=="Y1", "n2o_lake"],
       pch = 19, lwd = 4,  type = "o", col = nearshore)
points(lake_conc[lake_conc$site=="Y2", "doy"],
       lake_conc[lake_conc$site=="Y2", "n2o_lake"],
       pch = 19, lwd = 4, type = "o", col = offshore)
points(lake_conc[lake_conc$site=="G1", "doy"],
       lake_conc[lake_conc$site=="G1", "n2o_lake"],
       pch = 19, lwd = 4, lty = 3, type = "o", col = nearshore)
points(lake_conc[lake_conc$site=="G2", "doy"],
       lake_conc[lake_conc$site=="G2", "n2o_lake"],
       pch = 19, lwd = 4, lty = 3, type = "o", col = offshore)

plot(lake_conc[lake_conc$site=="PB0", "doy"],
     lake_conc[lake_conc$site=="PB0", "n2o_lake"],
     pch = 15, cex = 1.5, lwd = 4, type = "o", col = inlet,
     ylim = c(-0.01, 0.20),
     xlab = "", ylab = "")
mtext(side = 1, line = 3, "Day of Year", font = 2)
polygon(c(135,280,280,135), c(-1,-1, 0.0189, 0.0189), 
        col = "gray70", border = NA); box()
points(lake_conc[lake_conc$site=="PB1", "doy"],
       lake_conc[lake_conc$site=="PB1", "n2o_lake"],
       pch = 19, lwd = 4,  type = "o", col = nearshore)
points(lake_conc[lake_conc$site=="PB2", "doy"],
       lake_conc[lake_conc$site=="PB2", "n2o_lake"],
       pch = 19, lwd = 4, type = "o", col = offshore)
points(lake_conc[lake_conc$site=="CFL1", "doy"],
       lake_conc[lake_conc$site=="CFL1", "n2o_lake"],
       pch = 19, lwd = 4, lty = 3, type = "o", col = nearshore)
points(lake_conc[lake_conc$site=="CFL2", "doy"],
       lake_conc[lake_conc$site=="CFL2", "n2o_lake"],
       pch = 19, lwd = 4, lty = 3, type = "o", col = offshore)


#======================================================
# Surface Methane Density Plots

mendota_ghg = lake_conc %>%
  select(!(atmo_ch4_GC:n2o_atmo)) %>%
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
mendota_ghg$site <- factor(mendota_ghg$site, 
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
ggplot(mendota_ghg, aes(x = ch4_lake, y = site, fill = site)) + 
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
  xlab(label = 'Methane Concentration (uM)') +
  ylab(label = '')


