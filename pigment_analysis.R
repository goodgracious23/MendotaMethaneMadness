#PIGMENT & SUSPENDED SOLIDS DATA VISUALIZATION - Mendota Methane Madness
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(ggridges)) install.packages('ggridges')
library(ggridges)
if (!require(gridExtra)) install.packages('gridExtra')
library(gridExtra)

#Color palette for visualization
nearshore = rgb(34,87,126, max = 255, alpha = 200)
offshore = rgb(77,148,168, max = 255, alpha = 200)
inlet = rgb(149,209,204, max = 255, alpha = 200)

#Read in and Clean pigment data
pigments = read.csv("mendota_ch4_pigments_2021.csv")
pig = pigments %>%
  filter(!(site == "Y3" | site == "G3" | site == "PB3")) %>%
  group_by(doy, site, group) %>%
  summarize(chl = mean(Chl, na.rm = TRUE),
            pc = mean(PC, na.rm = TRUE)) %>%
  ungroup()

pig$site <- factor(pig$site, levels = c("CFL1", "CFL2", "G1", "G2", "PB0", "PB1", "PB2","S0", "Y0", "Y1", "Y2"))

#Ridgeline Plots of Pigments - TIME SERIES
windows(height = 9, width = 6.5)
ggplot(pig, aes(doy, site, height = chl, fill = site, group = site)) + 
  geom_density_ridges2(stat = "identity", 
                       scale = 3, 
                       linetype = 0, 
                       panel_scaling = TRUE) +
  scale_fill_cyclical(values = c(offshore, nearshore, offshore, nearshore, inlet,
                                 offshore, nearshore, inlet, inlet, offshore, nearshore)) +
  theme_ridges() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(145,270)) +
  labs(title = 'Chlorophyll (RFUs)')


windows(height = 9, width = 6.5)
ggplot(pig, aes(doy, site, height = pc, fill = site, group = site)) + 
  geom_density_ridges2(stat = "identity", 
                       scale = 3, 
                       linetype = 0, 
                       panel_scaling = TRUE) +
  scale_fill_cyclical(values = c(offshore, nearshore, offshore, nearshore, inlet,
                                 offshore, nearshore, inlet, inlet, offshore, nearshore)) +
  theme_ridges() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(145,270)) +
  labs(title = 'Phycocyanin (RFUs)')

#=======================================================
#Ridgeline Plots of Pigments - DENSITY
windows(height = 9, width = 12)
chl_ridge <-
  ggplot(pig, aes(x = chl, y = site, fill = site)) + 
  geom_density_ridges2(scale = 3, 
                       linetype = 0, 
                       panel_scaling = TRUE) +
  scale_fill_cyclical(values = c(offshore, nearshore, 
                                 offshore, nearshore, 
                                 inlet, offshore, nearshore, 
                                 inlet, inlet, offshore, nearshore)) +
  theme_ridges() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = 'Chlorophyll') +
  xlab(label = 'Chlorophyll (RFUs)')

pc_ridge <-
  ggplot(pig, aes(x = pc, y = site, fill = site)) + 
  geom_density_ridges2(scale = 3, 
                       linetype = 0, 
                       panel_scaling = TRUE) +
  scale_fill_cyclical(values = c(offshore, nearshore, 
                                 offshore, nearshore, 
                                 inlet, offshore, nearshore, 
                                 inlet, inlet, offshore, nearshore)) +
  theme_ridges() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = 'Phycocyanin') +
  xlab(label = 'Phycocyanin (RFUs)')

grid.arrange(chl_ridge, pc_ridge, nrow = 1)

#=================================================================
#SUSPENDED SOLIDS ANALYSIS - Mendota Methane Madness
#Read in and clean suspended solids data
ss = read.csv("mendota_ch4_suspendedsolids_2021.csv")

ss$tss = ((ss$dry_mass - ss$filter_mass) / (ss$volume_filtered/1000))*1000
ss$vss = ((ss$dry_mass - ss$ashed_mass) / (ss$volume_filtered/1000))*1000
ss$iss = ((ss$ashed_mass - ss$filter_mass) / (ss$volume_filtered/1000))*1000

ss$site <- factor(ss$site, levels = c("CFL2", "CFL1", "PB2", "PB1", "PB0",
                                      "G1", "G2","Y0", "S0", "Y2", "Y1"))

#Ridgeline Plots of Total Suspended Solids
windows(height = 9, width = 6.5)
ggplot(ss, aes(doy, site, height = tss, fill = site, group = site)) + 
  geom_density_ridges2(stat = "identity", 
                       scale = 3, 
                       linetype = 0, 
                       panel_scaling = TRUE) +
  scale_fill_cyclical(values = c(offshore, nearshore, offshore, nearshore, inlet,
                                 offshore, nearshore, inlet, inlet, offshore, nearshore)) +
  theme_ridges() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(limits = c(145,270)) +
  labs(title = 'Total Suspended Solids (mg/L)')
