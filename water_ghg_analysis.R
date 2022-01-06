#Water CH4 Analysis - Mendota Methane Madness
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

ghg = read.csv("mendota_ch4_ghg_2021.csv")

#Subset out the water values and take the mean of replicates
water = ghg %>%
  filter(type == "Water") %>%
  filter(!(site == "Y3" | site == "G3" | site == "PB3")) %>%
  group_by(site, doy, waterDepth) %>%
  summarize(ch4 = mean(CH4_FID, na.rm = TRUE),
            co2 = mean(CO2_TCD, na.rm = TRUE)) %>%
  ungroup() %>%
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

surface = water %>%
  filter(waterDepth == "Surface") 
surface = as.data.frame(surface)

bottom = water %>%
  filter(waterDepth == "Bottom")
bottom = as.data.frame(bottom)

#Color palette for visualization
offshore = rgb(34,87,126, max = 255, alpha = 200)
nearshore = rgb(77,148,168, max = 255, alpha = 200)
inlet = rgb(149,209,204, max = 255, alpha = 200)

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
