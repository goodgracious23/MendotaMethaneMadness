#PIGMENT DATA VISUALIZATION - Mendota Methane Madness

nearshore = rgb(34,87,126, max = 255, alpha = 200)
offshore = rgb(77,148,168, max = 255, alpha = 200)
inlet = rgb(149,209,204, max = 255, alpha = 200)

#Read in and Clean pigment data
pigments = read.csv("mendota_ch4_pigments_2021.csv")
pig = pigments %>%
  group_by(doy, site, group) %>%
  summarize(chl = mean(Chl, na.rm = TRUE),
            pc = mean(PC, na.rm = TRUE)) %>%
  ungroup()

pig$site <- factor(pig$site, levels = c("CFL2", "CFL1", "PB2", "PB1", "PB0",
                                        "G1", "G2","Y0", "S0", "Y2", "Y1"))

#Ridgeline Plots of Pigments
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


