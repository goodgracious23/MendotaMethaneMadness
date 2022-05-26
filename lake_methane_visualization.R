#=============================================================
# Let's do some plotting now
#=============================================================
if (!require(ggridges)) install.packages('ggridges')
library(ggridges)
lake_ch4 = read.csv("Lake_Concentrations_Profiles.csv")

#Color palette for visualization
# offshore = rgb(34,87,126, max = 255, alpha = 100)
# nearshore = rgb(77,148,168, max = 255, alpha = 100)
# inlet = rgb(149,209,204, max = 255, alpha = 100)
# reference = rgb(127,127,127, max = 255, alpha = 100)

offshore = rgb(180, 184, 151, max = 255, alpha = 220)
nearshore = rgb(54, 139, 133, max = 255, alpha = 200)
inlet = rgb(70, 70, 96, max = 255, alpha = 80)

#======================================================
#SURFACE - Time Series
plot(lake_ch4[lake_ch4$site=="S0", "doy"],
     lake_ch4[lake_ch4$site=="S0", "ch4_lake_surface"],
     col = "white", pch = 0, 
     ylim = c(0,8), xaxt = "n", ylab = "", xlab = "")
mtext(side = 2, line = 2.5, cex = 1.5,
      expression(CH[4]~"in water ("*mu*M*")"))
axis(1, at = c(152,182,213,244,274), 
     label = c("June", "July", "Aug", "Sept", "Oct"))
polygon(c(146,lake_ch4[lake_ch4$site=="Y0", "doy"],270),
        c(0,lake_ch4[lake_ch4$site=="Y0", "ch4_lake_surface"],0),
        col = inlet, border = NA)
polygon(c(146,lake_ch4[lake_ch4$site=="S0", "doy"],270),
        c(0,lake_ch4[lake_ch4$site=="S0", "ch4_lake_surface"],0),
        col = inlet, border = NA)
polygon(c(146,lake_ch4[lake_ch4$site=="Y1", "doy"],270),
        c(0,lake_ch4[lake_ch4$site=="Y1", "ch4_lake_surface"],0),
        col = nearshore, border = NA)
polygon(c(146,lake_ch4[lake_ch4$site=="Y2", "doy"],270),
        c(0,lake_ch4[lake_ch4$site=="Y2", "ch4_lake_surface"],0),
        col = offshore, border = NA)
points(lake_ch4[lake_ch4$site=="G1", "doy"],
       lake_ch4[lake_ch4$site=="G1", "ch4_lake_surface"],
       type = "l", col = "white", lwd = 3, lty = 1)
points(lake_ch4[lake_ch4$site=="G2", "doy"],
       lake_ch4[lake_ch4$site=="G2", "ch4_lake_surface"],
       type = "l", col = "white", lwd = 3, lty = 1)

#BOTTOM - Time Series
plot(lake_ch4[lake_ch4$site=="S0", "doy"],
     lake_ch4[lake_ch4$site=="S0", "ch4_lake_bottom"],
     col = "white", pch = 0, 
     ylim = c(0,9), xaxt = "n", ylab = "", xlab = "")
mtext(side = 2, line = 2.5, cex = 1.5,
      expression(CH[4]~"in water ("*mu*M*")"))
axis(1, at = c(152,182,213,244,274), 
     label = c("June", "July", "Aug", "Sept", "Oct"))
polygon(c(146,lake_ch4[lake_ch4$site=="S0", "doy"],270),
        c(0,lake_ch4[lake_ch4$site=="S0", "ch4_lake_bottom"],0),
        col = inlet, border = NA)
polygon(c(146,lake_ch4[lake_ch4$site=="Y0", "doy"],270),
        c(0,lake_ch4[lake_ch4$site=="Y0", "ch4_lake_bottom"],0),
        col = inlet, border = NA)
polygon(c(146,lake_ch4[lake_ch4$site=="Y1", "doy"],270),
        c(0,lake_ch4[lake_ch4$site=="Y1", "ch4_lake_bottom"],0),
        col = nearshore, border = NA)
polygon(c(146,lake_ch4[lake_ch4$site=="Y2", "doy"],270),
        c(0,lake_ch4[lake_ch4$site=="Y2", "ch4_lake_bottom"],0),
        col = offshore, border = NA)
polygon(c(146,lake_ch4[lake_ch4$site=="G1", "doy"],270),
        c(0,lake_ch4[lake_ch4$site=="G1", "ch4_lake_bottom"],0),
        col = reference, border = NA)
polygon(c(146,lake_ch4[lake_ch4$site=="G2", "doy"],270),
        c(0,lake_ch4[lake_ch4$site=="G2", "ch4_lake_bottom"],0),
        col = reference, border = NA)

# SURFACE Time Series - Pheasant Branch
inlet = rgb(70, 70, 96, max = 255, alpha = 150)

plot(lake_ch4[lake_ch4$site=="PB0", "doy"],
     lake_ch4[lake_ch4$site=="PB0", "ch4_lake_surface"],
     col = "white", pch = 0, 
     ylim = c(0,8), xaxt = "n", ylab = "", xlab = "")
mtext(side = 2, line = 2.5, cex = 1.5,
      expression(CH[4]~"in water ("*mu*M*")"))
axis(1, at = c(152,182,213,244,274), 
     label = c("June", "July", "Aug", "Sept", "Oct"))
polygon(c(139,lake_ch4[lake_ch4$site=="PB0", "doy"],237),
        c(0,lake_ch4[lake_ch4$site=="PB0", "ch4_lake_surface"],0),
        col = inlet, border = NA)
polygon(c(139,lake_ch4[lake_ch4$site=="PB1", "doy"],237),
        c(0,lake_ch4[lake_ch4$site=="PB1", "ch4_lake_surface"],0),
        col = nearshore, border = NA)
polygon(c(139,lake_ch4[lake_ch4$site=="PB2", "doy"],237),
        c(0,lake_ch4[lake_ch4$site=="PB2", "ch4_lake_surface"],0),
        col = offshore, border = NA)
points(lake_ch4[lake_ch4$site=="CFL1", "doy"],
       lake_ch4[lake_ch4$site=="CFL1", "ch4_lake_surface"],
       type = "l", col = "white", lwd = 3, lty = 1)
points(lake_ch4[lake_ch4$site=="CFL2", "doy"],
       lake_ch4[lake_ch4$site=="CFL2", "ch4_lake_surface"],
       type = "l", col = "white", lwd = 3, lty = 1)
# polygon(c(139,lake_ch4[lake_ch4$site=="CFL1", "doy"],237),
#         c(0,lake_ch4[lake_ch4$site=="CFL1", "ch4_lake_surface"],0),
#         col = reference, border = NA)
# polygon(c(139,lake_ch4[lake_ch4$site=="CFL2", "doy"],237),
#         c(0,lake_ch4[lake_ch4$site=="CFL2", "ch4_lake_surface"],0),
#         col = reference, border = NA)


# BOTTOM Time Series - Pheasant Branch
plot(lake_ch4[lake_ch4$site=="PB0", "doy"],
     lake_ch4[lake_ch4$site=="PB0", "ch4_lake_bottom"],
     col = "white", pch = 0, 
     ylim = c(0,8), xaxt = "n", ylab = "", xlab = "")
mtext(side = 2, line = 2.5, cex = 1.5,
      expression(CH[4]~"in water ("*mu*M*")"))
axis(1, at = c(152,182,213,244,274), 
     label = c("June", "July", "Aug", "Sept", "Oct"))
polygon(c(139,lake_ch4[lake_ch4$site=="PB0", "doy"],237),
        c(0,lake_ch4[lake_ch4$site=="PB0", "ch4_lake_bottom"],0),
        col = inlet, border = NA)
polygon(c(139,lake_ch4[lake_ch4$site=="PB1", "doy"],237),
        c(0,lake_ch4[lake_ch4$site=="PB1", "ch4_lake_bottom"],0),
        col = nearshore, border = NA)
polygon(c(139,lake_ch4[lake_ch4$site=="PB2", "doy"],237),
        c(0,lake_ch4[lake_ch4$site=="PB2", "ch4_lake_bottom"],0),
        col = offshore, border = NA)
polygon(c(139,lake_ch4[lake_ch4$site=="CFL1", "doy"],237),
        c(0,lake_ch4[lake_ch4$site=="CFL1", "ch4_lake_bottom"],0),
        col = reference, border = NA)
polygon(c(139,lake_ch4[lake_ch4$site=="CFL2", "doy"],237),
        c(0,lake_ch4[lake_ch4$site=="CFL2", "ch4_lake_bottom"],0),
        col = reference, border = NA)

#======================================================
# Surface Methane Density Plots
mendota_ghg = lake_ch4 %>%
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


# Surface Methane Concentrations
windows(height = 9, width = 6.5)
ggplot(mendota_ghg, aes(x = ch4_lake_surface, y = site, fill = site)) + 
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


#Bottom Water Methane Concentrations
windows(height = 9, width = 6.5)
ggplot(mendota_ghg, aes(x = ch4_lake_bottom, y = site, fill = site)) + 
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
  labs(title = 'Bottom Water Methane') +
  xlab(label = 'Methane Concentration (uM)') +
  ylab(label = '')