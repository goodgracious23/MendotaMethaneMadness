rates = read.csv("Methanogenesis_Summary.csv")

#Color palette for visualization
offshore = rgb(180, 184, 151, max = 255, alpha = 220)
nearshore = rgb(54, 139, 133, max = 255, alpha = 200)
inlet = rgb(70, 70, 96, max = 255, alpha = 80)

plot(rates[rates$site=="Y0", "doy"], 
     rates[rates$site=="Y0", "t1_rate"],
     ylim = c(0,0.03), xlim = c(140, 280), 
     col = "white", pch = 0, xlab = "", ylab = "", xaxt = "n")
axis(1, at = c(152,182,213,244,274), 
     label = c("June", "July", "Aug", "Sept", "Oct"))
mtext(side = 2, line = 2.5, 
      expression(Methanogenesis~Potential~"("*mu*M~g^-1~h^-1*")"))
polygon(c(146,rates[rates$site=="Y0", "doy"],270),
        c(0,rates[rates$site=="Y0", "t1_rate"],0),
        col = inlet, border = NA)
polygon(c(146,rates[rates$site=="S0", "doy"],270),
        c(0,rates[rates$site=="S0", "t1_rate"],0),
        col = inlet, border = NA)
polygon(c(146,rates[rates$site=="Y1", "doy"],270),
        c(0,rates[rates$site=="Y1", "t1_rate"],0),
        col = nearshore, border = NA)
polygon(c(146,rates[rates$site=="Y2", "doy"],270),
        c(0,rates[rates$site=="Y2", "t1_rate"],0),
        col = offshore, border = NA)
points(rates[rates$site=="G1", "doy"],
       rates[rates$site=="G1", "t1_rate"],
       type = "l", col = "white", lwd = 3, lty = 1)
points(rates[rates$site=="G2", "doy"],
       rates[rates$site=="G2", "t1_rate"],
       type = "l", col = "white", lwd = 3, lty = 1)


plot(rates[rates$site=="PB0", "doy"], 
     rates[rates$site=="PB0", "t1_rate"],
     ylim = c(0,0.045), xlim = c(140, 240), 
     col = "white", pch = 0, xlab = "", ylab = "", xaxt = "n")
axis(1, at = c(152,182,213,244,274), 
     label = c("June", "July", "Aug", "Sept", "Oct"))
mtext(side = 2, line = 2.5, 
      expression(Methanogenesis~Potential~"("*mu*M~g^-1~h^-1*")"))
polygon(c(139,rates[rates$site=="PB0", "doy"],237),
        c(0,rates[rates$site=="PB0", "t1_rate"],0),
        col = inlet, border = NA)
polygon(c(139,rates[rates$site=="PB1", "doy"],237),
        c(0,rates[rates$site=="PB1", "t1_rate"],0),
        col = nearshore, border = NA)
polygon(c(139,rates[rates$site=="PB2", "doy"],237),
        c(0,rates[rates$site=="PB2", "t1_rate"],0),
        col = offshore, border = NA)
points(rates[rates$site=="CFL1", "doy"],
       rates[rates$site=="CFL1", "t1_rate"],
       type = "l", col = "white", lwd = 3, lty = 1)
points(rates[rates$site=="CFL2", "doy"],
       rates[rates$site=="CFL2", "t1_rate"],
       type = "l", col = "white", lwd = 3, lty = 1)





# #===============================================
# sum_ch4_potential = sum_methanogenesis %>%
#   #Make the site names something another human being would understand
#   mutate(site = replace(site, site=="S0", "Sixmile Inlet"),
#          site = replace(site, site=="Y0", "Yahara Inlet"),
#          site = replace(site, site=="Y1", "Yahara Estuary\nNearshore"),
#          site = replace(site, site=="Y2", "Yahara Estuary\nOffshore"),
#          site = replace(site, site=="G1", "Yahara Estuary\nNearshore REF"),
#          site = replace(site, site=="G2", "Yahara Estuary\nOffshore REF"),
#          site = replace(site, site=="PB0", "Pheasant Branch\nInlet"),
#          site = replace(site, site=="PB1", "Pheasant Branch\nEstuary Nearshore"),
#          site = replace(site, site=="PB2", "Pheasant Branch\nEstuary Offshore"),
#          site = replace(site, site=="CFL1", "Pheasant Branch Estuary\nNearshore REF"),
#          site = replace(site, site=="CFL2", "Pheasant Branch Estuary\nOffshore REF"))
# #Order the sites in a way that makes sense for comparison
# sum_ch4_potential$site <- factor(sum_ch4_potential$site, 
#                                  levels = c("Pheasant Branch Estuary\nOffshore REF",
#                                             "Pheasant Branch\nEstuary Offshore",
#                                             "Pheasant Branch Estuary\nNearshore REF",
#                                             "Pheasant Branch\nEstuary Nearshore",
#                                             "Pheasant Branch\nInlet",
#                                             "Yahara Estuary\nOffshore REF",
#                                             "Yahara Estuary\nOffshore",
#                                             "Yahara Estuary\nNearshore REF",
#                                             "Yahara Estuary\nNearshore",
#                                             "Yahara Inlet",
#                                             "Sixmile Inlet"))
# 
# windows(height = 9, width = 6.5)
# ggplot(sum_ch4_potential, aes(x = t1_methanogenesis, y = site, fill = site)) + 
#   geom_density_ridges2(scale = 2, 
#                        linetype = 0, 
#                        panel_scaling = TRUE) +
#   scale_fill_cyclical(values = c(offshore, offshore, 
#                                  nearshore, nearshore, inlet, 
#                                  offshore, offshore, 
#                                  nearshore, nearshore, inlet, inlet)) +
#   theme_ridges() +
#   scale_y_discrete(expand = c(0, 0)) +
#   scale_x_continuous(expand = c(0, 0)) +
#   labs(title = 'Methanogenesis Potential') +
#   xlab(label = 'uM/g/h') +
#   ylab(label = '')
# 
# plot(alldat[alldat$site=="Y0", "surface_ch4_lake"] ~
#        alldat[alldat$site=="Y0", "t1_methanogenesis"])
# 
# plot(alldat$surface_ch4_lake ~ alldat$t1_methanogenesis)
