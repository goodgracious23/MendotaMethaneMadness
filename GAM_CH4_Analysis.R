#Libraries
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(party)) install.packages('party')
library(party)

#All of the data files - clean up the columns
water = read.csv("Lake_Concentrations_Profiles.csv") %>%
  select(-X, -Notes.x, - Notes.y) %>%
  mutate(diff_temp = surface_temp - bottom_temp)
pore = read.csv("core_methane_analysis.csv") %>%
  select(-X, -type, -ch4_ppm, -ch4_sedvol, 
         -pch4_head:-vol_porewater) %>%
  rename(ch4_tot_umol_pore = ch4_tot_umol)
surf_pore = pore %>%
  filter(sedDepth==1)
assay = read.csv("Methanogenesis_Summary.csv") %>%
  select(-X)
sus = read.csv("SuspendedSolids_Mendota.csv") %>%
  select(-X, -filter_mass:-ashed_mass)
pig = read.csv("Sum_Pig_Mendota.csv") %>%
  select(-X, -group)
srp = read.csv('SRP_Manual_MendotaMethaneMadness_2021.csv') %>%
  rename(site = Site) %>%
  select(-Date, -Abs)

#Let's have a data party - join everything together
join1 = left_join(water, surf_pore, by = c("site", "doy"))
join2 = left_join(join1, assay, by = c("site", "doy"))
join3 = left_join(join2, sus, by = c("site", "doy"))
join4 = left_join(join3, pig, by = c("site", "doy")) 
mmDat = left_join(join4, srp, by = c("site", "doy")) %>%
  filter(!(doy == 139)) %>%
  mutate(siteType = case_when(site=="Y0" ~ "inlet",
                              site=="S0" ~ "inlet",
                              site=="Y1" ~ "nearshore", 
                              site=="Y2" ~ "offshore",
                              site=="PB0" ~ "inlet",
                              site=="PB1" ~ "nearshore", 
                              site=="PB2" ~ "offshore",
                              site=="G1" ~ "reference",
                              site=="G2" ~ "reference",
                              site=="CFL1" ~ "reference",
                              site=="CFL2" ~ "reference")) %>%
  mutate(siteType2 = case_when(siteType=="inlet" ~ "inlet",
                              siteType=="nearshore" ~ "delta",
                              siteType=="offshore" ~ "delta", 
                              siteType=="reference" ~ "reference"))
mmDat$siteType <- factor(mmDat$siteType, 
                         levels = c("reference",
                                    "inlet",
                                    "nearshore",
                                    "offshore"))
mmDat$siteType2 <- factor(mmDat$siteType2, 
                         levels = c("reference",
                                    "delta",
                                    "inlet"))
mmDat$site <- factor(mmDat$site, 
                         levels = c("S0", "Y0", "Y1", "Y2",
                                    "PB0", "PB1", "PB2",
                                    "G1", "G2", "CFL1", "CFL2"))

# Visualizing the Explanatory Variables
offshore = rgb(165, 170, 130, max = 255, alpha = 255)
offshore_poly = rgb(180, 184, 151, max = 255, alpha = 150)
nearshore = rgb(54, 139, 133, max = 255, alpha = 255)
nearshore_poly = rgb(54, 139, 133, max = 255, alpha = 100)
inlet = rgb(70, 70, 96, max = 255, alpha = 255)
inlet_poly = rgb(70, 70, 96, max = 255, alpha = 100)
reference = "gray70"
reference_poly = rgb(153,153,153, max = 255, alpha = 70)

#=================================
# WATER COLUMN CH4
# Conditional Inference Trees
cforest_waterCH4 <- cforest(ch4_lake_surface ~ 
                              doy + bottom_depth +
                              porosity + bulk_density + orgmatter +
                              ch4_porewater + 
                              surface_temp + bottom_temp +
                              surface_DO_mgL + surface_DO_sat +
                              bottom_DO_mgL +  bottom_DO_sat +
                              bottom_SpCond +
                              mean_rate + t1_rate + t2_rate +
                              tss + vss + iss +
                              chl + pc + SRP_ugL,
                          data = mmDat,
                          controls = cforest_control(
                            ntree = 50000, mincriterion = 0.9))


#Identify the important variables and make barplot of ranks
varimp_waterCH4<-varimp(cforest_waterCH4)
barplot(abs(varimp_waterCH4[order(abs(varimp_waterCH4), 
                                  decreasing=T)]), las=2)

#Most important variables for water column concentration:
# orgmatter
# bulk_density
# SRP_ugL
# t1_rate

#Using the Important Variables, let's construct some GAMs
waterCH4_gam <- gam(ch4_lake_surface ~ 
                      s(orgmatter, by = siteType2, k = 7) +
                      s(SRP_ugL, by = siteType2) +
                      s(log(mean_rate), by = siteType2, k = 7),
                    data = mmDat, na.action = "na.omit",
                    method = "ML")

summary(waterCH4_gam)
gam.check(waterCH4_gam)
# plot(waterCH4_gam, pages = 1)

#Plot the partial effects
windows(height=3, width=9)
par(mfrow=c(1,3), mai=c(0.5,0.5,0,0), omi=c(0.1,0.6,0.6,0.1))

plot(waterCH4_gam, select = 3, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, ylab = "", xlab = "",
     shift = coef(waterCH4_gam)[1], ylim = c(-1,8), cex.axis = 1.5,
     shade.col = inlet_poly, pch = 19, col = inlet, lwd = 3)
mtext(side = 2, line = 3, "CH4 in Water")
plot(waterCH4_gam, select = 6, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, ylab = "", xlab = "",
     shift = coef(waterCH4_gam)[1], ylim = c(-1,8), cex.axis = 1.5,
     shade.col = inlet_poly, pch = 19, col = inlet, lwd = 3)
plot(waterCH4_gam, select = 9, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, ylab = "", xlab = "",
     shift = coef(waterCH4_gam)[1], cex.axis = 1.5,
     shade.col = inlet_poly, pch = 19, col = inlet, lwd = 3)

plot(waterCH4_gam, select = 2, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE,
     shift = coef(waterCH4_gam)[1], 
     ylab = "", xlab = "", ylim = c(-1,8), cex.axis = 1.5,
     shade.col = nearshore_poly, pch = 19, col = nearshore, lwd = 3)
mtext(side = 2, line = 3, "CH4 in Water")
plot(waterCH4_gam, select = 5, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, ylab = "", xlab = "",
     shift = coef(waterCH4_gam)[1],ylim = c(-1,8),cex.axis = 1.5,
     shade.col = nearshore_poly, pch = 19, col = nearshore, lwd = 3)
plot(waterCH4_gam, select = 8, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, ylab = "", xlab = "",
     shift = coef(waterCH4_gam)[1],ylim = c(-1,8),cex.axis = 1.5,
     shade.col = nearshore_poly, pch = 19, col = nearshore, lwd = 3)

plot(waterCH4_gam, select = 1, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shift = coef(waterCH4_gam)[1],
     shade = TRUE, rug = FALSE,
     xlab = "", ylab = "", ylim = c(-5,10), cex.axis = 1.5,
     shade.col = reference_poly, pch = 19, col = "gray70", lwd = 3)
mtext(side = 2, line = 3, "CH4 in Water")
plot(waterCH4_gam, select = 4, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, ylab = "", xlab = "",
     shift = coef(waterCH4_gam)[1], ylim = c(-5,10),cex.axis = 1.5,
     shade.col = reference_poly, pch = 19, col = reference, lwd = 3)
plot(waterCH4_gam, select = 7, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, 
     ylab = "", xlab = "",cex.axis = 1.5,
     shift = coef(waterCH4_gam)[1],ylim = c(-5,10),
     shade.col = reference_poly, pch = 19, col = reference, lwd = 3)


#=========================================
mmSed = mmDat %>%
  filter(!(is.na(mean_rate)))
cforest_assayCH4 <- cforest(log(mean_rate) ~ 
                              # siteNumeric + siteTypeNumeric +
                              doy + bottom_depth +
                              porosity + bulk_density + orgmatter +
                              surface_temp + bottom_temp +
                              surface_DO_mgL + surface_DO_sat +
                              bottom_DO_mgL +  bottom_DO_sat +
                              bottom_SpCond +
                              tss + vss + iss +
                              chl + pc + SRP_ugL,
                            data = mmSed,
                            controls = cforest_control(
                              ntree = 50000, mincriterion = 0.9))


#Identify the important variables and make barplot of ranks
varimp_assayCH4<-varimp(cforest_assayCH4)
barplot(abs(varimp_assayCH4[order(abs(varimp_assayCH4), 
                                decreasing = T)]), las=2)

assayCH4_gam <- gam(mean_rate ~ 
                      s(orgmatter, by = siteType2) +
                      s(SRP_ugL, by = siteType2) +
                      s(vss, by = siteType2),
                    data = mmSed, na.action = "na.omit")
summary(assayCH4_gam)
gam.check(assayCH4_gam)
plot(assayCH4_gam, pages = 1)


windows(height=3, width=9)
par(mfrow=c(1,3), mai=c(0.5,0.5,0,0), omi=c(0.1,0.6,0.6,0.1))

plot(assayCH4_gam, select = 3, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, ylab = "", xlab = "",
     shift = coef(assayCH4_gam)[1], 
     ylim = c(-0.1,0.1), cex.axis = 1.5,
     shade.col = inlet_poly, pch = 19, col = inlet, lwd = 3)
mtext(side = 2, line = 3, "Methanogenesis\nPotential")
plot(assayCH4_gam, select = 6, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, ylab = "", xlab = "",
     shift = coef(assayCH4_gam)[1], 
     ylim = c(-0.2,0.1), cex.axis = 1.5,
     shade.col = inlet_poly, pch = 19, col = inlet, lwd = 3)
plot(assayCH4_gam, select = 9, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, ylab = "", xlab = "",
     shift = coef(assayCH4_gam)[1], cex.axis = 1.5,
     # ylim = c(-1,2),
     shade.col = inlet_poly, pch = 19, col = inlet, lwd = 3)

plot(assayCH4_gam, select = 2, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE,
     shift = coef(assayCH4_gam)[1], 
     ylab = "", xlab = "", 
     ylim = c(-0.02,0.02), cex.axis = 1.5,
     shade.col = nearshore_poly, pch = 19, col = nearshore, lwd = 3)
mtext(side = 2, line = 3, "Methanogenesis\nPotential")
plot(assayCH4_gam, select = 5, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, ylab = "", xlab = "",
     shift = coef(assayCH4_gam)[1],
     ylim = c(-0.02,0.02),cex.axis = 1.5,
     shade.col = nearshore_poly, pch = 19, col = nearshore, lwd = 3)
plot(assayCH4_gam, select = 8, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, ylab = "", xlab = "",
     shift = coef(assayCH4_gam)[1],
     ylim = c(-0.02,0.02),cex.axis = 1.5,
     shade.col = nearshore_poly, pch = 19, col = nearshore, lwd = 3)

plot(assayCH4_gam, select = 1, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shift = coef(assayCH4_gam)[1],
     shade = TRUE, rug = FALSE,
     xlab = "", ylab = "", ylim = c(-0.02,0.05), cex.axis = 1.5,
     shade.col = reference_poly, pch = 19, col = "gray70", lwd = 3)
mtext(side = 2, line = 3, "Methanogenesis\nPotential")
plot(assayCH4_gam, select = 4, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, ylab = "", xlab = "",
     shift = coef(assayCH4_gam)[1], 
     ylim = c(-0.02,0.04),cex.axis = 1.5,
     shade.col = reference_poly, pch = 19, col = reference, lwd = 3)
plot(assayCH4_gam, select = 7, 
     se = TRUE, residuals = TRUE, all.terms = TRUE,
     shade = TRUE, rug = FALSE, 
     ylab = "", xlab = "",cex.axis = 1.5,
     shift = coef(assayCH4_gam)[1],ylim = c(-0.02,0.04),
     shade.col = reference_poly, pch = 19, col = reference, lwd = 3)




#=========================================
mmSed = mmDat %>%
  filter(!(is.na(ch4_porewater)))
cforest_poreCH4 <- cforest(log10(ch4_porewater) ~ 
                              # siteNumeric + siteTypeNumeric +
                              doy + bottom_depth +
                              porosity + bulk_density + orgmatter +
                              surface_temp + bottom_temp +
                              surface_DO_mgL + surface_DO_sat +
                              bottom_DO_mgL +  bottom_DO_sat +
                              bottom_SpCond +
                              tss + vss + iss +
                              chl + pc + SRP_ugL,
                            data = mmSed,
                            controls = cforest_control(
                              ntree = 50000, mincriterion = 0.9))


#Identify the important variables and make barplot of ranks
varimp_poreCH4<-varimp(cforest_poreCH4)
barplot(abs(varimp_poreCH4[order(abs(varimp_poreCH4), 
                                  decreasing = T)]), las=2)

sedCH4_gam <- gam(log10(ch4_porewater) ~ 
                      s(bottom_SpCond, by = siteType2) +
                      s(porosity, by = siteType2) +
                      s(doy, by = siteType2),
                    data = mmSed, na.action = "na.omit")
summary(sedCH4_gam)
gam.check(sedCH4_gam)
plot(sedCH4_gam, pages = 1)



