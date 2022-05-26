#Libraries
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(nlme)) install.packages('nlme')
library(nlme)
if (!require(MuMIn)) install.packages('MuMIn')
library(MuMIn)
if (!require(sjPlot)) install.packages('sjPlot')
library(sjPlot)

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
  rename(site = Site)

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
                              site=="CFL2" ~ "reference"))
mmDat$siteType <- factor(mmDat$siteType, 
                             levels = c("reference",
                                        "inlet",
                                        "nearshore",
                                        "offshore"))

#=======================================================
# Visualizing the Explanatory Variables
offshore = rgb(165, 170, 130, max = 255, alpha = 255)
offshore_poly = rgb(180, 184, 151, max = 255, alpha = 150)
nearshore = rgb(54, 139, 133, max = 255, alpha = 255)
nearshore_poly = rgb(54, 139, 133, max = 255, alpha = 100)
inlet = rgb(70, 70, 96, max = 255, alpha = 255)
inlet_poly = rgb(70, 70, 96, max = 255, alpha = 100)
reference = "gray70"
reference_poly = rgb(153,153,153, max = 255, alpha = 70)

windows(height = 8, width = 13)
par(mfrow = c(2,2))
#Sediment Porosity
boxplot(mmDat$SRP_ugL ~ mmDat$site, las = 2,
        col = c(reference_poly, reference_poly, 
                reference_poly, reference_poly,
                inlet_poly, nearshore_poly, offshore_poly, 
                inlet_poly, inlet_poly, 
                nearshore_poly, offshore_poly),
        border = c(reference, reference, reference, reference,
                   inlet, nearshore, offshore,
                   inlet, inlet, nearshore, offshore),
        ylim = c(0,20), pch = 20,
        ylab = "Sediment Porosity", xlab = "",
        names = NA)

#phycocyanin
boxplot(log10(mmDat$chl) ~ mmDat$site, las = 2,
        col = c(reference_poly, reference_poly, 
                reference_poly, reference_poly,
                inlet_poly, nearshore_poly, offshore_poly, 
                inlet_poly, inlet_poly, 
                nearshore_poly, offshore_poly),
        border = c(reference, reference, reference, reference,
                   inlet, nearshore, offshore,
                   inlet, inlet, nearshore, offshore),
        ylim = c(2.5,4.5), pch = 20,
        ylab = "", xlab = "", yaxt = "n",
        names = NA)
legend("topleft", 
       legend = c("reference", "inlet", "near", "farther"), 
       col = c(reference, inlet, nearshore, offshore), 
       pch = 15, pt.cex = 2)
axis(side = 2, at = c(log10(500), log10(600), log10(700), log10(800), log10(900), log10(1000), log10(2000), log10(3000), log10(4000), log10(5000), log10(6000), log10(7000), log10(8000), log10(9000), log10(10000), log10(20000), log10(30000)),
     labels = c("","","","","","1000","","","","","","","","","10,000","",""))

#Organic Matter in sediment
boxplot(mmDat$orgmatter ~ mmDat$site, las = 2,
        col = c(reference_poly, reference_poly, 
                reference_poly, reference_poly,
                inlet_poly, nearshore_poly, offshore_poly, 
                inlet_poly, inlet_poly, 
                nearshore_poly, offshore_poly),
        border = c(reference, reference, reference, reference,
                   inlet, nearshore, offshore,
                   inlet, inlet, nearshore, offshore),
        ylim = c(0,25), pch = 20,
        ylab = "Sediment Organic Matter (%)", xlab = "",
        names = NA)

#Dissolved Oxygen - Bottom
boxplot(mmDat$tss ~ mmDat$site, las = 2,
        col = c(reference_poly, reference_poly, 
                reference_poly, reference_poly,
                inlet_poly, nearshore_poly, offshore_poly, 
                inlet_poly, inlet_poly, 
                nearshore_poly, offshore_poly),
        border = c(reference, reference, reference, reference,
                   inlet, nearshore, offshore,
                   inlet, inlet, nearshore, offshore),
        ylim = c(0,30), pch = 20,
        ylab = "Suspended Solids (mg/L)", xlab = "",
        names = NA)
# abline(100,0, lty = 3)

#=======================================================
# TESTING FOR THE EFFECT OF SITE TYPE ON CONCENTRATIONS
# Mixed Models with Autocorrelation
#Surface Methane Concentrations
surfaceModelDat = mmDat %>%
  mutate(water_scale = scale(ch4_lake_surface)) %>%
  group_by(site) %>% arrange(doy, .by_group=TRUE) %>% ungroup()

waterSite_lme = nlme::lme(ch4_lake_surface ~ siteType,
                          random = ~ 1 | site,
                          correlation = corAR1(),
                          data = surfaceModelDat, method = 'REML')
summary(waterSite_lme)
MuMIn::r.squaredGLMM(waterSite_lme)
sjPlot::plot_model(waterSite_lme)

#Pore Water
sedModelDat = mmDat %>%
  filter(!(is.na(mean_porewater))) %>%
  mutate(pore_scale = scale(mean_porewater)) %>%
  group_by(site) %>% arrange(doy, .by_group=TRUE) %>% ungroup()

poreSite_lme = nlme::lme(pore_scale ~ siteType,
                          random = ~ 1 | site,
                          correlation = corAR1(),
                          data = sedModelDat, method = 'REML')
summary(poreSite_lme)
MuMIn::r.squaredGLMM(poreSite_lme)
sjPlot::plot_model(poreSite_lme)

#Methanogenesis Assay
assayModelDat = mmDat %>%
  filter(!(is.na(mean_rate))) %>%
  mutate(rate_scale = scale(mean_rate)) %>%
  group_by(site) %>% arrange(doy, .by_group=TRUE) %>% ungroup()

assay_ch4_lme = nlme::lme(rate_scale ~ siteType,
                          random = ~ 1 | site,
                          correlation = corAR1(),
                          data = assayModelDat, method = 'REML')
summary(assay_ch4_lme)
MuMIn::r.squaredGLMM(assay_ch4_lme)
sjPlot::plot_model(assay_ch4_lme)

#Make data frames of the siteType model outputs
surface_lme = coef(summary(waterSite_lme)) %>% 
  as.data.frame() %>% rownames_to_column(var="fixed.effect")
pore_lme = coef(summary(poreSite_lme)) %>% 
  as.data.frame() %>% rownames_to_column(var="fixed.effect")
methano_lme = coef(summary(assay_ch4_lme)) %>% 
  as.data.frame() %>% rownames_to_column(var="fixed.effect")
lme_output = rbind(surface_lme, pore_lme, methano_lme) %>%
  filter(!fixed.effect=="(Intercept)")
lme_output$measure = c("surface", "surface", "surface", 
                       "pore", "pore", "pore", 
                       "methano", "methano", "methano")

plot(lme_output$Value, c(9,8,7,6,5,4,3,2,1),
     pch = 19, cex = 2, col = c(inlet, nearshore, offshore),
     yaxt = "n", ylab = "", xlab = "Fixed Effect Compared to Reference", 
     xlim = c(-1,4))
lines(c(0,0), c(-10,10), lty = 3)
arrows(lme_output$Value - (lme_output$Std.Error * sqrt(lme_output$DF)), 
                           c(9,8,7,6,5,4,3,2,1),
       x1 = lme_output$Value + (lme_output$Std.Error * sqrt(lme_output$DF)), 
       y1 = c(9,8,7,6,5,4,3,2,1),
       angle = 0, length = 0, lwd = 3, col = c(inlet, nearshore, offshore))

#=======================================
# EXPLAINING
modelDat = mmDat %>%
  # filter(!(is.na(bottom_SpCond))) %>%
  filter(!(is.na(bulk_density))) %>%
  filter(!(is.na(SRP_ugL))) %>%
  # filter(ch4_lake_surface>0) %>%
  # filter(bottom_depth<1.9) %>%
  group_by(site) %>% arrange(doy, .by_group=TRUE) %>% ungroup()
water_ch4_lme = nlme::lme(ch4_lake_surface ~ 
                            orgmatter + bulk_density +
                            SRP_ugL,
                            # bottom_temp
                            # doy +
                            # bottom_depth,
                            # bottom_SpCond +
                            # vss,
                            # log10(chl),
                            # log10(pc),
                            # bottom_DO_sat,
                            # bulk_density +
                            # orgmatter,
                            # rate_scale,
                          random = ~ 1 | site,
                          correlation = corAR1(),
                          data = modelDat, method = 'REML')
summary(water_ch4_lme)
MuMIn::r.squaredGLMM(water_ch4_lme)
sjPlot::plot_model(water_ch4_lme)
sjPlot::tab_model(water_ch4_lme,
                  pred.labels = c("(Intercept)",
                                  "Porosity",
                                  "log10(Sediment conc)",
                                  "Orthophosphate"),
                  show.re.var= TRUE, 
                  dv.labels= "Effects on Surface Water Methane")

# Sediment Pore Water Explanatory Model
sedModelDat = mmDat %>%
  filter(!(is.na(ch4_porewater))) %>%
  filter(!(is.na(bottom_SpCond))) %>%
group_by(site) %>% arrange(doy, .by_group=TRUE) %>% ungroup()

sed_ch4_lme = nlme::lme(ch4_porewater ~ 
                          bulk_density +
                          bottom_SpCond +
                          iss,
                          random = ~ 1 | site,
                          correlation = corAR1(),
                          data = sedModelDat, method = 'REML')
summary(sed_ch4_lme)
MuMIn::r.squaredGLMM(sed_ch4_lme)
sjPlot::plot_model(sed_ch4_lme)

sjPlot::tab_model(sed_ch4_lme,
                  pred.labels = c("(Intercept)",
                                  "Bulk Density",
                                  "Sp. Conductivity",
                                  "Inorg. Susp. Solids"),
                  show.re.var= TRUE, 
                  dv.labels= "Effects on Sediment Methane")

#Methanogenesis Potential Explanatory Model
assayModelDat = mmDat %>%
  filter(!(is.na(mean_rate))) %>%
  filter(!(is.na(SRP_ugL))) %>%
  filter(!(is.na(bulk_density))) %>%
  group_by(site) %>% arrange(doy, .by_group=TRUE) %>% ungroup()

assay_ch4_lme = nlme::lme(log10(mean_rate) ~ 
                            vss *
                            log10(pc) +
                            porosity,
                        random = ~ 1 | site,
                        correlation = corAR1(),
                        data = assayModelDat, method = 'REML')
summary(assay_ch4_lme)
MuMIn::r.squaredGLMM(assay_ch4_lme)
sjPlot::plot_model(assay_ch4_lme)

sjPlot::tab_model(assay_ch4_lme,
                  # pred.labels = c("(Intercept)", 
                  #                 "Volatile Susp. Solids", 
                  #                 "Phycocyanin", 
                  #                 "Porosity", 
                  #                 "VSS x Phyco"),
                  show.re.var= TRUE, 
                  dv.labels= "Effects on Methanogenesis")

