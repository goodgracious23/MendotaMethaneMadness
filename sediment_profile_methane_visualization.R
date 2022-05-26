if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

pore_water = read.csv("core_methane_analysis.csv")

sum_pore = pore_water %>%
  group_by(site, sedDepth) %>%
  summarize(mean_pore_ch4 = mean(ch4_porewater, na.rm = TRUE)/10,
            sd_pore_ch4 = sd(ch4_porewater, na.rm = TRUE)/10/sqrt(10)) %>%
  ungroup() %>%
  filter(!sedDepth==20) 

sum_pore = as.data.frame(sum_pore)
sum_pore$low_sd = (sum_pore$mean_pore_ch4 - sum_pore$sd_pore_ch4)
sum_pore$high_sd = (sum_pore$mean_pore_ch4 + sum_pore$sd_pore_ch4)

sum_pore = sum_pore %>%
  mutate(low_sd = case_when(is.na(.$low_sd) ~ 
                              (mean_pore_ch4-10), 
                              TRUE ~ low_sd),
         high_sd = case_when(is.na(.$high_sd) ~ 
                               (mean_pore_ch4+10), 
                            TRUE ~ high_sd))

#============================================
offshore = rgb(165, 170, 130, max = 255, alpha = 255)
offshore_poly = rgb(180, 184, 151, max = 255, alpha = 150)
nearshore = rgb(54, 139, 133, max = 255, alpha = 255)
nearshore_poly = rgb(54, 139, 133, max = 255, alpha = 100)
inlet = rgb(70, 70, 96, max = 255, alpha = 255)
inlet_poly = rgb(70, 70, 96, max = 255, alpha = 100)
reference = "white"
reference_poly = rgb(153,153,153, max = 255, alpha = 70)

#Yahara Sediment Profiles
plot(sum_pore[sum_pore$site=="G1", "mean_pore_ch4"],
     sum_pore[sum_pore$site=="G1", "sedDepth"], 
     type = "o", pch = 19, lwd = 2, col = reference,
     ylim = c(16, 0), 
     xlim = c(0, max(sum_pore$mean_pore_ch4, na.rm = TRUE)),
     xlab = expression(CH[4]~Pore~Water~"("*mu*M*")"),
     ylab = "Depth in the Sediment (cm)")

polygon(c(sum_pore[sum_pore$site=="G1", "high_sd"],
          rev(sum_pore[sum_pore$site=="G1", "low_sd"])),
        c(sum_pore[sum_pore$site=="G1", "sedDepth"],
          rev(sum_pore[sum_pore$site=="G1", "sedDepth"])), 
        border = NA, col = reference_poly)
polygon(c(sum_pore[sum_pore$site=="G2", "high_sd"],
          rev(sum_pore[sum_pore$site=="G2", "low_sd"])),
        c(sum_pore[sum_pore$site=="G2", "sedDepth"],
          rev(sum_pore[sum_pore$site=="G2", "sedDepth"])), 
        border = NA, col = reference_poly)
polygon(c(sum_pore[sum_pore$site=="Y0", "high_sd"],
          rev(sum_pore[sum_pore$site=="Y0", "low_sd"])),
        c(sum_pore[sum_pore$site=="Y0", "sedDepth"],
          rev(sum_pore[sum_pore$site=="Y0", "sedDepth"])), 
        border = NA, col = inlet_poly)
polygon(c(sum_pore[sum_pore$site=="S0", "high_sd"],
          rev(sum_pore[sum_pore$site=="S0", "low_sd"])),
        c(sum_pore[sum_pore$site=="S0", "sedDepth"],
          rev(sum_pore[sum_pore$site=="S0", "sedDepth"])), 
        border = NA, col = inlet_poly)
polygon(c(sum_pore[sum_pore$site=="Y1", "high_sd"],
          rev(sum_pore[sum_pore$site=="Y1", "low_sd"])),
        c(sum_pore[sum_pore$site=="Y1", "sedDepth"],
          rev(sum_pore[sum_pore$site=="Y1", "sedDepth"])), 
        border = NA, col = nearshore_poly)
polygon(c(sum_pore[sum_pore$site=="Y2", "high_sd"],
          rev(sum_pore[sum_pore$site=="Y2", "low_sd"])),
        c(sum_pore[sum_pore$site=="Y2", "sedDepth"],
          rev(sum_pore[sum_pore$site=="Y2", "sedDepth"])), 
        border = NA, col = offshore_poly)
points(sum_pore[sum_pore$site=="G1", "mean_pore_ch4"],
       sum_pore[sum_pore$site=="G1", "sedDepth"], 
       type = "o", pch = 19, lwd = 2, col = reference)
points(sum_pore[sum_pore$site=="G2", "mean_pore_ch4"],
       sum_pore[sum_pore$site=="G2", "sedDepth"], 
       type = "o", pch = 19, lwd = 2, col = reference)
points(sum_pore[sum_pore$site=="Y2", "mean_pore_ch4"],
       sum_pore[sum_pore$site=="Y2", "sedDepth"], 
       type = "o", pch = 19, lwd = 2, col = offshore)
points(sum_pore[sum_pore$site=="Y1", "mean_pore_ch4"],
       sum_pore[sum_pore$site=="Y1", "sedDepth"], 
       type = "o", pch = 19, lwd = 2, col = nearshore)
points(sum_pore[sum_pore$site=="Y0", "mean_pore_ch4"],
       sum_pore[sum_pore$site=="Y0", "sedDepth"], 
       type = "o", pch = 19, lwd = 2, col = inlet)
points(sum_pore[sum_pore$site=="S0", "mean_pore_ch4"],
       sum_pore[sum_pore$site=="S0", "sedDepth"], 
       type = "o", pch = 19, lwd = 2, col = inlet)

#Pheasant Branch Sediment Profiles
plot(sum_pore[sum_pore$site=="CFL1", "mean_pore_ch4"],
     sum_pore[sum_pore$site=="CFL1", "sedDepth"], 
     type = "o", pch = 19, lwd = 2, col = reference,
     ylim = c(16, 0), 
     xlim = c(0, max(sum_pore$mean_pore_ch4+10, na.rm = TRUE)),
     xlab = expression(CH[4]~Pore~Water~"("*mu*M*")"),
     ylab = "Depth in the Sediment (cm)")

polygon(c(sum_pore[sum_pore$site=="CFL1", "high_sd"],
          rev(sum_pore[sum_pore$site=="CFL1", "low_sd"])),
        c(sum_pore[sum_pore$site=="CFL1", "sedDepth"],
          rev(sum_pore[sum_pore$site=="CFL1", "sedDepth"])), 
        border = NA, col = reference_poly)
polygon(c(sum_pore[sum_pore$site=="CFL2", "high_sd"],
          rev(sum_pore[sum_pore$site=="CFL2", "low_sd"])),
        c(sum_pore[sum_pore$site=="CFL2", "sedDepth"],
          rev(sum_pore[sum_pore$site=="CFL2", "sedDepth"])), 
        border = NA, col = reference_poly)
polygon(c(sum_pore[sum_pore$site=="PB0", "high_sd"],
          rev(sum_pore[sum_pore$site=="PB0", "low_sd"])),
        c(sum_pore[sum_pore$site=="PB0", "sedDepth"],
          rev(sum_pore[sum_pore$site=="PB0", "sedDepth"])), 
        border = NA, col = inlet_poly)
polygon(c(sum_pore[sum_pore$site=="PB1", "high_sd"],
          rev(sum_pore[sum_pore$site=="PB1", "low_sd"])),
        c(sum_pore[sum_pore$site=="PB1", "sedDepth"],
          rev(sum_pore[sum_pore$site=="PB1", "sedDepth"])), 
        border = NA, col = nearshore_poly)
polygon(c(sum_pore[sum_pore$site=="PB2", "high_sd"],
          rev(sum_pore[sum_pore$site=="PB2", "low_sd"])),
        c(sum_pore[sum_pore$site=="PB2", "sedDepth"],
          rev(sum_pore[sum_pore$site=="PB2", "sedDepth"])), 
        border = NA, col = offshore_poly)

points(sum_pore[sum_pore$site=="CFL1", "mean_pore_ch4"],
       sum_pore[sum_pore$site=="CFL1", "sedDepth"],
       type = "o", pch = 19, lwd = 2, col = reference)
points(sum_pore[sum_pore$site=="CFL2", "mean_pore_ch4"],
       sum_pore[sum_pore$site=="CFL2", "sedDepth"],
       type = "o", pch = 19, lwd = 2, col = reference)
points(sum_pore[sum_pore$site=="PB2", "mean_pore_ch4"],
       sum_pore[sum_pore$site=="PB2", "sedDepth"], 
       type = "o", pch = 19, lwd = 2, col = offshore)
points(sum_pore[sum_pore$site=="PB1", "mean_pore_ch4"],
       sum_pore[sum_pore$site=="PB1", "sedDepth"], 
       type = "o", pch = 19, lwd = 2, col = nearshore)
points(sum_pore[sum_pore$site=="PB0", "mean_pore_ch4"],
       sum_pore[sum_pore$site=="PB0", "sedDepth"], 
       type = "o", pch = 19, lwd = 2, col = inlet)

