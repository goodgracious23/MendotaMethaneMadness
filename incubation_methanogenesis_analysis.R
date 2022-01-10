#METHANOGENESIS ANALYSIS - Mendota Methane Madness
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

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

#Check that things are related to each other somewhat, check for outliers
plot(sed_phys$porosity ~ sed_phys$bulk_density)
bd_por_lm = lm(sed_phys$porosity ~ sed_phys$bulk_density)
summary(bd_por_lm)
abline(bd_por_lm)

#============================
#Subset incubation data and munge
ghg = read.csv("mendota_ch4_ghg_2021.csv")
inc_raw = ghg %>%
  filter(type == "Incubation") %>%
  select(-analysis_date, -export, -GC_batch_id, -GC_sample_id, -sedDepth, -rep,
         -CO2_FID, -CO2_TCD, -CH4_TCD, -N2O_ECD)