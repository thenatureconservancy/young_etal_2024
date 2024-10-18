
## Calculate weighted means per county for Wildfire Hazard Potential (WFHP, https://www.firelab.org/project/wildfire-hazard-potential), Forest to Faucets (Surface water importance, IMP and Wildfire potential to important surface water supplies, WFP attributes) Maps
# input data was created in ArcGIS pro by 1) creating a raster of the SVI data (https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html), then performing a combine of the resultant counties raster and other inputs (WFHP, IMP and WFP)  This gives a count of pixels per WFHP/IMP/WFP category per county. To make maps these counts need to be weighted.  

# test calculating weighted means on 7 test counties in Texas

library(tidyverse)
library(stringr)


#test with 7 counties in Texas
test_data  <- read_csv("data/tester_counties.csv")
View(test_data)

# value will be "Value_1", weighting variable will be "Count", group by GEOID

test_data <- test_data %>%
  mutate(Value_1 = na_if(Value_1, 6)) %>%
  mutate(Value_1 = na_if(Value_1, 7)) %>%
  group_by(GEOID) %>%
  mutate(weighted_wfhpNA = weighted.mean(Value_1, Count, na.rm = TRUE))

#write.csv(test_data, file = "to_join.csv")

# try with CONUS wide data
us_cntys_wfhp <- read_csv("data/us_cntys_wfhp.csv")
View(us_cntys_wfhp)

us_cntys_wfhp <- us_cntys_wfhp  %>%
  mutate(whp2020_cls_conus = na_if(whp2020_cls_conus, 6)) %>%
  mutate(whp2020_cls_conus = na_if(whp2020_cls_conus, 7)) %>%
  group_by(GEOID) %>%
  mutate(weighted_wfhp = weighted.mean(whp2020_cls_conus, Count, na.rm = TRUE)) 

us_cntys_wfhp$GEOID <- str_pad(us_cntys_wfhp$GEOID, width = 5, side = "left", pad = "0")

write.csv(us_cntys_wfhp, row.names = FALSE, file = "data/wfhp_data_to_join_US.csv")


# weighted means with forest to faucets data.  get weighted mean of WFP per county for joining

f2f_counties_raster <- read_csv("data/f2f_counties_raster.csv")
View(f2f_counties_raster)

f2f_counties_raster2 <- f2f_counties_raster %>%
  group_by(GEOID) %>%
  summarize(county_sum = sum(Count),
  weighted_wfp = weighted.mean(WFP, Count, na.rm = TRUE))
    
f2f_counties_raster2$GEOID <- str_pad(f2f_counties_raster2$GEOID, width = 5, side = "left", pad = "0")

write.csv(f2f_counties_raster2, row.names = FALSE, file = "data/wfp_data_to_join_US.csv")


# weighted means with forest to faucets data, IMP variable to get surface water importance for mapping

imp_cnties <- read_csv("data/imp_cntys.csv")

imp_cnties <- imp_cnties %>%
  group_by(geoid_text) %>%
  mutate(weighted_imp = weighted.mean(IMP_R, Count, na.rm = TRUE))

write.csv(imp_cnties, file = "data/imp_cnties_full.csv") # for exploration

imp_cnties_to_join <- imp_cnties %>%
  group_by(geoid_text) %>%
  summarize(weighted_im = mean(weighted_imp))

write.csv(imp_cnties_to_join, file = 'data/imp_cnties_to_join.csv')
