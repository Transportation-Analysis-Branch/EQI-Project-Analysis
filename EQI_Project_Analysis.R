### EQI Tool (local)

# Load libraries
library(sf)
library(DT)
library(readr)
library(dplyr)
library(crosstalk)
library(mapview)

# Project Unique ID (or name) should be "Name" in the input spatial data


# Set path to EQI spatial data (can be RDS file in the tool folder or just geodatabase)
EQI <- read_rds("path to EQI spatial data") %>%
  st_set_crs(4326) %>%
  st_transform(crs = 3310)

# Read projects from geodatabase (must be dissolved by project name)
projects <- st_read(dsn = "path to projects GDB", layer = "Projects feature class") %>%
  st_transform(crs = 3310)

# Create 500 ft buffer around projects
line_buff <- st_buffer(projects, 152.4)

# Calculate area of buffer
line_buff$area1 <- st_area(line_buff)

# Perform intersection between project buffer and EQI
intersection <- st_intersection(line_buff, EQI)

# Calculate area of intersected buffer
intersection$area2 <- st_area(intersection)

intersection <- intersection %>%
  st_drop_geometry()

# Calculate % screen coverage
coverage_stats <- intersection %>%
  mutate(coverage_ratio = as.numeric(area2 / area1)) %>%
  group_by(Name) %>%
  summarise(do_pct = sum(coverage_ratio[DEMOGRAPHIC_OVERLAY_SCREEN == "Yes"], na.rm = T),
            te_pct = sum(coverage_ratio[TRAFFIC_EXPOSURE_SCREEN == "Yes"], na.rm = T),
            ads_pct = sum(coverage_ratio[ACCESS_TO_DESTINATIONS_SCREEN == "Yes"], na.rm = T),
            tbpp_pct = sum(coverage_ratio[TBPP_SCREEN == "Yes"], na.rm = T)) %>%
  mutate(do_ind = ifelse(do_pct > 0, "Yes", "No")) %>%
  mutate(te_ind = ifelse(te_pct > 0, "Yes", "No")) %>%
  mutate(ads_ind = ifelse(ads_pct > 0, "Yes", "No")) %>%
  mutate(tbpp_ind = ifelse(tbpp_pct > 0, "Yes", "No"))

# Calculate summary statistics
project_summary_stats <- intersection %>%
  
  # Estimate population within intersected area using proportional allocation
  mutate(coverage_ratio = as.numeric(area2 / area1)) %>%
  mutate(new_pop = POP20 * coverage_ratio) %>%
  group_by(Name) %>%
  summarise(tot_pop_est = sum(new_pop, na.rm = T),
            
            #MHI
            mhi = weighted.mean(median_hh_income, new_pop, na.rm = T),
            mhi_min = min(median_hh_income, na.rm = T),
            mhi_max = max(median_hh_income, na.rm = T),
            
            #Crash
            crash_pct = weighted.mean(crash_percentile_local, new_pop, na.rm = T),
            crash_pct_min = min(crash_percentile_local, na.rm = T),
            crash_pct_max = max(crash_percentile_local, na.rm = T),
            
            #Traffic
            traffic_prox_vol = weighted.mean(traffic_proximity_and_volume_pe, new_pop, na.rm = T),
            traffic_prox_vol_min = min(traffic_proximity_and_volume_pe, na.rm = T),
            traffic_prox_vol_max = max(traffic_proximity_and_volume_pe, na.rm = T),
            
            #Ped
            ped_ratio = weighted.mean(PED_RATIO, new_pop, na.rm = T),
            ped_ratio_min = min(PED_RATIO, na.rm = T),
            ped_ratio_max = max(PED_RATIO, na.rm = T),
            
            #Bike
            bike_ratio = weighted.mean(BIKE_RATIO, new_pop, na.rm = T),
            bike_ratio_min = min(BIKE_RATIO, na.rm = T),
            bike_ratio_max = max(BIKE_RATIO, na.rm = T),
            
            #Transit
            transit_ratio = weighted.mean(TRANSIT_RATIO_POIs, new_pop, na.rm = T),
            transit_ratio_min = min(TRANSIT_RATIO_POIs, na.rm = T),
            transit_ratio_max = max(TRANSIT_RATIO_POIs, na.rm = T))

# Merge into data frame
project_df <- merge(coverage_stats,
                    project_summary_stats,
                    by = "Name",
                    all = T)
