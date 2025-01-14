### EQI Project Analysis Script

# Load libraries
library(sf)
library(DT)
library(readr)
library(dplyr)
library(crosstalk)
library(mapview)

# Set path to EQI spatial data (can be RDS file in the tool folder or just geodatabase)
EQI <- read_rds("EQI.rds") %>%
  st_set_crs(4326) %>%
  st_transform(crs = 3310)

# Read projects from geodatabase (must be dissolved by project name)
projects <- st_read(dsn = "data.gdb", layer = "Projects") %>%
  st_transform(crs = 3310)

# Create 500 ft buffer around projects
line_buff <- st_buffer(projects, 152.4)

# Perform intersection between project buffer and EQI
intersection <- st_intersection(line_buff, EQI) %>%
  st_drop_geometry()

# Calculate EQI screen coverage
project_screens <- intersection %>%
  mutate(TBPPS_IND = ifelse(TBPP_SCREEN == "Yes", 1, 0)) %>%
  mutate(ADS_IND = ifelse(ACCESS_TO_DESTINATIONS_SCREEN == "Yes", 1, 0)) %>%
  mutate(TE_IND = ifelse(TRAFFIC_EXPOSURE_SCREEN == "Yes", 1, 0)) %>%
  mutate(DO_IND = ifelse(DEMOGRAPHIC_OVERLAY_SCREEN == "Yes", 1, 0)) %>%
  group_by(Name) %>%
  summarise(DO_IND = sum(DO_IND),
            TE_IND = sum(TE_IND),
            ADS_IND = sum(ADS_IND),
            TBPPS_IND = sum(TBPPS_IND)) %>%
  mutate(DO_IND = ifelse(DO_IND > 0, "Yes", "No")) %>%
  mutate(TE_IND = ifelse(TE_IND > 0, "Yes", "No")) %>%
  mutate(ADS_IND = ifelse(ADS_IND > 0, "Yes", "No")) %>%
  mutate(TBPPS_IND = ifelse(TBPPS_IND > 0, "Yes", "No"))

# Calculate summary statistics
project_summary_stats <- intersection %>%
  group_by(Name) %>%
  summarise(tot_pop = sum(POP20, na.rm = T),
            
            #MHI
            mhi = weighted.mean(median_hh_income, POP20, na.rm = T),
            mhi_min = min(median_hh_income, na.rm = T),
            mhi_max = max(median_hh_income, na.rm = T),
            
            #Crash
            crash_pct = weighted.mean(crash_percentile_local, POP20, na.rm = T),
            crash_pct_min = min(crash_percentile_local, na.rm = T),
            crash_pct_max = max(crash_percentile_local, na.rm = T),
            
            #Traffic
            traffic_prox_vol = weighted.mean(traffic_proximity_and_volume_pe, POP20, na.rm = T),
            traffic_prox_vol_min = min(traffic_proximity_and_volume_pe, na.rm = T),
            traffic_prox_vol_max = max(traffic_proximity_and_volume_pe, na.rm = T),
            
            #Ped
            ped_ratio = weighted.mean(PED_RATIO, POP20, na.rm = T),
            ped_ratio_min = min(PED_RATIO, na.rm = T),
            ped_ratio_max = max(PED_RATIO, na.rm = T),
            
            #Bike
            bike_ratio = weighted.mean(BIKE_RATIO, POP20, na.rm = T),
            bike_ratio_min = min(BIKE_RATIO, na.rm = T),
            bike_ratio_max = max(BIKE_RATIO, na.rm = T),
            
            #Transit
            transit_ratio = weighted.mean(TRANSIT_RATIO_POIs, POP20, na.rm = T),
            transit_ratio_min = min(TRANSIT_RATIO_POIs, na.rm = T),
            transit_ratio_max = max(TRANSIT_RATIO_POIs, na.rm = T))

# Merge into data frame
project_df <- merge(project_screens,
                    project_summary_stats,
                    by = "Name",
                    all = T)
