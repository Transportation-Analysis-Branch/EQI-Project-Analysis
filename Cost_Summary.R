library(dplyr)
library(sf)
library(mapview)

projects <- read.csv("/Users/S152973/Downloads/EQI_Lookback_Cost/active_transportation_projects.csv")

costs <- read.csv("/Users/S152973/Downloads/EQI_Lookback_Cost/active_transportation_costs.csv") %>%
  filter(Activity.Category == "Bicycle and Pedestrian Infrastructure") %>%
  group_by(ID) %>%
  summarise(Total_Cost = (sum(Total_Cost, na.rm = T) * 1000))

project_df <- merge(projects,
                    costs,
                    by.x = "Project_ID",
                    by.y = "ID",
                    all.x = T)

write.csv(project_df, "/Users/S152973/Downloads/EQI_Lookback_Cost/active_transportation_analysis.csv")
