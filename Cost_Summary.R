library(dplyr)
library(sf)
library(mapview)

projects <- read.csv("/Users/S152973/Downloads/EQI_Lookback_Cost/reactive_safety_projects.csv")

costs <- read.csv("/Users/S152973/Downloads/EQI_Lookback_Cost/reactive_safety_costs.csv") %>%
  filter(Activity.Category == "Reactive Safety") %>%
  group_by(ID) %>%
  summarise(Total_Cost = (sum(Total_Cost, na.rm = T) * 1000),
            RW_Cost = (sum(RW_Cost, na.rm = T) * 1000),
            Const_Cost = (sum(Const_Cost, na.rm = T) * 1000),
            Support_Cost = (sum(Support_Cost, na.rm = T) * 1000))

project_df <- merge(projects,
                    costs,
                    by.x = "Project_ID",
                    by.y = "ID",
                    all.x = T)

write.csv(project_df, "/Users/S152973/Downloads/EQI_Lookback_Cost/reactive_safety_analysis_v2.csv")
