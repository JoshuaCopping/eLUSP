# 5d JOIN ALL SCENARIOS WITH MAX WIND SCENARIO ######################
source("./Code/0_setup.R")

# Load
# Max wind data
load(here("outputs", "scenario areas", "scenarios_areas_max.RData")) 
scenarios_areas_max <- scenarios_areas %>% select(-soil)
scenarios_change_areas_max <- scenarios_change_areas

# Original scenario data
load(here("outputs", "scenario areas", "scenarios_areas_all.RData")) 
load(here("outputs", "scenario areas", "scenarios_change_areas_all.RData")) 

# JOIN
scenarios_areas_all <- rbind(scenarios_areas, scenarios_areas_max)
scenarios_change_areas_all <- rbind(scenarios_change_areas, scenarios_change_areas_max)

# Rename
scenarios_areas <- scenarios_areas_all
scenarios_change_areas <- scenarios_change_areas_all

# Save
save(scenarios_areas, file = here("outputs", "scenario areas", "scenarios_areas_all_max.RData"))
save(scenarios_change_areas, file = here("outputs", "scenario areas", "scenarios_change_areas_all_max.RData"))

