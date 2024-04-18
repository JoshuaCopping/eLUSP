# 6b MERGE GET AREAS DATA ################################
source("./code/1_setup.R")

# Merge get areas data into single file

# SCENARIOS AREAS ---------------------------------------------
## LOAD DATA ----
# Load all files and rename with country 
load(here("outputs", "scenario areas", "scenarios_areas_nir_high.RData")) 
scenarios_areas_nir_high <- scenarios_areas

load(here("outputs", "scenario areas", "scenarios_areas_nir_low.RData")) 
scenarios_areas_nir_low <- scenarios_areas

load(here("outputs", "scenario areas", "scenarios_areas_wal_high.RData")) 
scenarios_areas_wal_high <- scenarios_areas

load(here("outputs", "scenario areas", "scenarios_areas_wal_low.RData")) 
scenarios_areas_wal_low <- scenarios_areas

load(here("outputs", "scenario areas", "scenarios_areas_sct_high.RData")) 
scenarios_areas_sct_high <- scenarios_areas

load(here("outputs", "scenario areas", "scenarios_areas_sct_low.RData")) 
scenarios_areas_sct_low <- scenarios_areas

load(here("outputs", "scenario areas", "scenarios_areas_eng_high.RData")) 
scenarios_areas_eng_high <- scenarios_areas

load(here("outputs", "scenario areas", "scenarios_areas_eng_low.RData")) 
scenarios_areas_eng_low <- scenarios_areas

rm(scenarios_areas, scenarios_change_areas); gc()


## COMBINE HIGH/LOW PER COUNTRY ----
# NIR
scenarios_areas_list <- list(scenarios_areas_nir_high, scenarios_areas_nir_low)
rm(scenarios_areas_nir_high, scenarios_areas_nir_low); gc()

scenarios_areas_nir <- rbindlist(scenarios_areas_list, use.names = TRUE)
rm(scenarios_areas_list); gc()

save(scenarios_areas_nir, file = here("outputs", "scenario areas", "scenarios_areas_nir.RData"))

# WAL
scenarios_areas_list <- list(scenarios_areas_wal_high, scenarios_areas_wal_low)
rm(scenarios_areas_wal_high, scenarios_areas_wal_low); gc()

scenarios_areas_wal <- rbindlist(scenarios_areas_list, use.names = TRUE)
rm(scenarios_areas_list); gc()

save(scenarios_areas_wal, file = here("outputs", "scenario areas", "scenarios_areas_wal.RData"))

# SCT
scenarios_areas_list <- list(scenarios_areas_sct_high, scenarios_areas_sct_low)
rm(scenarios_areas_sct_high, scenarios_areas_sct_low); gc()

scenarios_areas_sct <- rbindlist(scenarios_areas_list, use.names = TRUE)
rm(scenarios_areas_list); gc()

save(scenarios_areas_sct, file = here("outputs", "scenario areas", "scenarios_areas_sct.RData"))

# ENG
scenarios_areas_list <- list(scenarios_areas_eng_high, scenarios_areas_eng_low)
rm(scenarios_areas_eng_high, scenarios_areas_eng_low); gc()

scenarios_areas_eng <- rbindlist(scenarios_areas_list, use.names = TRUE)
rm(scenarios_areas_list); gc()

save(scenarios_areas_eng, file = here("outputs", "scenario areas", "scenarios_areas_eng.RData"))


## TIDY DATA & BIND INTO 1 BIG FILE ----
# Load data 
load(here("outputs", "scenario areas", "scenarios_areas_nir.RData")) 
load(here("outputs", "scenario areas", "scenarios_areas_wal.RData")) 
load(here("outputs", "scenario areas", "scenarios_areas_sct.RData")) 
load(here("outputs", "scenario areas", "scenarios_areas_eng.RData")) 

# Merge London and SE and trim data
# ENG
scenarios_areas_eng <- scenarios_areas_eng[, nuts1 := ifelse(nuts1 %chin% c("South East (England)", "London"), "South East & London (England)", nuts1)
][, keyby = .(country, year, nuts1, scenario, lcm, alc, peat, yc_sbi, yc_sok, yc_sp, yc_ss, natregen)
  , .(area = sum(area))]; gc()

# Bind all
scenarios_areas_list <- list(scenarios_areas_eng, scenarios_areas_sct, scenarios_areas_nir, scenarios_areas_wal)
rm(scenarios_areas_eng, scenarios_areas_nir, scenarios_areas_sct, scenarios_areas_wal); gc()

scenarios_areas_elusp <- rbindlist(scenarios_areas_list, use.names = TRUE)
rm(scenarios_areas_list); gc()

## SAVE 
save(scenarios_areas_elusp, file = here("outputs", "scenario areas", "scenarios_areas_elusp.RData"))


######################################################################
# SCENARIOS CHANGE AREAS ---------------------------------------------
## LOAD DATA ----
# Load all files and rename with country 
load(here("outputs", "scenario areas", "scenarios_areas_nir_high.RData")) 
scenarios_change_areas_nir_high <- scenarios_change_areas

load(here("outputs", "scenario areas", "scenarios_areas_nir_low.RData")) 
scenarios_change_areas_nir_low <- scenarios_change_areas

load(here("outputs", "scenario areas", "scenarios_areas_wal_high.RData")) 
scenarios_change_areas_wal_high <- scenarios_change_areas

load(here("outputs", "scenario areas", "scenarios_areas_wal_low.RData")) 
scenarios_change_areas_wal_low <- scenarios_change_areas

load(here("outputs", "scenario areas", "scenarios_areas_sct_high.RData")) 
scenarios_change_areas_sct_high <- scenarios_change_areas

load(here("outputs", "scenario areas", "scenarios_areas_sct_low.RData")) 
scenarios_change_areas_sct_low <- scenarios_change_areas

load(here("outputs", "scenario areas", "scenarios_areas_eng_high.RData")) 
scenarios_change_areas_eng_high <- scenarios_change_areas

load(here("outputs", "scenario areas", "scenarios_areas_eng_low.RData")) 
scenarios_change_areas_eng_low <- scenarios_change_areas

rm(scenarios_areas, scenarios_change_areas); gc()


## TIDY DATA & BIND INTO 1 BIG FILE ----
# Bind all
scenarios_change_areas_list <- list(scenarios_change_areas_nir_high, scenarios_change_areas_nir_low, scenarios_change_areas_wal_high, scenarios_change_areas_wal_low, scenarios_change_areas_sct_high, scenarios_change_areas_sct_low, scenarios_change_areas_eng_high, scenarios_change_areas_eng_low)
scenarios_change_areas_elusp <- rbindlist(scenarios_change_areas_list, use.names = TRUE)

rm(scenarios_change_areas_list, scenarios_change_areas_nir_high, scenarios_change_areas_nir_low, scenarios_change_areas_wal_high, scenarios_change_areas_wal_low, scenarios_change_areas_sct_high, scenarios_change_areas_sct_low, scenarios_change_areas_eng_high, scenarios_change_areas_eng_low); gc()


## SAVE
save(scenarios_change_areas_elusp, file = here("outputs", "scenario areas", "scenarios_change_areas_elusp.RData"))


######################################################################
# JOIN LUSP DATA ---------------------------------------------

## LOAD AND TIDY --------
# Load data  
load(here("data", "scenarios_areas_2022.RData"))
load(here("outputs", "scenario areas", "scenarios_areas_elusp.RData"))
load(here("outputs", "scenario areas", "scenarios_change_areas_elusp.RData"))

# Merge London and SE and trim data for LUSP
scenarios_areas <- scenarios_areas[, nuts1 := ifelse(nuts1 %chin% c("South East (England)", "London"), "South East & London (England)", nuts1)
][, keyby = .(country, year, nuts1, scenario, lcm, alc, peat, yc_sbi, yc_sok, yc_sp, yc_ss, natregen)
  , .(area = sum(area))]; gc()

# Rename LUSP scenarios to match eLUSP & remove wader scenarios
scenarios_areas <- scenarios_areas %>% 
  filter(!str_detect(scenario, "w")) %>% 
  mutate(scenario = case_when(scenario == 0 ~ "a",
                              scenario == 1 ~ "b",
                              scenario == 2 ~ "c",
                              scenario == 3 ~ "d",
                              scenario == 4 ~ "e",
                              scenario == 5 ~ "f",
                              scenario == 6 ~ "g",
                              scenario == 7 ~ "h",
                              scenario == 8 ~ "i"))

# Update order of columns in both scenarios change areas, to match & remove wader scenarios
scenarios_change_areas_elusp <- scenarios_change_areas_elusp %>% 
  select(scenario, year, lcm_from, lcm_to, country, peat:area)

scenarios_change_areas <- scenarios_change_areas %>% 
  filter(!str_detect(scenario, "w")) %>% 
  mutate(scenario = case_when(scenario == 0 ~ "a",
                              scenario == 1 ~ "b",
                              scenario == 2 ~ "c",
                              scenario == 3 ~ "d",
                              scenario == 4 ~ "e",
                              scenario == 5 ~ "f",
                              scenario == 6 ~ "g",
                              scenario == 7 ~ "h",
                              scenario == 8 ~ "i")) %>% 
  select(-name, -woodop, -waders)



## BIND DATA --------
# Bind eLUSP to LUSP data 
# Scenarios areas
scenarios_list <- list(scenarios_areas, scenarios_areas_elusp)
rm(scenarios_areas, scenarios_areas_elusp); gc()
scenarios_areas <- rbindlist(scenarios_list)
rm(scenarios_list); gc()
save(scenarios_areas, file = here("outputs", "scenario areas", "scenarios_areas_all.RData"))

# Scenarios change areas
scenarios_list <- list(scenarios_change_areas, scenarios_change_areas_elusp)
rm(scenarios_change_areas, scenarios_change_areas_elusp); gc()
scenarios_change_areas <- rbindlist(scenarios_list)
rm(scenarios_list); gc()
save(scenarios_change_areas, file = here("outputs", "scenario areas", "scenarios_change_areas_all.RData"))


## CHECKING CORRECT BINDS ETC ----

unique(scenarios_areas$scenario)

scenarios_areas %>% 
  group_by(country, scenario, year) %>% 
  summarise(area = sum(area)) 


unique(scenarios_change_areas$lcm_from)

scenarios_change_areas %>% 
  group_by(country, scenario, year) %>% 
  summarise(area = sum(area)) 


