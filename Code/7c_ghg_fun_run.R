# 7c GHG FUN RUN ######################
source("./Code/0_setup.R")

# LOAD DATA ---------------------------------------------
# GHG function and params
load(file = here("data", "ghg_pars_funs.RData"))

# Food results 
load(here("outputs", "food_results_basic_all.RData")) # Raw food results

# Ambition combinations
ambition_combos <- read_csv("data/ambition_combinations_LUSP.csv", skip = 0) %>%
  mutate(scenario = case_when(scenario == 0 ~ "a",
                              scenario == 1 ~ "b",
                              scenario == 2 ~ "c",
                              scenario == 3 ~ "d",
                              scenario == 4 ~ "e",
                              scenario == 5 ~ "f",
                              scenario == 6 ~ "g",
                              scenario == 7 ~ "h",
                              scenario == 8 ~ "i"))

# Scenario params
scenario_params <- read_csv("data/scenario_parameters_LUSP.csv", skip = 1)


# ADDS MODEL TO FUNCTION TO FILTER BY HIGH/LOW AMBITION ---------------------------------------------
#energy_ambition <- "low"

ghg_fun_run <- function(energy_ambition){
  
  print(paste("Start -", Sys.time()))

  ## LOAD & TIDY SCENARIOS AREAS ----
  print("Loading & tidying data")
  load(here("outputs", "scenario areas", "scenarios_areas_all_max.RData")) 
  scenarios_areas <- scenarios_areas[, keyby = .(country, year, peat, yc_sbi, yc_sok, yc_sp, yc_ss, natregen, lcm, nuts1, alc, scenario)
                                     , .(area = sum(area))]; gc()
  
  load(here("outputs", "scenario areas", "scenarios_change_areas_all_max.RData")) 
  scenarios_change_areas <- scenarios_change_areas[, keyby = .(country, year, peat, yc_sbi, yc_sok, yc_sp, yc_ss, natregen, lcm_from, lcm_to, nuts1, alc, scenario)
                                                   , .(area = sum(area))]; gc()
  
  ## Update scenarios areas with LUSP scenario grouping column
  scenarios_areas <- scenarios_areas[, lusp_scenario := substr(scenario, 0, 1)]
  scenarios_change_areas <- scenarios_change_areas[, lusp_scenario := substr(scenario, 0, 1)]
  
  ## Filter by high or low energy ambition
  if(energy_ambition == "high"){
    # High
    print("High energy ambition level")
    scenarios_areas <- scenarios_areas[str_detect(scenario, "high")]
    scenarios_change_areas <- scenarios_change_areas[str_detect(scenario, "high")]
    energy_ambition <- "high"
  } else {
    # Low - syntax: doesn't contain high - to include LUSP scenarios too
    print("Low energy ambition level")
    scenarios_areas <- scenarios_areas[!str_detect(scenario, "high")]
    scenarios_change_areas <- scenarios_change_areas[!str_detect(scenario, "high")]
  }
  
  # Pull out scenario names
  scenario_names <- as_tibble(scenarios_areas) %>% select(scenario, lusp_scenario) %>% unique 
  
  ## RUN FUNCTION ----
  gwp_combined <- do_ghg_fun(scenarios_areas, scenarios_change_areas,
                          scenario_params, ambition_combos,
                          ghg_pars_luc,
                          ghg_pars_ag, food_results_basic, pop, manure_systems, lowcarbonfarming,
                          ghg_pars_peat,
                          ghg_pars_intertidal,
                          ghg_pars_bioenergy,
                          ghg_pars_wood, 
                          ghg_pars_agroforestry,
                          ghg_pars_hedges,
                          energy_ambition,
                          scenario_names)
  
  print(paste("Finshed -", Sys.time()))
  
  return(gwp_combined)
  
}


# RUN FUNCTION ---------------------------------------------
# High energy ambition
gwp_combined_high <- ghg_fun_run("high")
save(gwp_combined_high, file = here("outputs", "gwp_combined_high.RData"))

rm(gwp_combined_high)
gc()


# Low energy ambition
gwp_combined_low <- ghg_fun_run("low")
save(gwp_combined_low, file = here("outputs", "gwp_combined_low.RData"))

rm(gwp_combined_low)
gc()


# Combine high & low outputs
# Load
load(here("outputs", "gwp_combined_high.RData"))
load(here("outputs", "gwp_combined_low.RData"))

# Bind high and low
gwp_combined <- rbind(gwp_combined_high$gwp_combined, gwp_combined_low$gwp_combined)
hwp_combined <- rbind(gwp_combined_high$hwp_combined, gwp_combined_low$hwp_combined)

# List GWP & HWP
gwp_combined <- list(gwp_combined = gwp_combined, 
                     hwp_combined = hwp_combined)

# Save
save(gwp_combined, file = here("outputs", "gwp_combined.RData"))

