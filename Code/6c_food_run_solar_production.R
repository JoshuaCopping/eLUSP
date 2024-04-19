# 6c FOOD PRODUCTION MODEL WITH SOLAR PRODUCTION - RUN ################################
source("./Code/0_setup.R")

## LOAD DATA ---------------------------------------------
# Load food function and data
load(here("data", "food_params_2022.RData"))
load(here("data", "food_fun_2022.RData"))

# Load scenario areas
load(here("outputs", "scenario areas", "scenarios_areas_all_max.RData"))
gc()

# Sum across woodland variables
scenarios_areas <- scenarios_areas[!lcm %chin% c("b.t", "b.w", "c.w", "c.w_pinewood", "c.l", "f.w", "a.h_palud", "i.g_palud", "r.k", "a.h_energy", "i.g_energy",
                                                 "wind_c.w", "wind_a.h_energy", "wind_a.h_palud", "wind_i.g_energy", "wind_i.g_palud")
                                   , keyby = .(scenario, year, country, lcm, nuts1, alc)
                                   , .(area = sum(area))]; gc()

## GET SCENARIO PROPERTIES ---------------------------------------------
# Ambition combinations
ambition_combos <- read_csv("data/ambition_combinations_LUSP.csv", skip = 0)

# Scenario params
scenario_params <- read_csv("data/scenario_parameters_LUSP.csv", skip = 1)


## ORGANIC ADJUSTMENTS ---------------------------------------------
# Add identifier for organic land
scenarios_areas <- scenarios_areas[, organic := lcm %chin% c("a.h_organic", "i.g_organic", "a.h_organic_silvoa", "i.g_organic_silvop", "wind_a.h_organic", "wind_i.g_organic", "wind_a.h_organic_silvoa", "wind_i.g_organic_silvop")] 


## AGROFORESTRY ADJUSTMENTS ---------------------------------------------
# Identify silvoarable type - fruit or poplar
food_params$.silvoarable_type <- "poplar"
# food_params$.silvoarable_type <- "apples"

# Agroforestry footprint
agroforestry_footprint <- read_csv(here("data", "agroforestry_footprint.csv"), skip = 1) %>% 
  select(-notes) %>% 
  filter(type != paste0("Silvoarable agroforestry (", food_params$.silvoarable_type, ")")) %>% 
  select(-type)

# Yield penalty (shading from silvoarable) and area penalty (equivalent to area removed from production)
# Join scenario areas with agroforestry footprint
scenarios_areas <- merge(scenarios_areas, as.data.table(agroforestry_footprint), all.x = TRUE, by = "lcm")
# Multiplier to 1 if NA
scenarios_areas <- scenarios_areas[, ':='(yield_multiplier = ifelse(is.na(yield_multiplier), 1, yield_multiplier),
                                          area_multiplier = ifelse(is.na(area_multiplier), 1, area_multiplier))] 


## WIND TURBINE FOOTPRINT ---------------------------------------------
# Update scenario areas, area multiplyer to account for % taken out of production for turbine footprint - value from wind_footprint in 1_setup


scenarios_areas <- scenarios_areas %>% 
  mutate(area_multiplier = case_when(str_detect(lcm, "wind") ~ 1-wind_footprint,
                                     TRUE ~ area_multiplier)) 

# unique(scenarios_areas$area_multiplier)

## UPDATE SOLAR ---------------------------------------------
# Update scenario areas, area multiplyer to account for % taken out of production for solar footprint - value from solar_footprint in 1_setup
# Then convert solar to n.g lcm class

scenarios_areas <- scenarios_areas %>% 
  mutate(area_multiplier = case_when(lcm == "solar" ~ 1-solar_footprint,
                                     TRUE ~ area_multiplier),
         lcm = case_when(lcm == "solar" ~ "n.g",
                         TRUE ~ lcm)) 


## HEDGE ADJUSTMENTS ---------------------------------------------
# Baseline hedge length
hedge_length <- read_csv(here("data", "cs_hedge_length.csv"), skip = 1) %>% 
  select(-notes) %>% 
  summarise(length = sum(length))

# Calculate cumulative area of new hedge
hedge_areas <- ambition_combos %>%
  select(name, scenario, ambition_hedge) %>% 
  rename(ambition = ambition_hedge) %>% 
  left_join(scenario_params %>%
              filter(grepl("hedge", par) & par != "hedge_stopyear") %>% 
              rename('frac' = val) %>% 
              select(frac, ambition) %>% 
              left_join(scenario_params %>%
                          filter(par == "hedge_stopyear") %>% 
                          select(ambition, val) %>% 
                          rename('stopyear' = val), 
                        by = "ambition"),
            by = "ambition") %>% 
  # Cross with baseline hedge length
  crossing(hedge_length) %>% 
  # Cross with all years 2020-2100
  crossing(year = 2015:2050) %>% 
  # Calculate additional km per 5-year period
  mutate(add = ifelse(year < stopyear & year >= 2020, length * frac / 5, 0)) %>%
  # Calculate cumulative ha of new hedge
  group_by(name, scenario) %>% 
  mutate(ha_removed = cumsum(add * 0.0015 * 100)) %>% # 0.0015 km wide (1.5m), 100 ha per km2 
  ungroup() %>% 
  select(-ambition, -frac, -stopyear, -length) 

# Update hedge_areas with eLUSP scenario names/codes 
# Create new tibble with LUSP & eLUSP scenario names
elusp_scenarios <- tibble(scenario_full = unique(scenarios_areas$scenario),
                          scenario_letter = str_sub(scenario_full, 1,1),
                          scenario_lusp = case_when(scenario_letter == "a" ~ 0,
                                                    scenario_letter == "b" ~ 1,
                                                    scenario_letter == "c" ~ 2,
                                                    scenario_letter == "d" ~ 3,
                                                    scenario_letter == "e" ~ 4,
                                                    scenario_letter == "f" ~ 5,
                                                    scenario_letter == "g" ~ 6,
                                                    scenario_letter == "h" ~ 7,
                                                    scenario_letter == "i" ~ 8))

# Join hedge_areas with eLUSP scenarios and tidy
hedge_areas <- left_join(hedge_areas, elusp_scenarios, by = c("scenario" = "scenario_lusp")) %>% 
  select(name, scenario_full, scenario_letter, year, add, ha_removed) %>% 
  rename(scenario = scenario_full)

# Reduce area of i.g and a.h
# Join scenario areas with hedge areas 
scenarios_areas <- merge(scenarios_areas, as.data.table(hedge_areas), all.x = TRUE, by = c("scenario", "year"))
# Calculate proportion of a.h and i.g removed in each scenario year
scenarios_areas <- scenarios_areas[, c("prop_a.h_removed", "prop_i.g_removed") := .(ha_removed/2/sum(area[lcm %in% c("a.h", "a.h_organic", "wind_a.h", "wind_a.h_organic")]), 
                                                                                    ha_removed/2/sum(area[lcm %in% c("i.g", "i.g_organic", "wind_i.g", "wind_i.g_organic")]))
                                   , by = .(year, scenario)
                                   # Update areas
][, area := case_when(lcm %chin% c("a.h", "a.h_organic", "wind_a.h", "wind_a.h_organic") ~ area * (1 - prop_a.h_removed),
                      lcm %chin% c("i.g", "i.g_organic", "wind_i.g", "wind_i.g_organic") ~ area * (1 - prop_i.g_removed),
                      TRUE ~ area)
  # Removed unnecessary columns
][, c("add", "ha_removed", "prop_a.h_removed", "prop_i.g_removed") := NULL]


## IMPROVED PRACTICES ---------------------------------------------
# Read low carbon farming measures
lowcarbonfarming <- read_csv(here("data", "lowcarbonfarming.csv"), skip = 1)

# Tidy legume crop params
min_legumes <- lowcarbonfarming %>% 
  filter(measure == "legume_crops") %>% 
  select(-sruc_code, -scope, -impact) %>% 
  gather(ambition, prop, -measure, -start_year, -rollout_period) %>% 
  mutate(ambition = gsub("uptake_", "", ambition)) %>% 
  crossing(year = 2015:2050) %>% 
  # Applied to 50% of farmland
  mutate(min_legumes = 0.5 * case_when(year < start_year ~ 0, 
                                       year >= start_year & year < start_year + rollout_period ~ (year - start_year + 1) * prop / rollout_period,
                                       TRUE ~ prop)) %>% 
  left_join(ambition_combos %>% 
              select(scenario, ambition_lowcarbonfarming) %>% 
              rename(ambition = ambition_lowcarbonfarming),
            by = "ambition") %>% 
  select(-start_year, -rollout_period, -measure, -prop, -ambition) %>% 
  inner_join(elusp_scenarios, by = c("scenario" = "scenario_lusp")) %>% 
  select(-scenario, -scenario_letter) %>% 
  rename(scenario = scenario_full)

# Add to food_params
food_params$.min_legumes <- min_legumes

# Calculate n (0.0625-ha pixels) from area (ha)
scenarios_areas <- scenarios_areas[, n := area * 16]


## UPDATE LCM WIND NAMES FOR FOOD_PRODUCTION_FUN ---------------------------------------------
# removes wind from start - area of turbine footprint already accounted for
scenarios_areas <- scenarios_areas %>% 
  mutate(lcm = case_when(str_detect(lcm, "wind") ~ str_sub(lcm, 6),
                         TRUE ~ lcm))

## RUN - BASIC FOOD PRODUCTION ---------------------------------------------
# Grab all combinations of scenario * year
food_results_basic_solar <- scenarios_areas[, .N, keyby = c("scenario", "year")
][, N := NULL] %>% 
  as_tibble() %>%
  mutate(i = 1:nrow(.)) %>%
  group_by(i) %>%
  nest() %>% 
  # Map food_production_fun across unique scenario-years
  mutate(food = purrr::map(data, ~food_production_fun(x = scenarios_areas[scenario == .$scenario & year == .$year], 
                                                      params = food_params, 
                                                      waste = FALSE))) %>% 
  ungroup()

# Save
save(food_results_basic_solar, file = here("outputs", "food_results_basic_all_solar.RData"))

