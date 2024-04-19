# 7a GHG PARAMS EDIT ######################
source("./Code/0_setup.R")


# UPDATE GHG LUC  ---------------------------------------------
# GWP conversion factors
gwp100 <- read_csv(here("data", "gwp100.csv"), skip = 1) %>% filter(notes == "AR4") %>% select(-notes)

## SOIL----
# Read soil stock change params
luc_soil <- read_csv(here("data", "ghg_luc_soil.csv"), skip = 1) %>% select(-source, -units) 

# Update luc soil with forest - settlement
luc_soil_extra <- tibble(
  from = "Forestland",
  to = "Settlement",
  country = unique(luc_soil$country),
  soil_c = c(104, 236, 124, 244),
  n2o = unique(luc_soil$n2o),
  time = 100
)

# Bind luc_soil and new fores - settlement
luc_soil <- rbind(luc_soil, luc_soil_extra)

# LCM crosswalk
lcm_crosswalk <- tibble(from_to = c("Forestland", "Forestland", "Cropland", "Settlement", "Grassland", "Grassland", "Grassland", "Grassland", "Grassland", "Grassland", "Grassland", "Miscanthus", "SRC", "SRF"),
                        lcm = c("b.w", "c.w", "a.h", "b.t", "i.g", "n.g", "c.g", "a.g", "h.r", "b.g", "f.s", "a.h_energy_misc", "a.h_energy_src", "i.g_energy"))

# Calculate annual flux to/from soil following LUC
gwp_luc_soil <- luc_soil %>%
  # Convert C to CO2
  mutate(gwp_t = soil_c * 44/12,
         ghg = "CO2") %>% 
  select(-soil_c, -n2o) %>% 
  # Join with LCM crosswalk
  left_join(lcm_crosswalk, by = c("from" = "from_to")) %>% 
  rename(lcm_from = lcm) %>% 
  left_join(lcm_crosswalk, by = c("to" = "from_to")) %>% 
  rename(lcm_to = lcm) %>% 
  select(lcm_from, lcm_to, country, gwp_t, time, ghg) %>% 
  # Cross with all years
  crossing(year = 0:80) %>%
  # Calculate annual flux
  group_by(country, lcm_from, lcm_to) %>% 
  mutate(stock = gwp_t * (1 - exp(year / (time / log(0.01)))),
         gwp_t = stock - lag(stock),
         gwp_t = ifelse(is.na(gwp_t), 0, gwp_t)) %>%
  ungroup() %>% 
  select(-stock, -time)

# Add N2O (mineralised due to loss of SOC. = 0 if SOC gain)
gwp_luc_soil <- gwp_luc_soil %>%
  mutate(n2o = ifelse(gwp_t < 0, 0, gwp_t * unique(luc_soil$n2o)),
         ghg = "N2O") %>% 
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = n2o * gwp100) %>% 
  select(-n2o, -gwp100) %>% 
  bind_rows(gwp_luc_soil, .)


## SAVE ----
# Load existing data to get biomass table 
load(here("data", "ghg_pars_luc.RData"))
gwp_luc_biomass <- ghg_pars_luc$gwp_luc_biomass

ghg_pars_luc <- list(gwp_luc_soil = gwp_luc_soil, 
                     gwp_luc_biomass = gwp_luc_biomass)

save(ghg_pars_luc, file = here("data", "ghg_pars_luc.RData"))


# UPDATE LUC DATA WITH WIND & SOLAR LAND CLASSES  ---------------------------------------------
# LUC
load(here("data", "ghg_pars_luc.RData"))

gwp_luc_renewables_soil <- ghg_pars_luc$gwp_luc_soil %>% 
  filter(lcm_from %in% c("a.h", "c.w", "i.g") & lcm_to == "b.t" | lcm_from == "a.h" & lcm_to == "i.g") %>% 
  mutate(lcm_to = case_when(lcm_from == "a.h" & lcm_to == "i.g" ~ "solar",
                            lcm_from %in% c("a.h", "c.w", "i.g") & lcm_to == "b.t" ~ "wind"),
         gwp_t = case_when(lcm_to == "wind" ~ gwp_t*wind_footprint,
                           lcm_to == "solar" ~ gwp_t*(1-solar_footprint)))

# gwp_luc_renewables_soil %>% filter(lcm_to == "solar")
# unique(gwp_luc_renewables_soil$lcm_from)

ghg_pars_luc[["gwp_luc_renewables_soil"]] <- gwp_luc_renewables_soil

save(ghg_pars_luc, file = here("data", "ghg_pars_luc.RData"))


