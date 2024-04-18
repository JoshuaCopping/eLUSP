# 9d BIRD MODEL RUN MAX WIND ######################
source("./code/1_setup.R")


# SCENARIO/AMBITION LEVELS ---------------------------------------------
# Ambition combinations 
ambition_combos <- read_csv(here("data", "ambition_combinations_LUSP.csv"), skip = 0) %>%
  # Change name to elusp verions 
  mutate(scenario = case_when(scenario == 0 ~ "a",
                              scenario == 1 ~ "b",
                              scenario == 2 ~ "c",
                              scenario == 3 ~ "d",
                              scenario == 4 ~ "e",
                              scenario == 5 ~ "f",
                              scenario == 6 ~ "g",
                              scenario == 7 ~ "h",
                              scenario == 8 ~ "i")) %>% 
  crossing(elusp_prefix = c("high", "low", "np_high", "np_low")) %>% 
  mutate(elusp_scenario = str_c(scenario, elusp_prefix, sep = "_"),
         scenario = elusp_scenario) %>% 
  select(-elusp_prefix, -elusp_scenario) 
  
ambition_combos <- ambition_combos %>%
  rbind(ambition_combos %>% 
          filter(scenario == "i_np_high") %>% 
          mutate(scenario = "i_np_max"))

# Scenario params (for hedge increase)
scenario_params <- read_csv(here("data", "scenario_parameters_LUSP.csv"), skip = 1)

# Get hedge params
hedge_increase <- ambition_combos %>% 
  select(scenario, ambition_hedge) %>% 
  rename(ambition = ambition_hedge) %>%
  left_join(scenario_params %>% 
              filter(par == "hedge") %>% 
              mutate(hedge_increase = 1 + val * 6) %>% 
              select(ambition, hedge_increase), 
            by = "ambition") %>% 
  select(-ambition) %>% 
  mutate(scenario = as.character(scenario)) %>% 
  # Add row for 2015
  add_row(scenario = "2015",
          hedge_increase = 1)


# BIRD DATA ---------------------------------------------
load(here("data", "bbs_model_list_new_2022.RData"))

# Load species list
bbs_species <- read_csv(here("data", "bird_spp.csv"), skip = 1) %>% 
  filter(keep == "yes") %>%
  select(spp, gb_only, bocc4, bocc5, farmland_list, woodland_list, wetland_list, rspb_list)

# Join species traits to model list
bbs_model_list <- bbs_model_list_success %>%
  left_join(bbs_species, by = "spp")


# GET NEWDATA (SCENARIO-SPECIFIC LANDCOVER)  ---------------------------------------------
load(here("outputs", "bbs_scenarios_covars_max_wind.RData"))
bbs_covars_nir_max <- bbs_covars_nir %>% filter(scenario != "2015")
bbs_covars_gbr_max <- bbs_covars_gbr %>% filter(scenario != "2015")

load(here("outputs", "bbs_scenarios_covars_new.RData"))
bbs_covars_nir <- rbind(bbs_covars_nir, bbs_covars_nir_max)
bbs_covars_gbr <- rbind(bbs_covars_gbr, bbs_covars_gbr_max)

load(here("data", "density_multipliers.RData")) 

# Extract newdata (per scenario)
newdata <- bind_rows(bbs_covars_nir %>% 
                       select(scenario, covars) %>% 
                       unnest(cols = covars) %>% 
                       # For NI - if missing hedge data, use national average
                       mutate(hedge_total = ifelse(hedgena_total > 0, 
                                                   mean(hedge_total[hedgena_total == 0]),
                                                   hedge_total)), 
                     bbs_covars_gbr %>% 
                       select(scenario, covars) %>% 
                       unnest(cols = covars)) %>% 
  rename(hedge = hedge_total) %>% 
  select(-hedgena_total) 

# Calculate effective_area (land area)
effective_area <- newdata %>% 
  select(contains("sq")) %>% 
  rowSums(na.rm = TRUE)

# Tidy newdata
newdata <- newdata %>% 
  # Tidy country/island attributes
  mutate(country = ifelse(nuts1 %in% c("Scotland", "Wales", "Northern Ireland"), nuts1, "England"),
         nuts1 = ifelse(nuts1 %in% c("South East (England)", "London"), "South East & London (England)", nuts1)) %>% 
  # Tidy buffer attributes
  mutate(w.d_buf = b.w_buf + c.w_buf) %>% 
  select(-contains("buf"), w.d_buf, b.t_buf, a.h_buf) %>% 
  # Add effective area (total land area)
  mutate(effective_area = effective_area) %>% 
  # Tidy square attributes
  mutate_at(vars(contains("sq")), list(~ifelse(is.na(.), 0, .))) %>% 
  mutate_at(vars(contains("sq")), list(~./effective_area)) %>% 
  mutate(s.g_sq = c.g_sq + n.g_sq + a.g_sq + c.g_new_sq + n.g_new_sq + a.g_new_sq + c.g_woodpa_sq + n.g_woodpa_sq + a.g_woodpa_sq,
         h.r_sq = h.r_sq + h.r_woodpa_sq,
         # Add all wind_a.h... here
         a.h_sq = a.h_sq + a.h_energy_sq + a.h_palud_sq + a.h_silvoa_sq + a.h_organic_sq + a.h_organic_silvoa_sq + wind_a.h_sq + wind_a.h_silvoa_sq + wind_a.h_energy_sq + wind_a.h_organic_sq + wind_a.h_organic_silvoa_sq,
         # Add all wind_i.g... + solar here
         i.g_sq = i.g_sq + i.g_palud_sq + i.g_silvop_sq + i.g_organic_sq + i.g_organic_silvop_sq + wind_i.g_sq + wind_i.g_silvop_sq + wind_i.g_organic_sq + solar_sq,
         c.l_sq = c.l_sq + s.m_sq,
         # Add wind_c.w and wind_i.g_energy (short-rotation forestry) here
         c.w_sq = c.w_sq + c.w_pinewood_sq + i.g_energy_sq + wind_i.g_energy_sq + wind_c.w_sq,
         woodpa_sq = c.g_woodpa_sq + n.g_woodpa_sq + a.g_woodpa_sq + h.r_woodpa_sq,
         # Add all wind on silvoa/silvop here
         silvoa_sq = a.h_silvoa_sq + a.h_organic_silvoa_sq + wind_a.h_silvoa_sq + wind_a.h_organic_silvoa_sq,
         silvop_sq = i.g_silvop_sq + i.g_organic_silvop_sq + wind_i.g_silvop_sq + wind_i.g_organic_silvop_sq,
         # Add all wind on organic here
         a.h_organic_sq = a.h_organic_sq + a.h_organic_silvoa_sq + wind_a.h_organic_sq + wind_a.h_organic_silvoa_sq,
         i.g_organic_sq = i.g_organic_sq + i.g_organic_silvop_sq + wind_i.g_organic_sq + wind_i.g_organic_silvop_sq,
         # Calculate total area of wind/solar
         renewable = solar_sq + wind_a.h_sq + wind_a.h_silvoa_sq + wind_a.h_energy_sq + wind_a.h_organic_sq + wind_a.h_organic_silvoa_sq + wind_i.g_sq + wind_i.g_silvop_sq + wind_i.g_organic_sq) %>% 
  # Drop un-used columns
  select(-c.g_sq, -n.g_sq, -a.g_sq, -c.g_new_sq, -n.g_new_sq, -a.g_new_sq, -c.g_woodpa_sq, -n.g_woodpa_sq, -a.g_woodpa_sq, -h.r_woodpa_sq,
         -a.h_energy_sq, -a.h_palud_sq, -a.h_silvoa_sq, -a.h_organic_silvoa_sq, -wind_a.h_sq, -wind_a.h_organic_sq, -wind_a.h_energy_sq, -wind_a.h_silvoa_sq, -wind_a.h_organic_silvoa_sq,
         -i.g_energy_sq, -i.g_palud_sq, -i.g_silvop_sq, -i.g_organic_silvop_sq, -wind_i.g_sq, -wind_i.g_organic_sq, -wind_i.g_energy_sq, -wind_i.g_silvop_sq, -wind_i.g_organic_silvop_sq,
         -r.k_sq, -c.w_pinewood_sq, -s.m_sq, -solar_sq, -wind_c.w_sq) %>%
  # Log-transform continuous variables
  mutate_at(vars(-x, -y, -elev, -scenario, -country, -nuts1, -effective_area), list(~log(.+0.01)))

# Increase hedge length (on arable and grassland only)
newdata <- newdata %>%
  left_join(hedge_increase, by = "scenario") %>% 
  mutate(hedge_increase = ifelse(is.na(hedge_increase), 1, hedge_increase),
         hedge = exp(hedge) - 0.01,
         hedge = hedge * hedge_increase * (exp(a.h_sq) + exp(i.g_sq) - 0.02),
         hedge = log(hedge + 0.01)) %>% 
  select(-hedge_increase)

# Agroforestry; silvoarable = 33.33km per km2
newdata <- newdata %>% 
  mutate(agfor_sq = exp(woodpa_sq) + exp(silvoa_sq) + exp(silvop_sq) - 0.03,
         hedge = exp(hedge) - 0.01,
         hedge = hedge + 1000/30 * agfor_sq,
         hedge = log(hedge + 0.01)) %>% 
  select(-agfor_sq)

# Tidy multipliers
# Organic
organic_multipliers <- organic_multipliers %>%
  full_join(unique(select(bbs_model_list, spp)), by = "spp") %>% 
  mutate(organic_multiplier = organic_multiplier - 1,
         organic_multiplier = ifelse(is.na(organic_multiplier), 0, organic_multiplier)) 


# FUNCTION ---------------------------------------------
predict_fun <- function(mod, spp, gb_only, organic_multipliers, newdat){
  # Exclude NI if necessary
  if(gb_only == 1){
    newdat. <- filter(newdat, country != "Northern Ireland") 
  } else{
    newdat. <- newdat
  }
  
  # Get predicted population for each square
  fit <- bind_cols(select(newdat., country, scenario, x, y, a.h_sq, i.g_sq, a.h_organic_sq, i.g_organic_sq, renewable),
                   predict(mod, newdat., type = "link", se.fit = TRUE)) %>%  
    mutate(fit = exp(fit)) %>% 
    select(-se.fit) 

  # Adjust fit with 99th percentile
  max <- quantile(fit$fit, 0.99)
  fit <- fit %>%
    mutate(fit = ifelse(fit > max, max, fit))
  
  ## WIND/SOLAR ADJUSTMENT
  fit <- fit %>% 
    # Back-transform renewable area
    mutate(renewable_sq = (exp(renewable) - 0.01)) %>%
    # Subtract X% from of estimate pop size, where X% = fraction of each square which is renewable
    mutate(fit_lo = fit - fit * renewable_sq,
           fit_hi = fit) %>%  # This should be unaffected by renewables, i.e. none scenario/LUSP
    select(-fit)
  
  ## ORGANIC ADJUSTMENT
  fit <- fit %>% 
    # Back-transform organic area
    mutate(organic_sq = (exp(a.h_organic_sq) - 0.01) + (exp(i.g_organic_sq) - 0.01)) %>%
    # Apply organic multiplier to fraction of each square which is a.h_organic
    mutate(fit_lo = fit_lo + fit_lo * organic_sq * filter(organic_multipliers, spp == !!spp)$organic_multiplier,
           fit_hi = fit_hi + fit_hi * organic_sq * filter(organic_multipliers, spp == !!spp)$organic_multiplier)
  
  # Add in NI if necessary
  if(gb_only == 1){
    fit <- fit %>% 
      bind_rows(newdat %>% 
                  filter(country == "Northern Ireland") %>% 
                  select(country, scenario, x, y)) %>% 
      mutate_at(vars(fit_lo, fit_hi), list(~ifelse(is.na(.), 0, .)))
  }
  
  # Summarise (total population, UK and country-specific)
  fit_sum <- bind_rows(fit %>% 
                         group_by(scenario) %>% 
                         summarise(pop_lo = sum(fit_lo), 
                                   pop_hi = sum(fit_hi), 
                                   .groups = "drop") %>% 
                         mutate(pop_lo_rel = pop_lo / pop_lo[scenario == "2015"],
                                pop_hi_rel = pop_hi / pop_hi[scenario == "2015"],
                                country = "UK"),
                       fit %>% 
                         group_by(scenario, country) %>% 
                         summarise(pop_lo = sum(fit_lo), 
                                   pop_hi = sum(fit_hi), 
                                   .groups = "drop") %>% 
                         group_by(country) %>% 
                         mutate(pop_lo_rel = pop_lo / pop_lo[scenario == "2015"],
                                pop_hi_rel = pop_hi / pop_hi[scenario == "2015"]) %>% 
                         ungroup()) %>% 
    filter(pop_hi_rel != 0)
  
  # Plot comparing hi (optimistic) and lo (pessimistic)
  # fit_sum %>%
  #   filter(country == "UK" & scenario != "2015") %>%
  #   select(-pop_lo, -pop_hi) %>%
  #   gather(est, pop, -scenario, -country) %>%
  #   ggplot(aes(x = scenario, y = pop, fill = est)) +
  #   geom_col(position = position_dodge()) +
  #   coord_flip()
  
  # End
  return(fit_sum)
  
}


# RUN  ---------------------------------------------
# Get predictions
bbs_fits_out <- bbs_model_list %>%
  select(-mod, -failed, -summary) %>%
  rowwise() %>% 
  mutate(params_to_map = list(list(topmod, spp, gb_only))) %>%
  ungroup() %>% 
  mutate(fit = purrr::map(params_to_map, ~predict_fun(.x[[1]], 
                                                      spp = .x[[2]], 
                                                      gb_only = .x[[3]], 
                                                      organic_multipliers = organic_multipliers, 
                                                      newdat = newdata)))

save(bbs_fits_out, file = here("outputs", "bird_results_raw_max_wind_all.RData"))


# AVERAGE ACROSS SPECIES  ---------------------------------------------
load(here("outputs", "bird_results_raw_max_wind_all.RData"))

# Get species groups
spp_groups <- bind_rows(bbs_species %>% 
                          select(spp) %>% 
                          mutate(group = "All species"),
                        bbs_species %>%
                          filter(!is.na(farmland_list)) %>%
                          select(spp) %>%
                          mutate(group = "Farmland birds"),
                        bbs_species %>% 
                          filter(farmland_list == "specialist") %>% 
                          select(spp) %>% 
                          mutate(group = "Farmland specialists"),
                        bbs_species %>% 
                          filter(farmland_list == "generalist") %>% 
                          select(spp) %>% 
                          mutate(group = "Farmland generalists"),
                        bbs_species %>%
                          filter(!is.na(woodland_list)) %>%
                          select(spp) %>%
                          mutate(group = "Woodland birds"),
                        bbs_species %>% 
                          filter(woodland_list  == "specialist") %>% 
                          select(spp) %>% 
                          mutate(group = "Woodland specialists"),
                        bbs_species %>% 
                          filter(woodland_list  == "generalist") %>% 
                          select(spp) %>% 
                          mutate(group = "Woodland generalists"),
                        bbs_species %>% 
                          filter(bocc4 != "green") %>% 
                          select(spp) %>% 
                          mutate(group = "BoCC Red/Amber (old)"),
                        bbs_species %>% 
                          filter(bocc5 != "green") %>% 
                          select(spp) %>% 
                          mutate(group = "BoCC Red/Amber"),
                        bbs_species %>% 
                          filter(rspb_list == "priority") %>% 
                          select(spp) %>% 
                          mutate(group = "RSPB Priority Species"),
                        bbs_species %>% 
                          filter(spp %in% c("CU", "RK", "SN", "L.", "OC")) %>% 
                          mutate(group = "Farmland waders")) %>% 
  select(spp, group)


# Geometric mean per group
birds_sum_geomean <- bbs_fits_out %>%
  select(spp, species, fit) %>% 
  unnest(cols = fit) %>%
  filter(scenario != "2015") %>%
  filter(spp != "WA") %>% # Exclude Water Rail 
  left_join(spp_groups, by = "spp") %>%
  group_by(scenario, group, country) %>% 
  summarise(pop_lo_rel = exp(mean(log(pop_lo_rel))),
            pop_hi_rel = exp(mean(log(pop_hi_rel))),
            n = length(species),
            .groups = "drop") 


# SAVE TIDY  ---------------------------------------------
save(birds_sum_geomean, file = here("outputs", "bird_results_tidy_max_wind_all.RData"))


