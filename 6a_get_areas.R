# 6a GET AREAS ################################
source("./code/1_setup.R")

# Cross-tabulate data to get area and associated values - running in loops per country and ambition due to intensity and merging later


# LOAD SCENARIOS DATA AND SETUP ---------------------------------------------
# LCM - 2015 
r_lcm_eng <- raster(here("rasters", "get area data", "r_lcm_new_eng.tif"))
r_lcm_nir <- raster(here("rasters", "get area data", "r_lcm_new_nir.tif"))
r_lcm_sct <- raster(here("rasters", "get area data", "r_lcm_new_sct.tif"))
r_lcm_wal <- raster(here("rasters", "get area data", "r_lcm_new_wal.tif"))

# Native pinewood
r_pinewood_sct <- raster(here("rasters", "get area data", "r_pinewood_sct.tif"))
r_lcm_sct[r_pinewood_sct[] == 1] <- 2.1

# LCM lookup
load(here("data", "raster_lookup_lcm.RData"))
# Add novel land covers
lookup_lcm <- lookup_lcm %>% 
  bind_rows(tibble(lcm_layer = c(2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 9.1, 10.1, 3.2, 4.2, 3.3, 4.3, 3.31, 4.31, 3.4, 4.4, 5.2, 6.2, 7.2, 22, 23, 23.1, 23.2, 23.3, 23.31, 23.4, 24, 24.1, 24.2, 24.3, 24.31, 24.4, 25),
                   lcm = c("c.w_pinewood", "a.h_silvoa", "i.g_silvop", "n.g_woodpa", "c.g_woodpa", "a.g_woodpa", "h.r_woodpa", "h.r_woodpa", "a.h_energy", "i.g_energy", "a.h_organic", "i.g_organic", "a.h_organic_silvoa", "i.g_organic_silvop", "a.h_palud", "i.g_palud", "n.g_new", "c.g_new", "a.g_new", "wind_c.w", "wind_a.h", "wind_a.h_silvoa", "wind_a.h_energy", "wind_a.h_organic", "wind_a.h_organic_silvoa", "wind_a.h_palud", "wind_i.g", "wind_i.g_silvop", "wind_i.g_energy", "wind_i.g_organic", "wind_i.g_organic_silvop", "wind_i.g_palud", "solar")))

# ALC
r_alc_eng <- raster(here("rasters", "other", "r_alc_eng.tif"))
r_alc_nir <- raster(here("rasters", "other", "r_alc_nir.tif"))
r_alc_sct <- raster(here("rasters", "other", "r_alc_sct.tif"))
r_alc_wal <- raster(here("rasters", "other", "r_alc_wal.tif"))
load(here("data", "raster_lookup_alc.RData"))

# NUTS1
r_nuts1_eng <- raster(here("rasters", "get area data", "r_nuts1_eng.tif"))
r_nuts1_nir <- raster(here("rasters", "get area data", "r_nuts1_nir.tif"))
r_nuts1_sct <- raster(here("rasters", "get area data", "r_nuts1_sct.tif"))
r_nuts1_wal <- raster(here("rasters", "get area data", "r_nuts1_wal.tif"))
load(here("data", "raster_lookup_nuts1.RData"))

# Peat
r_peat_eng <- raster(here("rasters", "get area data", "r_peat_eng.tif"))
r_peat_nir <- raster(here("rasters", "get area data", "r_peat_nir.tif"))
r_peat_sct <- raster(here("rasters", "get area data", "r_peat_sct.tif"))
r_peat_wal <- raster(here("rasters", "get area data", "r_peat_wal.tif"))

# Yield class
r_cyc_eng <- stack(here("rasters", "get area data", "r_cyc_eng.tif"))[[c(6:9)]]
r_cyc_nir <- stack(here("rasters", "get area data", "r_cyc_nir.tif"))[[c(6:9)]]
r_cyc_sct <- stack(here("rasters", "get area data", "r_cyc_sct.tif"))[[c(6:9)]]
r_cyc_wal <- stack(here("rasters", "get area data", "r_cyc_wal.tif"))[[c(6:9)]]

names(r_cyc_eng) <- c("yc_sbi", "yc_sok", "yc_sp", "yc_ss")
names(r_cyc_nir) <- c("yc_sbi", "yc_sok", "yc_sp", "yc_ss")
names(r_cyc_sct) <- c("yc_sbi", "yc_sok", "yc_sp", "yc_ss")
names(r_cyc_wal) <- c("yc_sbi", "yc_sok", "yc_sp", "yc_ss")

# Nat regen
r_natregen_eng <- raster(here("rasters", "get area data", "r_natregen_eng.tif"))
r_natregen_nir <- raster(here("rasters", "get area data", "r_natregen_nir.tif"))
r_natregen_sct <- raster(here("rasters", "get area data", "r_natregen_sct.tif"))
r_natregen_wal <- raster(here("rasters", "get area data", "r_natregen_wal.tif"))

# Soil
r_soil_eng <- raster(here("rasters", "get area data", "r_soiltype_eng.tif"))
r_soil_nir <- raster(here("rasters", "get area data", "r_soiltype_nir.tif"))
r_soil_sct <- raster(here("rasters", "get area data", "r_soiltype_sct.tif"))
r_soil_wal <- raster(here("rasters", "get area data", "r_soiltype_wal.tif"))


# CREATE GET AREAS FUNCTION ---------------------------------------------
get_areas_loops <- function(country_abbr, scenario_abbr){
  
  options(dplyr.summarise.inform = FALSE)
  
  # Scenarios areas blank dataframe
  scenarios_areas <- tibble(
    country = as.character(),
    scneario = as.character(),
    year = as.integer(),
    peat = as.double(),
    area = as.double(),
    yc_sbi = as.double(),
    yc_sok = as.double(),
    yc_sp = as.double(),
    yc_ss = as.double(),
    natregen = as.double(),
    soil = as.double(),
    lcm = as.character(),
    nuts1 = as.character(), 
    alc = as.character()
  )
  
  # Scenarios change areas blank dataframe
  scenarios_change_areas <- tibble(
    year = as.double(),
    lcm_from = as.character(),
    lcm_to = as.character(),
    peat = as.double(),
    nuts1 = as.character(), 
    alc = as.character(),
    yc_sbi = as.double(),
    yc_sok = as.double(),
    yc_sp = as.double(),
    yc_ss = as.double(),
    natregen = as.double(),
    soil = as.double(),
    area = as.double(),
    country = as.character(),
    scenario = as.character()
  )
  
  # Rename data for function
  if(country_abbr == "nir"){
    r_lcm <- r_lcm_nir 
    r_nuts1 <- r_nuts1_nir
    r_alc <- r_alc_nir 
    r_peat <- r_peat_nir
    r_cyc <- r_cyc_nir
    r_natregen <- r_natregen_nir
    r_soil <- r_natregen_nir
  }
  
  if(country_abbr == "eng"){
    r_lcm <- r_lcm_eng 
    r_nuts1 <- r_nuts1_eng
    r_alc <- r_alc_eng 
    r_peat <- r_peat_eng
    r_cyc <- r_cyc_eng
    r_natregen <- r_natregen_eng
    r_soil <- r_natregen_eng
  }
  
  if(country_abbr == "sct"){
    r_lcm <- r_lcm_sct 
    r_nuts1 <- r_nuts1_sct
    r_alc <- r_alc_sct 
    r_peat <- r_peat_sct
    r_cyc <- r_cyc_sct
    r_natregen <- r_natregen_sct
    r_soil <- r_natregen_sct
  }
  
  if(country_abbr == "wal"){
    r_lcm <- r_lcm_wal 
    r_nuts1 <- r_nuts1_wal
    r_alc <- r_alc_wal 
    r_peat <- r_peat_wal
    r_cyc <- r_cyc_wal
    r_natregen <- r_natregen_wal
    r_soil <- r_natregen_wal
  }
  
  .country <- case_when(country_abbr == "eng" ~ "England",
                        country_abbr == "nir" ~ "Northern Ireland",
                        country_abbr == "sct" ~ "Scotland",
                        country_abbr == "wal" ~ "Wales")
  
  lcm_list <-list.files(here("rasters", "updated lcm"), pattern = glob2rx(glue("r_renewable_lcm_{country_abbr}*{scenario_abbr}.tif$")), recursive = TRUE)
  lcm_stack_all <- lapply(here("rasters", "updated lcm", lcm_list), stack) # stack all LCM scenarios
  
  
  for(j in 1:length(lcm_stack_all)){
    
    .scenario <- str_sub(names(lcm_stack_all[[j]][[1]]), 21) %>% 
      str_sub(end = -3)
    
    r_lcm_stack <- lcm_stack_all[[j]]
    
    r_lcm_stack <- stack(r_lcm, r_lcm_stack)
    t <- nlayers(r_lcm_stack)
    
    ##### Cross-tabulate areas in each period
    print("Cross-tabulating areas...")
    
    ## Stack fixed spatial variables
    r_vars_stack <- stack(r_nuts1, r_alc, r_peat, r_cyc, r_natregen, r_soil)
    
    ## Fix names
    names(r_lcm_stack) <- paste0("y", 2015 + 5 * seq(0, t - 1, by = 1))
    names(r_vars_stack)[c(1:3, 8, 9)] <- c("nuts1_layer", "alc_layer", "peat", "natregen", "soil")
    
    ## Crosstabulate areas
    # LCM
    df_lcm_5y <- crosstabDT(stack(r_lcm_stack, r_vars_stack), digits = 2, long = TRUE, useNA = TRUE)
    
    ## Tidy
    areas <- df_lcm_5y %>% 
      as.tbl() %>% 
      gather(year, lcm_layer, -nuts1_layer, -alc_layer, -peat, -yc_sbi, -yc_sok, -yc_sp, -yc_ss, -natregen, -soil, -Freq) %>% 
      filter(!is.na(lcm_layer)) %>% 
      mutate(year = as.integer(gsub("y", "", year)),
             area = Freq * 0.0625,
             country = .country, 
             scenario = .scenario) %>% 
      select(country, scenario, year, lcm_layer, nuts1_layer, alc_layer, peat, area, yc_sbi, yc_sok, yc_sp, yc_ss, natregen, soil) %>% 
      left_join(lookup_lcm, by = "lcm_layer") %>% 
      left_join(lookup_nuts1, by = "nuts1_layer") %>% 
      left_join(lookup_alc, by = c("alc_layer", "country")) %>% 
      select(-contains("layer"))
    
    
    ##### Calculate land-use change areas
    if(t > 1){
      print("Calculating land-use change areas...")
      
      df_lcm_5y <- df_lcm_5y %>%
        as_tibble()
      
      ## Empty list
      change_areas <- as.list(rep(NA, t - 1))
      
      for(i in 1:(t - 1)){
        change_areas[[i]] <- df_lcm_5y[, c(i, i+1, t+1:10)] %>%
          filter(complete.cases(.)) %>% 
          set_names(c("lcm_from", "lcm_to", names(.)[-c(1, 2)])) %>%
          mutate(country = .country) %>% 
          left_join(lookup_lcm, by = c('lcm_from' = 'lcm_layer')) %>% 
          left_join(lookup_lcm, by = c('lcm_to' = 'lcm_layer')) %>% 
          left_join(lookup_nuts1, by = "nuts1_layer") %>% 
          left_join(lookup_alc, by = c("alc_layer", "country")) %>% 
          select(lcm.x, lcm.y, peat, nuts1, alc, yc_sbi, yc_sok, yc_sp, yc_ss, natregen, soil, Freq) %>%
          rename("lcm_from" = lcm.x, "lcm_to" = lcm.y) %>% 
          mutate(year = 2015 + i * 5) %>%
          group_by(year, lcm_from, lcm_to, peat, nuts1, alc, yc_sbi, yc_sok, yc_sp, yc_ss, natregen, soil) %>% 
          summarise(area = sum(Freq * 0.0625)) %>% 
          ungroup() %>% 
          mutate(country = .country, 
                 scenario = .scenario)
      }
      
      change_areas <- bind_rows(change_areas)

    } 
    
    # Bind data
    scenarios_areas <- rbind(scenarios_areas, areas)
    scenarios_change_areas <- rbind(scenarios_change_areas, change_areas)
    
    # Progress update
    print(paste("Finished loop", j, "for", country_abbr, scenario_abbr, "-", Sys.time()))

  }
  
  # Set DT ----
  setDT(scenarios_areas)
  setDT(scenarios_change_areas)

  # SAVE OUTPUTS ----
  save(scenarios_areas, scenarios_change_areas, file = here("outputs", "scenario areas", glue("scenarios_areas_{country_abbr}_{scenario_abbr}.RData")))
  print(paste("Finished", country_abbr, scenario_abbr))
  
  return(list(scenarios_areas = scenarios_areas, scenarios_change_areas = scenarios_change_areas)) 
  
  removeTmpFiles(h = 0); gc()
  
}


# RUN FUNCTION ---------------------------------------------
# Run per country and ambition
get_areas_loops("nir", "high")
get_areas_loops("nir", "low")

get_areas_loops("wal", "high")
get_areas_loops("wal", "low")

get_areas_loops("sct", "high")
get_areas_loops("sct", "low")

get_areas_loops("eng", "high")
get_areas_loops("eng", "low")






