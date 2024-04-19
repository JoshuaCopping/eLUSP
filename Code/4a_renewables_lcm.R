# 4a UPDATE LCM WITH RENEWABLES ################################
source("./Code/0_setup.R")


# LOAD DATA OUTSIDE FUNCTION ---------------------------------------------
# Load country props
load(here("data", "country_props_renewables.RData"))

# Load parameter file
scenario_params <- read_csv(here("data", "renewable_scenario_parameters.csv"), skip = 1)


# UPDATE LCM FUNCTION ---------------------------------------------
update_lcm <- function(country_abbreviation){
  
  print(paste("Updating LCMs for", country_abbreviation))
  
  # Creates df for looping over countries from function input
  country_df <- tibble(
    name = country_abbreviation 
  )
  
  # Dataframe for looping each scenario
  scenario_df <- tibble(
    lcm_name = c("a", "a", "b", "b", "c", "c", "d", "d", "e", "e", "f", "f", "g", "g", "h", "h", "i", "i"),
    opp_name = c("a", "a_np", "b", "b_np", "c", "c_np", "d", "d_np", "e", "e_np", "f", "f_np", "g", "g_np", "h", "h_np", "i", "i_np")
  )
  
  # Dataframe for looping ambition combos
  ambition_df <- tibble(
    ambition_level = c("high", "low")
  )
  
  for (i in 1:nrow(country_df)) {
    
    country_abbr <- country_df$name[i]
    
    # Load LCM data
    lcm_list <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*lcm*{country_abbr}*.tif$")), recursive = TRUE)
    lcm_stack <- lapply(here("rasters", "scenarios", lcm_list), stack) # stack all LCM scenarios
    
    # Load wind opportunity data
    wind_list <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*wind*{country_abbr}*.tif$")), recursive = TRUE)
    wind_stack <- lapply(here("rasters", "scenarios", wind_list), raster)
    
    # Load solar opportunity data
    solar_list <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*solar*{country_abbr}*.tif$")), recursive = TRUE)
    solar_stack <- lapply(here("rasters", "scenarios", solar_list), raster)
    
    
    ## Loop for scenario ----
    for (j in 1:nrow(scenario_df)) {
      
      # Get names/abbreviations LCM
      lcm_abbr <- scenario_df$lcm_name[[j]]
      scenario_abbr <- scenario_df$opp_name[[j]]
      
      # Select relevant/needed rasters
      r_windop <- wind_stack[[j]]
      r_solarop <- solar_stack[[j]]
      
      # Load ranking data
      r_renewablerank <- stack(here("rasters", "scenarios", lcm_abbr, glue("r_renewablerank_{country_abbr}_{scenario_abbr}.tif")))
      load(here("outputs", "ranking tables", glue("df_renewablerank_{country_abbr}_{scenario_abbr}.RData")))
      
      df_renewablerank <- df20_rank
      
      # Update parameters
      # Wind 
      params_wind <- scenario_params %>%
        filter(grepl("wind_", parameter)) %>%
        select(-note) %>% 
        # Capacity in GW, convert to MW, - then divide by density (5 MW/km2 for wind) - then convert km2 to ha
        mutate(ha = (value*1000)/5*100,
               year = as.numeric(gsub("wind_rate_", "", parameter))) %>% 
        select(year, ha, ambition) %>% 
        # Add country props
        crossing(select(country_props_renewables[[j]], country, wind_prop)) %>% 
        mutate(ha = ha * wind_prop) %>% 
        select(-wind_prop)
      
      # Solar 
      params_solar <- scenario_params %>%
        filter(grepl("solar_", parameter)) %>%
        select(-note) %>% 
        # Capacity in GW, convert to MW, - then divide by density (45 MW/km2 for wind) - then convert km2 to ha
        mutate(ha = (value*1000)/45*100,
               year = as.numeric(gsub("solar_rate_", "", parameter))) %>% 
        select(year, ha, ambition) %>% 
        # Add country props
        crossing(select(country_props_renewables[[j]], country, solar_prop)) %>% 
        mutate(ha = ha * solar_prop) %>% 
        select(-solar_prop)
      
      
      ## Loop for high/low ambition levels ----
      for (k in 1:nrow(ambition_df)) {
        
        # Select LCM layer, to be updated per ambition level loop
        # LCM - only 9 loaded, so use loop number/2 rounded
        lcm_num <- ceiling(j/2)
        r_lcm <- lcm_stack[[lcm_num]]
        
        # Update LCM names with year
        names(r_lcm) <- paste0("y", seq(2020, 2050, by = 5))
        
        # Select abbreviations/names for filtering scenario parameters
        ambition_level <- ambition_df$ambition_level[[k]]
        country_name <- case_when(country_abbr == "eng" ~ "England",
                                  country_abbr == "nir" ~ "Northern Ireland",
                                  country_abbr == "sct" ~ "Scotland",
                                  country_abbr == "wal" ~ "Wales")
        
        # Scenario parameters
        params_wind2 <- params_wind %>% filter(ambition == paste(ambition_level) & country == paste(country_name))
        params_solar2 <- params_solar %>% filter(ambition == paste(ambition_level) & country == paste(country_name))
        
        ## MAIN LCM UPDATE FUNCTION ----
        # t = number of time periods
        t <- nlayers(r_lcm)
        
        # Renewable energy installations - onshore wind first then solar
        if(sum(params_wind2$ha) != 0 | sum(params_solar2$ha) != 0){
          
          # Bind wind and solar params into renewable - suppress name changes from showing in console
          suppressMessages(ha_renewable <- bind_cols(params_wind2, params_solar2) %>% 
                             select(c(1, 2, 6)) %>% 
                             set_names(c("year", "wind", "solar")))
          
          # Create empty df for installation year
          df_renewablerank2 <- df_renewablerank %>% 
            mutate(wind_year = NA_real_, solar_year = NA_real_,
                   wind_add = 0, solar_add = 0, 
                   wind_area = 0, solar_area = 0)  
          
          for(l in 1:(t-1)){
            df_renewablerank2 <- df_renewablerank2 %>%
              # Add new wind in rank order
              arrange(wind_rank) %>% 
              mutate(wind_add = case_when(!is.na(wind_year) ~ 0,
                                          is.na(wind_year) & cumsum(windop) <= ha_renewable$wind[l] ~ windop,
                                          is.na(wind_year) & cumsum(windop) >  ha_renewable$wind[l] ~ 0)) %>%
              mutate(windop = windop - wind_add) %>% 
              # Update windsolarop column to stop assigning wind and solar in same cell
              mutate(solarop = ifelse(wind_add > 0, solarop - windsolarop, solarop),
                     windsolarop = ifelse(wind_add > 0 , 0, windsolarop)) %>%
              # Add new solar in rank order
              arrange(solar_rank) %>% 
              mutate(solar_add = case_when(!is.na(solar_year) ~ 0,
                                           is.na(solar_year) & cumsum(solarop) <= ha_renewable$solar[l] ~ solarop,
                                           is.na(solar_year) & cumsum(solarop) >  ha_renewable$solar[l]  ~ 0)) %>%
              mutate(solarop = solarop - solar_add)%>%
              mutate(windop = ifelse(solar_add > 0, windop - windsolarop, windop),
                     windsolarop = ifelse(solar_add > 0 , 0, windsolarop)) %>%
              # Add creation year
              mutate(wind_year = ifelse(wind_add == 0, wind_year, ha_renewable$year[l]),
                     solar_year = ifelse(solar_add == 0, solar_year, ha_renewable$year[l])) %>% 
              # Uupdate renewables area
              mutate(wind_area = wind_area + wind_add,
                     solar_area = solar_area + solar_add,
                     wind_add = 0,
                     solar_add = 0)
            
          }
          
          # Check how much land has been assigned as percentage of total
          # df_renewablerank2 %>% group_by(wind_year) %>% summarise(sum(wind_area) / unique(ha_renewable$wind))
          # df_renewablerank2 %>% group_by(solar_year) %>% summarise(sum(solar_area) / unique(ha_renewable$solar))
          
          # Tidy
          df_renewablerank2 <- df_renewablerank2 %>% 
            select(wind_rank, solar_rank, wind_year, solar_year) 
          
          # Substitute renewable rank (wind and solar rank) with year created 
          r_year <- subsDT(r_renewablerank[[c(1, 2)]], df_renewablerank2, by = c(1, 2), which = c(3, 4))
          names(r_year) <- c("wind_year", "solar_year")
          
          r_year$wind_year[is.na(r_windop)] <- NA
          r_year$solar_year[is.na(r_solarop)] <- NA
          
          # Get values
          v_wind <- getValues(r_year$wind_year)
          v_solar<- getValues(r_year$solar_year)
          
          # Check how much land is being assigned 
          # table(v_wind) * 0.0625
          # table(v_solar) * 0.0625
          
          # Update r_lcm
          for(m in 2:t){
            r_lcm[[m]][v_wind <= (2020 + 5 * (m - 1))] <- r_lcm[[m]][v_wind <= (2020 + 5 * (m - 1))] + 20 # wind 
            r_lcm[[m]][v_solar <= (2020 + 5 * (m - 1))] <- 25 # solar
            
          }
          
          # Tidy
          rm(v_wind, v_solar); gc()
        }
        
        # Save raster
        writeRaster(r_lcm, here("rasters", "updated lcm", lcm_abbr, glue("r_renewable_lcm_{country_abbr}_{scenario_abbr}_{ambition_level}.tif")), overwrite = TRUE)
        
        # remove temporary files
        removeTmpFiles(h = 0)
        
        # Progress update
        print(paste("Finished", country_abbr, scenario_abbr, ambition_level, "-", Sys.time()))
        
      }
      
    }
    
    # Progress update
    print(paste("Finished", country_abbr))
    
  }
  
  print("--- FINISHED ---")
  
}


# RUN FUNCTION ---------------------------------------------
# Run per country
update_lcm("eng")
update_lcm("nir")
update_lcm("sct")
update_lcm("wal")

