# 4 RANK AREAS FOR RENEWABLES ################################
source("./code/1_setup.R")


# SET RANK RENEWABLES FUNCTION ---------------------------------------------
rank_renewables <- function(country_abbr){
  
  # Load ALC raster 
  r_alc <- raster(here("rasters", "other", glue("r_alc_{country_abbr}.tif")))
  
  # Load LCM rasters
  r_lcm_5y <- stack(here("rasters", "other", glue("r_lcm_5y_{country_abbr}.tif")))
  r_lcm <- raster(r_lcm_5y[[8]])
  
  # Load wind opportunity data
  wind_list <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*wind*{country_abbr}*.tif$")), recursive = TRUE)
  wind_stack <- lapply(here("rasters", "scenarios", wind_list), raster)
  
  # Load solar opportunity data
  solar_list <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*solar*{country_abbr}*.tif$")), recursive = TRUE)
  solar_stack <- lapply(here("rasters", "scenarios", solar_list), raster)
  
  for (i in 1:length(wind_stack)) {
    
    # Get wind and solar opportunity rasters per scenario loop
    r_windop <- wind_stack[[i]]
    r_solarop <- solar_stack[[i]]
    
    # Get names for saving later
    name <- names(r_windop)
    scenario <- case_when(grepl("np", name) ~ paste0(str_sub(name, 14,14),"_np"),
                          TRUE ~  str_sub(name, 14,14))
    
    # Calculate modal ALC (25 ha units)
    print("Aggregating rasters 20-fold")
    
    r20 <- aggregate(r_alc, fact = 20, fun = modal)
    names(r20) <- "alc"
    
    # Get 25-ha cell ID
    r20$cell <- 1:ncell(r20)
    
    # Calculate area of wind & solar opportunity (25 ha units)
    print("Aggregating rasters 12-fold")
    r20$windop <- aggregate(r_windop, fact = 20, fun = sum) * 0.0625
    r20$solarop <- aggregate(r_solarop, fact = 20, fun = sum) * 0.0625
    # Calculate area of wind & solar overlap opportunity (25 ha units)
    r20$windsolarop <- aggregate(r_windop & r_solarop, fact = 20, fun = sum) * 0.0625
    
    # Construct attribute ranking table - 25ha
    print("Constructing ranking table")
    set.seed(1)
    df20_rank <- as_tibble(as.data.frame(r20)) %>%   
      drop_na(alc) %>%
      mutate(windop = ifelse(is.na(windop), 0, windop),
             solarop = ifelse(is.na(solarop), 0, solarop),
             windsolarop = ifelse(is.na(solarop), 0, solarop)) %>% 
      # ALC wind ranking 
      sample_frac(1) %>% 
      arrange(-alc) %>% 
      mutate(wind_rank = 1:nrow(.)) %>% 
      # ALC solar ranking 
      sample_frac(1) %>% 
      arrange(-alc) %>% 
      mutate(solar_rank = 1:nrow(.)) %>%
      arrange(cell)
    
    print("Projecting rasters back to 25-m")
    # Project rank to 25 ha
    r20_rank <- subsDT(stack(replicate(2, r20$cell)), select(df20_rank, cell, wind_rank, solar_rank), by = 1, which = c(2, 3))
    
    # Project rank back to 0.0625 ha
    r_rank <- stack(projectRaster(r20_rank, r_lcm, method = 'ngb'))
    names(r_rank) <- c("wind_rank", "solar_rank")
    
    print("Replacing 25-m values")
    # if is.na(LCM) <- NA
    r_rank$wind_rank[is.na(r_windop)] <- NA
    r_rank$solar_rank[is.na(r_solarop)] <- NA
    
    # SAVE OUTPUTS
    print("Saving outputs")
    
    data_name <- paste0("renewablerank_", country_abbr, "_", scenario)
    scenario_abbr <- str_sub(data_name, 19,19)
    
    # Dataframe
    save(df20_rank, file = here("outputs", "ranking tables", glue("df_{data_name}.RData")))
    
    # Raster
    writeRaster(r_rank, here("rasters", "scenarios", scenario_abbr, glue("r_{data_name}.tif")), overwrite = TRUE)
    
    # Progress update
    print(paste("Finsihed loop", i, "for", country_abbr, "-", Sys.time()))
    
  }
  
  # Progress update
  print("Removing temporary files")
  removeTmpFiles(h = 0)
  
  print(paste("--- FINISHED", country_abbr, "---"))
  
}


# RUN RANK RENEWABLES FUNCTION ---------------------------------------------
# Run per country
rank_renewables("eng")
rank_renewables("nir")
rank_renewables("sct")
rank_renewables("wal")

