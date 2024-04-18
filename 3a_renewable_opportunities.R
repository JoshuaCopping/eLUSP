# 3a RENEWABLES OPPORTUNITY LAYERS ######################
source("./code/1_setup.R")

# WIND & SOLAR OPPORTUNITIES FUNCTION  -------------------------
opportunities_function <- function(country_abbr, national_park = "NO"){
  
  # Load rasters 
  lcm_list <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*lcm*{country_abbr}*.tif$")), recursive = TRUE)
  lcm_stack <- lapply(here("rasters", "scenarios", lcm_list), stack) # stack all LCM scenarios
  
  r_wind <- raster(here("rasters", "opportunity masking", glue("r_wind_{country_abbr}.tif"))) # wind speed 
  r_solar <- raster(here("rasters", "opportunity masking", glue("r_solar2_{country_abbr}.tif"))) # solar irradiance 
  r_slope <- raster(here("rasters", "opportunity masking", glue("r_slope_{country_abbr}.tif"))) # slope %
  r_aspect <- raster(here("rasters", "opportunity masking", glue("r_aspect_{country_abbr}.tif"))) # aspect in degrees 
  r_woodop <- raster(here("rasters", "opportunity masking", glue("r_woodop_conifers_{country_abbr}.tif"))) # updated TBL woodop with conifers
  r_wind_sensitivity <- raster(here("rasters", "opportunity masking", glue("r_wind_sensitivity_{country_abbr}.tif"))) # sensitive species - wind
  r_solar_sensitivity <- raster(here("rasters", "opportunity masking", glue("r_solar_sensitivity_{country_abbr}.tif"))) # sensitive species - solar
  r_airport <- raster(here("rasters", "opportunity masking", glue("r_airport_{country_abbr}.tif"))) # buffered airports 
  r_geese <- raster(here("rasters", "opportunity masking", glue("r_geese_{country_abbr}.tif"))) # buffered geese/swan SPAs
  r_np <- raster(here("rasters", "opportunity masking", glue("r_np_{country_abbr}.tif"))) # national parks, national scenic areas, AONBs
  r_peat <- raster(here("rasters", "get area data", glue("r_peat_{country_abbr}.tif"))) # peat areas
  
  # Loop over each scenario a:i
  for (i in 1:length(lcm_stack)) {
    
    # Update with current scenario & stack
    r_lcm <- lcm_stack[[i]] 
    stacked_rasters <- stack(r_lcm, r_woodop, r_wind, r_solar, r_wind_sensitivity, r_solar_sensitivity, r_slope, r_aspect, r_airport, r_geese, r_peat, r_np)
    
    ## Get scenario name/id
    name <- names(r_lcm[[1]])
    scenario_abbr <- str_sub(name, 7,7)
    scenario_abbr_np <- scenario_abbr # Creates scenario_abbr_np for saving path
    
    # WIND OPPORTUNITY ----
    print("Creating wind opportunities layer")
    r_windop <- raster(stacked_rasters[[1]]) # Create blank raster
    r_windop[stacked_rasters[[8]][] == 1] <- 1 # HABITAT/FEATURE SENSITIVITY - allow on woodop layer
    r_windop[stacked_rasters[[1]][] < 2 | stacked_rasters[[1]][] >= 5] <- NA # LAND COVER - 2020 - exclude land cover types other than 2,3,4
    r_windop[stacked_rasters[[2]][] < 2 | stacked_rasters[[2]][] >= 5] <- NA # LAND COVER - 2025 - exclude land cover types other than 2,3,4
    r_windop[stacked_rasters[[3]][] < 2 | stacked_rasters[[3]][] >= 5] <- NA # LAND COVER - 2030 - exclude land cover types other than 2,3,4
    r_windop[stacked_rasters[[4]][] < 2 | stacked_rasters[[4]][] >= 5] <- NA # LAND COVER - 2035 - exclude land cover types other than 2,3,4
    r_windop[stacked_rasters[[5]][] < 2 | stacked_rasters[[5]][] >= 5] <- NA # LAND COVER - 2040 - exclude land cover types other than 2,3,4
    r_windop[stacked_rasters[[6]][] < 2 | stacked_rasters[[6]][] >= 5] <- NA # LAND COVER - 2045 - exclude land cover types other than 2,3,4
    r_windop[stacked_rasters[[7]][] < 2 | stacked_rasters[[7]][] >= 5] <- NA # LAND COVER - 2050 - exclude land cover types other than 2,3,4
    r_windop[stacked_rasters[[7]][] > 2.0 & stacked_rasters[[7]][] < 2.2] <- NA # LAND COVER - exclude Caledonian pine - value isn't exactly 2.1, which is why using this statement
    r_windop[stacked_rasters[[9]][] < 5] <- NA # WIND SPEED - exclude wind speed less than 5 m/s
    r_windop[stacked_rasters[[11]][] > 0] <- NA # SPECIES SENSITIVITY - exclude medium and high areas - wind sensitivity
    r_windop[stacked_rasters[[13]][] > 18] <- NA # SLOPE - exclude slopes more than 18%
    r_windop[stacked_rasters[[15]][] == 1] <- NA # AIRPORTS - exclude 5km buffered airport areas
    r_windop[stacked_rasters[[16]][] == 1] <- NA # GEESE/SWANS - exclude buffered geese/swan SPAs
    r_windop[stacked_rasters[[17]][] == 1] <- NA # PEAT - exclude peat areas - issue with coniferous woodland on peat in woodop - not needed for solar
    # NATIONAL PARKS - inclusion/exclusion of AONBs, national parks, national scenic areas - NO = building excluded from national parks
    if(national_park == "NO"){
      print("Masking national parks")
      r_windop[stacked_rasters[[18]][] == 1] <- NA # National parks, national scenic areas and AONBs
      scenario_abbr_np <- paste0(scenario_abbr, "_np") # Updates scenario_abbr_np with np if used
    }
    print("Saving windop raster")
    writeRaster(r_windop, here("rasters", "scenarios", scenario_abbr, glue("r_windop_{country_abbr}_{scenario_abbr_np}.tif")), overwrite = TRUE)
    
    # SOLAR OPPORTUNITY ----
    print("Creating solar opportunities layer")
    r_solarop <- raster(stacked_rasters[[1]]) # Create blank raster
    r_solarop[stacked_rasters[[8]][] == 1] <- 1 # HABITAT/FEATURE SENSITIVITY - allow on woodop layer
    r_solarop[stacked_rasters[[1]][] < 3 | stacked_rasters[[1]][] >= 5] <- NA # LAND COVER - 2020 - exclude land cover types other than 3,4
    r_solarop[stacked_rasters[[2]][] < 3 | stacked_rasters[[2]][] >= 5] <- NA # LAND COVER - 2025 - exclude land cover types other than 3,4
    r_solarop[stacked_rasters[[3]][] < 3 | stacked_rasters[[3]][] >= 5] <- NA # LAND COVER - 2030 - exclude land cover types other than 3,4
    r_solarop[stacked_rasters[[4]][] < 3 | stacked_rasters[[4]][] >= 5] <- NA # LAND COVER - 2035 - exclude land cover types other than 3,4
    r_solarop[stacked_rasters[[5]][] < 3 | stacked_rasters[[5]][] >= 5] <- NA # LAND COVER - 2040 - exclude land cover types other than 3,4
    r_solarop[stacked_rasters[[6]][] < 3 | stacked_rasters[[6]][] >= 5] <- NA # LAND COVER - 2045 - exclude land cover types other than 3,4
    r_solarop[stacked_rasters[[7]][] < 3 | stacked_rasters[[7]][] >= 5] <- NA # LAND COVER - 2050 - exclude land cover types other than 3,4
    r_solarop[stacked_rasters[[7]][] > 2.0 & stacked_rasters[[7]][] < 2.2] <- NA # LAND COVER - exclude Caledonian pine - value isn't exactly 2.1, which is why using this statement
    r_solarop[stacked_rasters[[10]][] < 1075.2] <- NA # IRRADIANCE - exclude irradiance values under 1075.2 kWh/m2
    r_solarop[stacked_rasters[[12]][] > 0] <- NA # SPECIES SENSITIVITY - exclude medium and high areas - solar sensitivity
    r_solarop[stacked_rasters[[13]][] > 5] <- NA # SLOPE - exclude slopes >5%
    r_solarop[stacked_rasters[[14]][] < 112.5 | stacked_rasters[[8]][] > 247.5] <- NA # ASPECT - exclude aspects other than southeast, south and southwest
    # NATIONAL PARKS - inclusion/exclusion of AONBs, national parks, national scenic areas - NO = building excluded from national parks
    if(national_park == "NO"){
      print("Masking national parks")
      r_solarop[stacked_rasters[[18]][] == 1] <- NA # National parks, national scenic areas and AONBs
      scenario_abbr_np <- paste0(scenario_abbr, "_np") # Updates scenario_abbr_np with np if used
    }
    print("Saving solarop raster")
    writeRaster(r_solarop, here("rasters", "scenarios", scenario_abbr, glue("r_solarop_{country_abbr}_{scenario_abbr_np}.tif")), overwrite = TRUE)
    
    print("Removing temporary files")
    removeTmpFiles(h = 0)
    
    # Progress update
    print(paste("Finished loop", i, "for", country_abbr, "-", Sys.time()))
    
  }
  
  # Progress update
  print(paste("--- FINISHED", country_abbr, "---"))
  
}


# RUN FUNCTION -------------------------
# Run per country with National Park/protected landscape inclusion and exclusion
# England 
opportunities_function("eng", national_park = "NO")
opportunities_function("eng", national_park = "YES")

# Northern Ireland 
opportunities_function("nir", national_park = "NO")
opportunities_function("nir", national_park = "YES")

# Scotland 
opportunities_function("sct", national_park = "NO")
opportunities_function("sct", national_park = "YES")

# Wales 
opportunities_function("wal", national_park = "NO")
opportunities_function("wal", national_park = "YES")


