# 1a DATA PREPARATION ######################
source("./Code/0_setup.R")

# LOAD RASTERS -------------------------- 
# Land cover - using scenario d 
eng.lcm <- stack(here("rasters", "scenarios", "d", "r_lcm_d_eng.tif"))
nir.lcm <- stack(here("rasters", "scenarios", "d", "r_lcm_d_nir.tif"))
sct.lcm <- stack(here("rasters", "scenarios", "d", "r_lcm_d_sct.tif"))
wal.lcm <- stack(here("rasters", "scenarios", "d", "r_lcm_d_wal.tif"))

# Wind speed
eng.wind <- raster(here("rasters", "raw", "eng_wind_45.tif"))
nir.wind <- raster(here("rasters", "raw", "nir_wind_45_ing.tif"))
sct.wind <- raster(here("rasters", "raw", "sct_wind_45.tif"))
wal.wind <- raster(here("rasters", "raw", "wal_wind_45.tif"))

# Solar irradiance
eng.solar <- raster(here("rasters", "raw", "eng_solar_v2.tif"))
nir.solar <- raster(here("rasters", "raw", "nir_solar_v2.tif"))
sct.solar <- raster(here("rasters", "raw", "sct_solar_v2.tif"))
wal.solar <- raster(here("rasters", "raw", "wal_solar_v2.tif"))

# Slope 
eng.slope <- raster(here("rasters", "raw", "eng_slope_percentage.tif"))
nir.slope <- raster(here("rasters", "raw", "nir_slope_percentage_ing.tif"))
sct.slope <- raster(here("rasters", "raw", "sct_slope_percentage.tif"))
wal.slope <- raster(here("rasters", "raw", "wal_slope_percentage.tif"))

# National Parks 
eng.np <- raster(here("rasters", "raw", "eng_NP_AONB_NSA_rast.shp.tif"))
nir.np <- raster(here("rasters", "raw", "nir_NP_AONB_NSA_rast.shp.tif"))
sct.np <- raster(here("rasters", "raw", "sct_NP_AONB_NSA_rast.shp.tif"))
wal.np <- raster(here("rasters", "raw", "wal_NP_AONB_NSA_rast.shp.tif"))

# Airports 
eng.airport <- raster(here("rasters", "raw", "eng_airports_5k_rast.tif"))
nir.airport <- raster(here("rasters", "raw", "nir_airports_5k_rast.tif"))
sct.airport <- raster(here("rasters", "raw", "sct_airports_5k_rast.tif"))
wal.airport <- raster(here("rasters", "raw", "wal_airports_5k_rast.tif"))

# Aspect 
eng.aspect <- raster(here("rasters", "raw", "eng_aspect.tif"))
nir.aspect <- raster(here("rasters", "raw", "nir_aspect.tif"))
sct.aspect <- raster(here("rasters", "raw", "sct_aspect.tif"))
wal.aspect <- raster(here("rasters", "raw", "wal_aspect.tif"))

# Eco-sensitivity - species - wind
eng.wind.sensit <- raster(here("rasters", "raw", "eng_wind_sensitivity_filled.tif"))
nir.wind.sensit <- raster(here("rasters", "raw", "nir_wind_sensitivity_filled.tif"))
sct.wind.sensit <- raster(here("rasters", "raw", "sct_wind_sensitivity_filled.tif"))
wal.wind.sensit <- raster(here("rasters", "raw", "wal_wind_sensitivity_filled.tif"))

# Eco-sensitivity - species - solar
eng.solar.sensit <- raster(here("rasters", "raw", "eng_solar_sensitivity_filled.tif"))
nir.solar.sensit <- raster(here("rasters", "raw", "nir_solar_sensitivity_filled.tif"))
sct.solar.sensit <- raster(here("rasters", "raw", "sct_solar_sensitivity_filled.tif"))
wal.solar.sensit <- raster(here("rasters", "raw", "wal_solar_sensitivity_filled.tif"))


# TIDY RASTERS function -------------------------
# Crop to same extent and resample to same resolution as lcm, saving output

# Function 
# Output = file path for saving
crop_resample <- function(lcm, 
                          wind_input, wind_output,
                          solar_input, solar_output,
                          slope_input, slope_output){
  
  # Wind
  print("cropping and resampling wind")
  updated_wind <- terra::crop(wind_input, lcm, snap = "near") 
  updated_wind <- terra::resample(updated_wind, lcm, "bilinear")
  writeRaster(updated_wind, wind_output, overwrite = TRUE) 
  # Solar
  print("cropping and resampling solar")
  updated_solar <- terra::crop(solar_input, lcm, snap = "near") 
  updated_solar <- terra::resample(updated_solar, lcm, "bilinear")
  writeRaster(updated_solar, solar_output, overwrite = TRUE) 
  # Slope
  print("cropping and resampling slope")
  updated_slope <- terra::crop(slope_input, lcm, snap = "near") 
  updated_slope <- terra::resample(updated_slope, lcm, "bilinear")
  writeRaster(updated_slope, slope_output, overwrite = TRUE)
  
  print("crop and resampling complete")
}

# RUN FUNCTION  -------------------------
# Ignore for single variables 
# England 
crop_resample(eng.lcm[[7]], 
              eng.wind, "./rasters_edited/r_wind_eng.tif",
              eng.solar, "./rasters_edited/r_solar_eng.tif",
              eng.slope, "./rasters_edited/r_slope_eng.tif")
print("ENGLAND FINISHED")

# Northern Ireland
crop_resample(nir.lcm[[7]], 
              nir.wind, "./rasters_edited/r_wind_nir.tif",
              nir.solar, "./rasters_edited/r_solar_nir.tif",
              nir.slope, "./rasters_edited/r_slope_nir.tif")
print("NORTHERN IRELAND FINISHED")

# Scotland 
crop_resample(sct.lcm[[7]], 
              sct.wind, "./rasters_edited/r_wind_sct.tif",
              sct.solar, "./rasters_edited/r_solar_sct.tif",
              sct.slope, "./rasters_edited/r_slope_sct.tif")
print("SCOTLAND FINISHED")

# Wales 
crop_resample(wal.lcm[[7]], 
              wal.wind, "./rasters_edited/r_wind_wal.tif",
              wal.solar, "./rasters_edited/r_solar_wal.tif",
              wal.slope, "./rasters_edited/r_slope_wal.tif")
print("WALES FINISHED")


# TIDY RASTERS single layers no function  -------------------------
# Reproject NIR new files to lcm

## NOT ALWAYS NEEDED - CHECK NIR CRS
# nir.np <- projectRaster(nir.np, nir.lcm) # reproject nir data 

### below converts to terra object for using 'near' method, for rasters with classes - i.e. sensitivity
eng.wind.sensit <- rast(eng.wind.sensit)
eng.lcm <- rast(eng.lcm[[7]])

nir.wind.sensit <- rast(nir.wind.sensit)
nir.lcm <- rast(nir.lcm[[7]])

sct.wind.sensit <- rast(sct.wind.sensit)
sct.lcm <- rast(sct.lcm[[7]])

wal.wind.sensit <- rast(wal.wind.sensit)
wal.lcm <- rast(wal.lcm[[7]])


# Crop and resample data 
updated_raster <- terra::crop(eng.wind.sensit, eng.lcm, snap = "near") 
updated_raster <- terra::resample(updated_raster, eng.lcm, method = "near")
writeRaster(updated_raster,  here("rasters", "opportunity masking", "r_wind_sensitivity_eng.tif"), overwrite=TRUE)

updated_raster <- terra::crop(nir.wind.sensit, nir.lcm, snap = "near") 
updated_raster <- terra::resample(updated_raster, nir.lcm, method = "near")
writeRaster(updated_raster,  here("rasters", "opportunity masking", "r_wind_sensitivity_nir.tif"), overwrite=TRUE)

updated_raster <- terra::crop(sct.wind.sensit, sct.lcm, snap = "near") 
updated_raster <- terra::resample(updated_raster, sct.lcm, method = "near")
writeRaster(updated_raster, here("rasters", "opportunity masking", "r_wind_sensitivity_sct.tif"), overwrite=TRUE)

updated_raster <- terra::crop(wal.wind.sensit, wal.lcm, snap = "near") 
updated_raster <- terra::resample(updated_raster, wal.lcm, method = "near")
writeRaster(updated_raster,  here("rasters", "opportunity masking", "r_wind_sensitivity_wal.tif"), overwrite=TRUE)


### CALCULATE MINIMUM SOLAR IRRADIANCE VALUE  -------------------------- 

# solar_farms <- read_csv(file.choose())

# solar <- solar_farms %>% 
#   filter(Installed %in% (1:50)) %>% 
#   drop_na(solar_val1)

# quantile(solar$solar_val1, probs = 0.03)

# new_solar <- solar %>% 
#   filter(solar_val1 >= 1075.154)
