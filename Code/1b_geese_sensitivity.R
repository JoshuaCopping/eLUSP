# 1b GEESE SENSITIVITY DATA ######################
source("./Code/0_setup.R")

# LOAD DATA -------------------------- 
# Spa features sheet
df_spa_data <- read_csv(here("data", "uk_spa_features.csv"))

# protected area database
v_spa <- st_read(here("vectors", "raw", "uk_spa.shp"))


# TIDY/FILTER DATA -------------------------- 
# Create species info/buffer distance dataframe
df_spa_info <- df_spa_data %>% 
  filter(str_detect(Common_name, c("swan", "goose"))) %>% 
  mutate(Buffer = case_when(Common_name == "Whooper swan" ~ 5,
                            Common_name == "Mute swan" ~ 5,
                            Common_name == "Tundra swan" ~ 5,
                            Common_name == "Greylag goose" ~ 20,
                            Common_name == "Pink-footed goose" ~ 20,
                            Common_name == "Greenland white-fronted goose" ~ 8,
                            Common_name == "Greater white-fronted goose" ~ 8,
                            Common_name == "Barnacle goose" ~ 15,
                            Common_name == "Taiga bean goose" ~ 9,
                            Common_name == "Dark-bellied brent goose" ~ 1.5,
                            Common_name == "Light-bellied brent goose" ~ 1.5,
                            TRUE ~ 0),
         Buffer = Buffer*1000) %>% 
  select(SITE_NAME, Species, Common_name, Buffer) %>% 
  group_by(SITE_NAME) %>% 
  filter(Buffer == max(Buffer))

## SPA vector data 
v_spa_tidy <- v_spa %>% 
  select(SITENAME) %>% 
  mutate(SITENAME = case_when(SITENAME == "Monach Isles" ~ "Monach Islands",
                              SITENAME == "Firth of Tay & Eden Estuary" ~ "Firth of Tay and Eden Estuary",
                              TRUE ~ SITENAME)) %>% 
  filter(SITENAME %in% df_spa_info$SITE_NAME) %>%
  left_join(df_spa_info, by = c("SITENAME" = "SITE_NAME"))


# BUFFER SPA SITES -------------------------- 
v_spa_buffer <- st_buffer(v_spa_tidy, v_spa_tidy$Buffer)

# convert to spatial* from sf 
v_spa_spat <- as_Spatial(v_spa_buffer, cast = TRUE, IDs = paste0("ID", seq_along(v_spa_buffer)))


# RASTERISE SPA SITES -------------------------- 
# load lcms
r_lcm_eng <- stack(here("rasters", "scenarios", "d", "r_lcm_d_eng.tif"))
r_lcm_nir <- stack(here("rasters", "scenarios", "d", "r_lcm_d_nir.tif"))
r_lcm_sct <- stack(here("rasters", "scenarios", "d", "r_lcm_d_sct.tif"))
r_lcm_wal <- stack(here("rasters", "scenarios", "d", "r_lcm_d_wal.tif"))

r_lcm_eng <- r_lcm_eng[[7]]
r_lcm_nir <- r_lcm_nir[[7]]
r_lcm_sct <- r_lcm_sct[[7]]
r_lcm_wal <- r_lcm_wal[[7]]

# Convert nir to bng
r_lcm_nir_bng <- projectRaster(r_lcm_nir, crs = crs(r_lcm_eng))

# Merge into 1
r_lcm_uk <- mosaic(r_lcm_eng, r_lcm_wal, r_lcm_sct, fun = min) 

# Convert to files terra format
t_lcm_uk <- rast(r_lcm_uk)
t_spa <- vect(v_spa_spat)

# Rasterize
r_spa_uk <- terra::rasterize(t_spa, t_lcm_uk)


# PREP FOR CROP & RESAMPLE SPAs -------------------------- 
# Split into 4 countries
# Load uk shape
gbr <- gadm_sp_loadCountries(fileNames = 'GBR', level = 1, basefile = './shapefiles')
gbr <- gbr[[2]]
# Project from wgs84 to british nation grid
gbr_bng <- spTransform(gbr, crs(r_spa_uk))

# Split uk shape
v_eng <- gbr_bng[gbr_bng@data$NAME_1 == "England",]
v_nir <- gbr_bng[gbr_bng@data$NAME_1 == "Northern Ireland",]
v_sct <- gbr_bng[gbr_bng@data$NAME_1 == "Scotland",]
v_wal <- gbr_bng[gbr_bng@data$NAME_1 == "Wales",]

# Convert to terra spatVector
sv_eng <- vect(v_eng)
sv_nir <- vect(v_nir)
sv_sct <- vect(v_sct)
sv_wal <- vect(v_wal)

# Mask rasterized SPAs for 4 countries
spa_eng <- terra::mask(r_spa_uk, sv_eng)
spa_nir <- terra::mask(r_spa_uk, sv_nir)
spa_sct <- terra::mask(r_spa_uk, sv_sct)
spa_wal <- terra::mask(r_spa_uk, sv_wal)

# Below converts to terra object for using 'near' method, for rasters with classes
t_lcm_eng <- rast(r_lcm_eng[[1]])
t_lcm_nir <- rast(r_lcm_nir[[1]])
t_lcm_sct <- rast(r_lcm_sct[[1]])
t_lcm_wal <- rast(r_lcm_wal[[1]])

# Reproject nir spa
spa_nir_ig <- terra::project(spa_nir, t_lcm_nir)


# CROP,RESAMPLE & SAVE SPAs -------------------------- 
spa_eng_update <- terra::crop(spa_eng, t_lcm_eng[[1]], snap = "near") 
spa_eng_update <- terra::resample(spa_eng_update, t_lcm_eng, method = "near")
writeRaster(spa_eng_update,  here("rasters", "opportunity masking", "r_geese_eng.tif"), overwrite=TRUE)

spa_nir_update <- terra::crop(spa_nir_ig, t_lcm_nir[[1]], snap = "near") 
spa_nir_update <- terra::resample(spa_nir_update, t_lcm_nir, method = "near")
writeRaster(spa_nir_update,  here("rasters", "opportunity masking", "r_geese_nir.tif"), overwrite=TRUE)

spa_sct_update <- terra::crop(spa_sct, t_lcm_sct[[1]], snap = "near") 
spa_sct_update <- terra::resample(spa_sct_update, t_lcm_sct, method = "near")
writeRaster(spa_sct_update,  here("rasters", "opportunity masking", "r_geese_sct.tif"), overwrite=TRUE)

spa_wal_update <- terra::crop(spa_wal, t_lcm_wal[[1]], snap = "near") 
spa_wal_update <- terra::resample(spa_wal_update, t_lcm_wal, method = "near")
writeRaster(spa_wal_update,  here("rasters", "opportunity masking", "r_geese_wal.tif"), overwrite=TRUE)

