# 2c MASKING CONIFER DATA ######################
source("./code/1_setup.R")

### NORTHERN IRELAND -------------------------
## LOAD DATA
# LCM
r_lcm_nir <- stack(here("rasters", "scenarios", "a", "r_lcm_a_nir.tif"))
r_lcm_nir <- r_lcm_nir[[1]]

# Woodland opportunity
r_woodop_nir <- raster(here("rasters", "opportunity masking", "r_woodop_nir.tif"))

# Constraints
v_cons_nir <- vect(here("vectors", "conifer masking", "allConstraints_merged_nir.shp"))

## RASTERIZE DATA
r_cons_nir <- rasterize(v_cons_nir, r_lcm_nir)

## MASK CONIFERS
# Create conifers layer
r_conifers_nir <- r_lcm_nir
r_conifers_nir[r_lcm_nir[] != 2] <- NA

# Mask conifers layer 
r_conifers_mask_nir <- r_conifers_nir
r_conifers_mask_nir[r_cons_nir[] == 1] <- NA

## UPDATE WOODOP
# Merge woodop and conifers mask
r_woodop_conifers_nir <- r_woodop_nir
r_woodop_conifers_nir[r_conifers_mask_nir[] == 2] <- 1

# Save 
writeRaster(r_woodop_conifers_nir, here("rasters", "opportunity masking", "r_woodop_conifers_nir.tif"), overwrite = TRUE)

# Remove layers
rm(v_cons_nir, r_lcm_nir, r_cons_nir, r_conifers_nir, r_conifers_mask_nir, r_woodop_nir, r_woodop_conifers_nir)
gc()


### WALES -------------------------
## LOAD DATA
# LCM
r_lcm_wal <- rast(here("rasters", "scenarios", "a", "r_lcm_a_wal.tif"))
r_lcm_wal <- r_lcm_wal[[1]]

# Woodland opportunity
r_woodop_wal <- raster(here("rasters", "opportunity masking", "r_woodop_wal.tif"))

# Constraints
v_cons_wal <- vect(here("vectors", "conifer masking", "allConstraints_merged_wal.shp"))

## RASTERIZE DATA
r_cons_wal <- rasterize(v_cons_wal, r_lcm_wal)

rm(v_cons_wal)
gc()

## MASK CONIFERS
# Create conifers layer
r_conifers_wal <- r_lcm_wal
r_conifers_wal[r_lcm_wal[] != 2] <- NA

# Mask conifers layer 
r_conifers_mask_wal <- r_conifers_wal
r_conifers_mask_wal[r_cons_wal[] == 1] <- NA

## UPDATE WOODOP
# Merge woodop and conifers mask
r_woodop_conifers_wal <- r_woodop_wal
r_woodop_conifers_wal[r_conifers_mask_wal[] == 2] <- 1

# Save 
writeRaster(r_woodop_conifers_wal, here("rasters", "opportunity masking", "r_woodop_conifers_wal.tif"), overwrite = TRUE)

# Remove layers
rm(r_lcm_wal, r_cons_wal, r_conifers_wal, r_conifers_mask_wal, r_woodop_wal, r_woodop_conifers_wal)
gc()


### SCOTLAND -------------------------
## LOAD DATA
# LCM
r_lcm_sct <- stack(here("rasters", "scenarios", "a", "r_lcm_a_sct.tif"))
r_lcm_sct <- r_lcm_sct[[1]]

# Woodland opportunity
r_woodop_sct <- raster(here("rasters", "opportunity masking", "r_woodop_sct.tif"))

# Constraints
r_peat_sct <- raster(here("rasters", "conifer masking", "r_peat_sct.tif"))
r_priority_sct <- raster(here("rasters", "conifer masking", "r_priorityHabitat_sct.tif"))
r_designated_sct <- raster(here("rasters", "conifer masking", "r_designated_sct.tif"))
r_monuments_sct <- raster(here("rasters", "conifer masking", "r_monuments_sct.tif"))
r_buildings_sct <- raster(here("rasters", "conifer masking", "r_buildings_sct.tif"))
r_roads_sct <- raster(here("rasters", "conifer masking", "r_RoadsRail_sct.tif"))
r_Lwater_sct <- raster(here("rasters", "conifer masking", "r_LinearWater_sct.tif"))
r_Swater_sct <- raster(here("rasters", "conifer masking", "r_SurfaceWater_sct.tif"))

## MASK CONIFERS
# Create conifers layer
r_conifers_sct <- r_lcm_sct
r_conifers_sct[r_lcm_sct[] != 2] <- NA

# Mask conifers layer 
r_conifers_mask_sct <- r_conifers_sct
r_conifers_mask_sct[r_peat_sct[] == 1] <- NA
r_conifers_mask_sct[r_priority_sct[] == 1] <- NA
r_conifers_mask_sct[r_designated_sct[] == 1] <- NA
r_conifers_mask_sct[r_monuments_sct[] == 1] <- NA
r_conifers_mask_sct[r_buildings_sct[] == 1] <- NA
r_conifers_mask_sct[r_roads_sct[] == 1] <- NA
r_conifers_mask_sct[r_Lwater_sct[] == 1] <- NA
r_conifers_mask_sct[r_Swater_sct[] == 1] <- NA

## UPDATE WOODOP
# Merge woodop and conifers mask
r_woodop_conifers_sct <- r_woodop_sct
r_woodop_conifers_sct[r_conifers_mask_sct[] == 2] <- 1

# Save 
writeRaster(r_woodop_conifers_sct, here("rasters", "opportunity masking", "r_woodop_conifers_sct.tif"), overwrite = TRUE)

# Remove layers
rm(list = ls())
gc()


### ENGLAND -------------------------
## LOAD DATA
# LCM
r_lcm_eng <- stack(here("rasters", "scenarios", "a", "r_lcm_a_eng.tif"))
r_lcm_eng <- r_lcm_eng[[1]]

# Woodland opportunity
r_woodop_eng <- raster(here("rasters", "opportunity masking", "r_woodop_eng.tif"))

# Constraints
r_peat_eng <- raster(here("rasters", "conifer masking", "r_peat_eng.tif"))
r_priority_eng <- raster(here("rasters", "conifer masking", "r_priorityHabitat_eng.tif"))
r_designated_eng <- raster(here("rasters", "conifer masking", "r_designated_eng.tif"))
r_monuments_eng <- raster(here("rasters", "conifer masking", "r_monuments_eng.tif"))
r_buildings_eng <- raster(here("rasters", "conifer masking", "r_buildings_eng.tif"))
r_roads_eng <- raster(here("rasters", "conifer masking", "r_RoadsRail_eng.tif"))
r_Lwater_eng <- raster(here("rasters", "conifer masking", "r_LinearWater_eng.tif"))
r_Swater_eng <- raster(here("rasters", "conifer masking", "r_SurfaceWater_eng.tif"))

## MASK CONIFERS
# Create conifers layer
r_conifers_eng <- r_lcm_eng
r_conifers_eng[r_lcm_eng[] != 2] <- NA

# Mask conifers layer 
r_conifers_mask_eng <- r_conifers_eng
r_conifers_mask_eng[r_peat_eng[] == 1] <- NA
r_conifers_mask_eng[r_priority_eng[] == 1] <- NA
r_conifers_mask_eng[r_designated_eng[] == 1] <- NA
r_conifers_mask_eng[r_monuments_eng[] == 1] <- NA
r_conifers_mask_eng[r_buildings_eng[] == 1] <- NA
r_conifers_mask_eng[r_roads_eng[] == 1] <- NA
r_conifers_mask_eng[r_Lwater_eng[] == 1] <- NA
r_conifers_mask_eng[r_Swater_eng[] == 1] <- NA

## UPDATE WOODOP
# Merge woodop and conifers mask
r_woodop_conifers_eng <- r_woodop_eng
r_woodop_conifers_eng[r_conifers_mask_eng[] == 2] <- 1

# Save 
writeRaster(r_woodop_conifers_eng, here("rasters", "opportunity masking", "r_woodop_conifers_eng.tif"), overwrite = TRUE)

