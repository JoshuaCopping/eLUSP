# 8c BIRDS SCENARIOS EXTRACT MAX WIND ######################
source("./Code/0_setup.R")


# READ BASELINE SPATIAL DATA  ---------------------------------------------
load(here("data", "raster_lookup_nuts1.RData"))
load(here("data", "raster_lookup_lcm.RData"))

# Add novel land covers
lookup_lcm <- lookup_lcm %>% 
  bind_rows(tibble(lcm_layer = c(2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 9.1, 10.1, 3.2, 4.2, 3.3, 4.3, 3.31, 4.31, 3.4, 4.4, 5.2, 6.2, 7.2, 22, 23, 23.1, 23.2, 23.3, 23.31, 23.4, 24, 24.1, 24.2, 24.3, 24.31, 24.4, 25),
                   lcm = c("c.w_pinewood", "a.h_silvoa", "i.g_silvop", "n.g_woodpa", "c.g_woodpa", "a.g_woodpa", "h.r_woodpa", "h.r_woodpa", "a.h_energy", "i.g_energy", "a.h_organic", "i.g_organic", "a.h_organic_silvoa", "i.g_organic_silvop", "a.h_palud", "i.g_palud", "n.g_new", "c.g_new", "a.g_new", "wind_c.w", "wind_a.h", "wind_a.h_silvoa", "wind_a.h_energy", "wind_a.h_organic", "wind_a.h_organic_silvoa", "wind_a.h_palud", "wind_i.g", "wind_i.g_silvop", "wind_i.g_energy", "wind_i.g_organic", "wind_i.g_organic_silvop", "wind_i.g_palud", "solar")))


# Read updated LCM rasters
r_lcm_eng <- raster(here("rasters", "get area data", "r_lcm_new_eng.tif"))
r_lcm_nir <- raster(here("rasters", "get area data", "r_lcm_new_nir.tif"))
r_lcm_sct <- raster(here("rasters", "get area data", "r_lcm_new_sct.tif"))
r_lcm_wal <- raster(here("rasters", "get area data", "r_lcm_new_wal.tif"))

# Read NUTS1 rasters
r_nuts1_eng <- raster(here("rasters", "get area data", "r_nuts1_eng.tif"))
r_nuts1_nir <- raster(here("rasters", "get area data", "r_nuts1_nir.tif"))
r_nuts1_sct <- raster(here("rasters", "get area data", "r_nuts1_sct.tif"))
r_nuts1_wal <- raster(here("rasters", "get area data", "r_nuts1_wal.tif"))

# Read hedge length raster
r_hedges_eng <- stack(here("rasters", "other", "r_hedges_eng.tif"))
r_hedges_nir <- stack(here("rasters", "other", "r_hedges_nir.tif"))
r_hedges_sct <- stack(here("rasters", "other", "r_hedges_sct.tif"))
r_hedges_wal <- stack(here("rasters", "other", "r_hedges_wal.tif"))

# Read island raster
r_island_eng <- stack(here("rasters", "other", "r_island_eng.tif"))
r_island_nir <- stack(here("rasters", "other", "r_island_nir.tif"))
r_island_sct <- stack(here("rasters", "other", "r_island_sct.tif"))
r_island_wal <- stack(here("rasters", "other", "r_island_wal.tif"))

# Read elevation raster
r_elev_eng <- stack(here("rasters", "other", "r_elev_eng.tif"))
r_elev_nir <- stack(here("rasters", "other", "r_elev_nir.tif"))
r_elev_sct <- stack(here("rasters", "other", "r_elev_sct.tif"))
r_elev_wal <- stack(here("rasters", "other", "r_elev_wal.tif"))

# Merge GB countries
r_lcm_gbr <- merge(merge(r_lcm_eng, r_lcm_wal), r_lcm_sct)
r_nuts1_gbr <- merge(merge(r_nuts1_eng, r_nuts1_wal), r_nuts1_sct)
r_hedges_gbr <- merge(merge(r_hedges_eng, r_hedges_wal), r_hedges_sct)
r_island_gbr <- merge(merge(r_island_eng, r_island_wal), r_island_sct)
r_elev_gbr <- merge(merge(r_elev_eng, r_elev_wal), r_elev_sct)


# RESCALE HEDGE COVER ---------------------------------------------
## Fix NIR
# Isolate missing areas
r_hedgesna_nir <- r_hedges_nir == -1
r_hedgesna_gbr <- r_hedges_gbr == -1 # should be none

# Fraction with no data (-1)
hedge_freq_nir <- freq(r_hedges_nir)$r_hedges_nir
hedge_missing_nir <- hedge_freq_nir %>% as.data.frame() %>% as.tbl() %>% 
  filter(!is.na(value)) %>% 
  summarise(prop_missing = count[value == -1] / sum(count))

# Replace no data (-1) with NA
r_hedges_nir[r_hedges_nir == -1] <- NA


## Get estimated baseline hedge length
hedge_length <- read_csv(here("data", "cs_hedge_length.csv"), skip = 1) %>% select(-notes) %>%
  mutate(country = ifelse(country == "Northern Ireland", country, "GB")) %>% 
  group_by(country) %>% 
  summarise(length = sum(length)) %>% 
  ungroup() %>% 
  mutate(pixels = c(cellStats(r_hedges_gbr[[1]], sum), # 13265839
                    (1 - hedge_missing_nir$prop_missing) * cellStats(r_hedges_nir[[1]], sum))) %>% # 886347.6
  mutate(km_per_pix = length/pixels)


## Rescale hedge length
r_hedges_gbr <- r_hedges_gbr * hedge_length$km_per_pix[hedge_length$country == "GB"] # 0.0531 / 0.0625 ha = 0.85 km / ha
r_hedges_nir <- r_hedges_nir * hedge_length$km_per_pix[hedge_length$country == "Northern Ireland"] # 0.13 / 0.0625 ha = 2.1 km / ha


# GET 1km grid and 2-km radius buffer  ---------------------------------------------
run <- FALSE

if(run){
  # Aggregate raster 40-fold, so 25m grid becomes sqm
  grid_sq_nir <- aggregate(!is.na(r_lcm_nir), 40, fun = sum)
  grid_sq_gbr <- aggregate(!is.na(r_lcm_gbr), 40, fun = sum)
  
  # Polygonise
  grid_sq_nir <- rasterToPolygons(grid_sq_nir/1600, fun = function(x)x>0)
  grid_sq_gbr <- rasterToPolygons(grid_sq_gbr/1600, fun = function(x)x>0)
  
  # Convert to SF
  grid_sq_nir <- st_as_sf(grid_sq_nir)
  grid_sq_gbr <- st_as_sf(grid_sq_gbr)
  
  # Add xy
  xy_nir <- st_coordinates(st_centroid(grid_sq_nir))
  xy_gbr <- st_coordinates(st_centroid(grid_sq_gbr))
  
  grid_sq_nir <- grid_sq_nir %>% mutate(x = xy_nir[,1], y = xy_nir[,2])
  grid_sq_gbr <- grid_sq_gbr %>% mutate(x = xy_gbr[,1], y = xy_gbr[,2])
  
  # 2000m buffer around centroid
  grid_buf_nir <- st_buffer(st_centroid(grid_sq_nir), dist = 2000, nQuadSegs = 10)
  grid_buf_gbr <- st_buffer(st_centroid(grid_sq_gbr), dist = 2000, nQuadSegs = 10)
  
  # Transform
  grid_sq_nir <- st_transform(grid_sq_nir, crs(r_lcm_nir))
  grid_sq_gbr <- st_transform(grid_sq_gbr, crs(r_lcm_gbr))
  grid_buf_nir <- st_transform(grid_buf_nir, crs(r_lcm_nir))
  grid_buf_gbr <- st_transform(grid_buf_gbr, crs(r_lcm_gbr))
  
  # Save
  st_write(grid_sq_nir, "rdata/sf/grid_sq_nir.shp", delete_dsn = TRUE)
  st_write(grid_sq_gbr, "rdata/sf/grid_sq_gbr.shp", delete_dsn = TRUE)
  st_write(grid_buf_nir, "rdata/sf/grid_buf_nir.shp", delete_dsn = TRUE)
  st_write(grid_buf_gbr, "rdata/sf/grid_buf_gbr.shp", delete_dsn = TRUE)
  
}

# Read
grid_sq_nir <- st_read(here("vectors", "bird_grids", "grid_sq_nir.shp")) %>% select(-layer, -x, -y)
grid_sq_gbr <- st_read(here("vectors", "bird_grids", "grid_sq_gbr.shp")) %>% select(-layer, -x, -y)
grid_buf_nir <- st_read(here("vectors", "bird_grids", "grid_buf_nir.shp")) %>% select(-layer, -x, -y)
grid_buf_gbr <- st_read(here("vectors", "bird_grids", "grid_buf_gbr.shp")) %>% select(-layer, -x, -y)



## EXTRACT LAND COVER ------------------------------
# Function for xtracting LCM coverage in polygon 
mode_fun <- function(x, y) {
  x = x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

## Fun
# squares <- grid_sq_nir
# buffers <- grid_buf_nir
# lcm <- raster(here("rasters", "updated lcm", "a", "r_renewable_lcm_nir_a_high.tif") band = 7)
# nuts1 <- r_nuts1_nir
# elev <- r_elev_nir
# hedge <- r_hedges_nir
# hedgena <- r_hedgesna_nir
# island <- r_island_nir
# lookup_lcm
# lookup_nuts1


get_covars_fun <- function(squares, buffers, lcm, nuts1, elev, hedge, hedgena, island, lookup_lcm, lookup_nuts1){
  # XY
  df_xy <- st_coordinates(st_centroid(st_transform(squares, 27700))) %>%
    as.data.frame() %>% 
    as_tibble() %>% 
    set_names(c("x", "y"))
  
  # Mean altitude
  df_elev <- exactextractr::exact_extract(elev, st_transform(squares, crs(lcm)), 'mean')
  
  # Modal island/mainland
  df_island <- exactextractr::exact_extract(island, st_transform(squares, crs(lcm)), mode_fun)
  
  # Modal NUTS1
  df_nuts1 <- squares %>% 
    st_set_geometry(NULL) %>% 
    mutate(nuts1 = exact_extract(nuts1, st_transform(squares, crs(lcm)), fun = mode_fun)) %>% 
    rename(nuts1_layer = nuts1) %>% 
    left_join(lookup_nuts1, by = "nuts1_layer") %>% 
    select(-nuts1_layer)
  
  # LCM 1km
  df_lcm_sq <- exactextractr::exact_extract(lcm, st_transform(squares, crs(lcm)))
  df_lcm_sq <- df_lcm_sq %>% 
    enframe("id") %>% 
    mutate(value = purrr::map(value, ~{.x %>% 
        group_by(value) %>% 
        summarise(prop = sum(coverage_fraction), .groups = "drop")})) %>% 
    unnest(cols = c('value')) %>%
    rename(lcm_layer = value) %>% 
    mutate(lcm_layer = round(lcm_layer, 2)) %>% 
    left_join(lookup_lcm, by = "lcm_layer") %>% 
    group_by(id, lcm) %>% 
    summarise(prop = sum(prop), .groups = "drop") %>% 
    group_by(id) %>% 
    mutate(prop = prop / sum(prop)) %>% 
    ungroup() %>% 
    spread(lcm, prop, fill = 0) %>% 
    select(-`<NA>`) %>% 
    mutate_all(list(~ifelse(is.na(.), 0, .))) %>%
    set_names(paste0(names(.), "_sq")) %>% 
    rename(id = id_sq)        
  
  # LCM 2km
  df_lcm_buf <- exactextractr::exact_extract(lcm, st_transform(buffers, crs(lcm)))  
  df_lcm_buf <- df_lcm_buf %>% 
    enframe("id") %>% 
    mutate(value = purrr::map(value, ~{.x %>% 
        group_by(value) %>% 
        summarise(prop = sum(coverage_fraction)) %>% 
        ungroup()})) %>% 
    unnest(cols = c('value')) %>%
    rename(lcm_layer = value) %>% 
    mutate(lcm_layer = round(lcm_layer, 2)) %>% 
    left_join(lookup_lcm, by = "lcm_layer") %>% 
    mutate(lcm = case_when(str_detect(lcm, "wind") ~ str_c(str_remove(lcm, "wind_"), "_wind"),
                           lcm == "solar" ~ "i.g",
                           TRUE ~ lcm)) %>%
    mutate(lcm = substr(lcm, 1, 3)) %>% # Use simplified LCM classes for buffer
    group_by(id, lcm) %>% 
    summarise(prop = sum(prop), .groups = "drop") %>% 
    group_by(id) %>% 
    mutate(prop = prop / sum(prop)) %>% 
    ungroup() %>% 
    spread(lcm, prop, fill = 0) %>% 
    select(-`<NA>`) %>% 
    mutate_all(list(~ifelse(is.na(.), 0, .))) %>%
    set_names(paste0(names(.), "_buf")) %>% 
    rename(id = id_buf)
  
  # Hedge (sq)
  df_hedge <- exactextractr::exact_extract(hedge, st_transform(squares, crs(lcm)), 'sum')
  df_hedge <- tibble(hedge_total = df_hedge)
  
  df_hedgena <- exactextractr::exact_extract(hedgena, st_transform(squares, crs(lcm)), 'sum')
  df_hedgena <- tibble(hedgena_total = df_hedgena)
  
  
  # Combine & return
  return(bind_cols(as_tibble(df_nuts1),
                   df_xy,
                   elev = df_elev,
                   island = df_island,
                   df_hedge,
                   df_hedgena,
                   df_lcm_sq %>% select(-id),
                   df_lcm_buf %>% select(-id)))
}



# Do NIR  
bbs_covars_nir <- tibble(file = c("rasters/get area data/r_lcm_new_nir.tif",
                                  "rasters/max wind/r_renewable_lcm_nir_i_np_max.tif"),
                         band = c(1, 7),
                         scenario = c("2015", "i_np_max")) %>% 
  mutate(raster = purrr::map2(file, band, ~raster(x = .x, band = .y)),
         covars = purrr::map(raster, ~get_covars_fun(grid_sq_nir, grid_buf_nir, 
                                                     ., r_nuts1_nir, r_elev_nir, r_hedges_nir, r_hedgesna_nir, r_island_nir, 
                                                     lookup_lcm, lookup_nuts1)))

# Progress update
print(paste("Finished NIR", Sys.time()))


# Do GBR (merge eng, sct & wal) 
bbs_covars_gbr <- tibble(file = list(list("rasters/get area data/r_lcm_new_eng.tif",
                                          "rasters/get area data/r_lcm_new_sct.tif",
                                          "rasters/get area data/r_lcm_new_wal.tif"),
                                     list("rasters/max wind/r_renewable_lcm_eng_i_np_max.tif",
                                          "rasters/max wind/r_renewable_lcm_sct_i_np_max.tif",
                                          "rasters/max wind/r_renewable_lcm_wal_i_np_max.tif")),
                         band = c(1, 7),
                         scenario = c("2015", "i_np_max")) %>% 
  mutate(raster = purrr::map2(file, band, ~do.call(merge, lapply(.x, raster, band = .y))),
         covars = purrr::map(raster, ~get_covars_fun(grid_sq_gbr, grid_buf_gbr, 
                                                     ., r_nuts1_gbr, r_elev_gbr, r_hedges_gbr, r_hedgesna_gbr, r_island_gbr, 
                                                     lookup_lcm, lookup_nuts1)))

# Progress update
print(paste("Finished GBR", Sys.time()))

# SAVE 
save(bbs_covars_nir, bbs_covars_gbr, file = here("outputs", "bbs_scenarios_covars_max_wind.RData"))
removeTmpFiles(h = 0)

