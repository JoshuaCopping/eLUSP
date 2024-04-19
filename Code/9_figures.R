# 9 FIGURES & TABLES FOR PAPER ######################
source("./Code/0_setup.R")


# FIGURE 1 - LAND & ENERGY ---------------------------------------------
df_landcover_area <- read_csv(here("outputs", "df_renewable_landcover_area.csv"))
load(here("outputs", "scenario areas", "scenarios_areas_elusp.RData")) 
load(here("data", "raster_lookup_alc.RData"))
ambition_combos <- read_csv(here("data", "ambition_combinations_lusp.csv")) # Ambition combos for names 

colour_pal <- c("darkorange2", "cyan4")

areas_data <- 
  scenarios_areas_elusp %>%
  filter(year == 2050) %>% 
  mutate(lcm = case_when(grepl("wind", lcm) ~ "wind",
                         TRUE ~ lcm)) %>% 
  group_by(scenario, lcm) %>%
  summarise(area = sum(area)) %>% 
  mutate(tot_uk_area = sum(area)) %>% 
  filter(grepl("wind|solar", lcm)) %>% 
  rename("opportunity_type" = "lcm",
         "installed_area" = "area") %>% 
  mutate(opp_scenario = case_when(str_detect(scenario, "high") ~ str_remove(scenario, "_high"),
                                  str_detect(scenario, "low") ~ str_remove(scenario, "_low"))) %>% 
  left_join(df_landcover_area %>% 
              mutate(scenario = case_when(national_parks == "masked" ~ paste0(scenario, "_np"),
                                          TRUE ~ scenario)) %>% 
              group_by(country, scenario, national_parks, opportunity_type, alc) %>%
              summarise(area = sum(ha)) %>% 
              select(country, scenario, alc, area, opportunity_type) %>% 
              mutate(land_type = case_when(country %in% c("England", "Northern Ireland", "Wales") & alc < 4 ~ "Best & most versatile land",
                                           country %in% c("Scotland") & alc < 3.2 ~ "Best & most versatile land",
                                           TRUE ~ "Less productive land")) %>% 
              group_by(scenario, opportunity_type, land_type) %>%
              summarise(opportunity_area = sum(area)) %>% 
              rename("opp_scenario" = scenario) %>% 
              group_by(opp_scenario, opportunity_type) %>% 
              mutate(total_opp = sum(opportunity_area)) %>% 
              ungroup(),
            by = c("opp_scenario", "opportunity_type")) %>% 
  select(-opp_scenario) %>%
  mutate(installed_opp_perc = installed_area/total_opp*100,
         opp_uk_perc = opportunity_area/tot_uk_area*100,
         total_opp_uk_perc = total_opp/tot_uk_area*100,
         lusp_scenario = str_sub(scenario, 1, 1)) %>%
  # SCENARIO NAMES
  left_join(ambition_combos %>% 
              select(scenario, name) %>% 
              mutate(scenario = letters[row_number()],
                     name = case_when(name == "BAU" ~ "Baseline",
                                      name == "Widespread Engagement" ~ "WidespreadEngagement",
                                      name == "NbS-bioenergy" ~ "NbSNoBiomass",
                                      name == "NbS+organic" ~ "NbSOrganic",
                                      name == "NbS+HNVf" ~ "NBS Extra",
                                      name == "OnFarmSharing" ~ "OnFarmOrganic",
                                      TRUE ~ name)),
            by = c("lusp_scenario" = "scenario")) %>% 
  mutate(energy_ambition = case_when(str_detect(scenario, "high") ~ str_to_title(str_sub(scenario, -4)),
                                     str_detect(scenario, "low") ~ str_to_title(str_sub(scenario, -3)),
                                     TRUE ~ "LUSP"),
         name_energy = str_c(name, energy_ambition, sep = " - "),
         name = factor(name,
                       levels = c("Baseline", "BNZP", "WidespreadEngagement", "NbS", "NbSNoBiomass", "NbSOrganic", "NBS Extra", "OnFarmOrganic", "OnFarmBNZP"),
                       ordered = TRUE)) %>% 
  arrange(name) %>% 
  ungroup() %>% 
  mutate(id = row_number(),
         opportunity_type = str_to_title(opportunity_type)) %>% 
  filter(str_detect(scenario, "np")) %>% filter(name %in% c("Baseline", "NBS Extra"))


# Plot 
ggplot() + 
  geom_col(data = areas_data,
           aes(x = opportunity_area/10e5, y = reorder(name_energy, -id),
               fill = land_type),
           width = 0.8) +
  geom_text(data = areas_data %>% filter(land_type == "Less productive land" & opportunity_type == "Wind"),
            aes(x = (total_opp/10e5)*1.04, y = reorder(name_energy, -id),
                label = paste0(sprintf('%.1f', total_opp_uk_perc), "%")),
            size = 3,
            hjust = 0,
            colour = "grey65",
            fontface = "bold") +
  geom_text(data = areas_data %>% filter(land_type == "Less productive land" & opportunity_type == "Solar"),
            aes(x = (total_opp/10e5)*1.3, y = reorder(name_energy, -id),
                label = paste0(sprintf('%.1f', total_opp_uk_perc), "%")),
            size = 3,
            hjust = 0,
            colour = "grey65",
            fontface = "bold") +
  scale_fill_manual(values = c("grey90", "grey82")) +
  new_scale("fill") +
  geom_col(data = areas_data %>% filter(land_type == "Less productive land"),
           aes(x = installed_area/10e5, y = reorder(name_energy, -id),
               fill = name),
           width = 0.8,
           alpha = 0.8) +
  geom_text(data = areas_data %>% filter(land_type == "Less productive land"),
            aes(x = (installed_area/10e5)*1.2, y = reorder(name_energy, -id),
                label = paste0(sprintf('%.1f', installed_opp_perc), "%"),
                colour = name),
            size = 3,
            hjust = 0,
            fontface = "bold",
            show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)),
                     breaks = seq(0, 7, by = 1),
                     minor_breaks = seq(0, 7, by = 0.1)) +
  scale_fill_manual(values = colour_pal) +
  scale_colour_manual(values = colour_pal) +
  labs(x = "Area (million ha)") + 
  facet_wrap(~opportunity_type) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 8, 
                                    colour = "black"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, 
                                 colour = "black"),
        strip.text = element_text(size = 8,
                                  colour = "black",
                                  face = "bold"),
        plot.margin = margin(10, 20, 5, 10),
        panel.spacing.x = unit(1.2, "lines"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size = 0.3)) 


ggsave(here("plots", "Paper Figures", "Figure_1.png"), 
       width = 172, 
       height = 86,
       units = "mm",
       dpi = 300)


# FIGURE 2 - FRACTIONAL MAPS -------------------
# Load & tidy rasters
r_baseline_low_eng <- rast(here("rasters", "updated lcm", "a", "r_renewable_lcm_eng_a_np_low.tif"), lyrs = 7)
r_baseline_low_nir <- rast(here("rasters", "updated lcm", "a", "r_renewable_lcm_nir_a_np_low.tif"), lyrs = 7)
r_baseline_low_sct <- rast(here("rasters", "updated lcm", "a", "r_renewable_lcm_sct_a_np_low.tif"), lyrs = 7)
r_baseline_low_wal <- rast(here("rasters", "updated lcm", "a", "r_renewable_lcm_wal_a_np_low.tif"), lyrs = 7)

r_baseline_high_eng <- rast(here("rasters", "updated lcm", "a", "r_renewable_lcm_eng_a_np_high.tif"), lyrs = 7)
r_baseline_high_nir <- rast(here("rasters", "updated lcm", "a", "r_renewable_lcm_nir_a_np_high.tif"), lyrs = 7)
r_baseline_high_sct <- rast(here("rasters", "updated lcm", "a", "r_renewable_lcm_sct_a_np_high.tif"), lyrs = 7)
r_baseline_high_wal <- rast(here("rasters", "updated lcm", "a", "r_renewable_lcm_wal_a_np_high.tif"), lyrs = 7)

r_nbs_low_eng <- rast(here("rasters", "updated lcm", "i", "r_renewable_lcm_eng_i_np_low.tif"), lyrs = 7)
r_nbs_low_nir <- rast(here("rasters", "updated lcm", "i", "r_renewable_lcm_nir_i_np_low.tif"), lyrs = 7)
r_nbs_low_sct <- rast(here("rasters", "updated lcm", "i", "r_renewable_lcm_sct_i_np_low.tif"), lyrs = 7)
r_nbs_low_wal <- rast(here("rasters", "updated lcm", "i", "r_renewable_lcm_wal_i_np_low.tif"), lyrs = 7)

r_nbs_high_eng <- rast(here("rasters", "updated lcm", "i", "r_renewable_lcm_eng_i_np_high.tif"), lyrs = 7)
r_nbs_high_nir <- rast(here("rasters", "updated lcm", "i", "r_renewable_lcm_nir_i_np_high.tif"), lyrs = 7)
r_nbs_high_sct <- rast(here("rasters", "updated lcm", "i", "r_renewable_lcm_sct_i_np_high.tif"), lyrs = 7)
r_nbs_high_wal <- rast(here("rasters", "updated lcm", "i", "r_renewable_lcm_wal_i_np_high.tif"), lyrs = 7)

# Reproject NIR
r_baseline_low_nir_bng <- project(r_baseline_low_nir, crs(r_baseline_low_eng), res = 25)
r_baseline_high_nir_bng <- project(r_baseline_high_nir, crs(r_baseline_low_eng), res = 25)
r_nbs_low_nir_bng <- project(r_nbs_low_nir, crs(r_baseline_low_eng), res = 25)
r_nbs_high_nir_bng <- project(r_nbs_high_nir, crs(r_baseline_low_eng), res = 25)

# Merge
r_baseline_low <- terra::merge(r_baseline_low_eng, r_baseline_low_nir_bng, r_baseline_low_sct, r_baseline_low_wal)
r_baseline_high <- terra::merge(r_baseline_high_eng, r_baseline_high_nir_bng, r_baseline_high_sct, r_baseline_high_wal)
r_nbs_low <- terra::merge(r_nbs_low_eng, r_nbs_low_nir_bng, r_nbs_low_sct, r_nbs_low_wal)
r_nbs_high <- terra::merge(r_nbs_high_eng, r_nbs_low_nir_bng, r_nbs_low_sct, r_nbs_low_wal)

# Save
writeRaster(r_baseline_low, here("rasters", "merged", "baseline_low.tif"), overwrite = TRUE)
writeRaster(r_baseline_high, here("rasters", "merged", "baseline_high.tif"), overwrite = TRUE)
writeRaster(r_nbs_low, here("rasters", "merged", "nbs_low.tif"), overwrite = TRUE)
writeRaster(r_nbs_high, here("rasters", "merged", "nbs_high.tif"), overwrite = TRUE)
rm(list = ls()); gc()

# GET FRACTIONAL VALUES
# Load
r_baseline_low <- raster(here("rasters", "merged", "baseline_low.tif"))
r_baseline_high <- raster(here("rasters", "merged", "baseline_high.tif"))
r_nbs_low <- raster(here("rasters", "merged", "nbs_low.tif"))
r_nbs_high <- raster(here("rasters", "merged", "nbs_high.tif"))

# Shapefiles
v_uk <- st_read(here("vectors", "gadm36_GBR_shp", "gadm36_GBR_0.shp")) %>% st_transform(27700)

# Lookup
load(here("data", "raster_lookup_lcm.RData"))
# Add novel land covers
lookup_lcm <- lookup_lcm %>%
  bind_rows(tibble(lcm_layer = c(2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 9.1, 10.1, 3.2, 4.2, 3.3, 4.3, 3.31, 4.31, 3.4, 4.4, 5.2, 6.2, 7.2, 22, 23, 23.1, 23.2, 23.3, 23.31, 23.4, 24, 24.1, 24.2, 24.3, 24.31, 24.4, 25),
                   lcm = c("c.w_pinewood", "a.h_silvoa", "i.g_silvop", "n.g_woodpa", "c.g_woodpa", "a.g_woodpa", "h.r_woodpa", "h.r_woodpa", "a.h_energy", "i.g_energy", "a.h_organic", "i.g_organic", "a.h_organic_silvoa", "i.g_organic_silvop", "a.h_palud", "i.g_palud", "n.g_new", "c.g_new", "a.g_new", "wind_c.w", "wind_a.h", "wind_a.h_silvoa", "wind_a.h_energy", "wind_a.h_organic", "wind_a.h_organic_silvoa", "wind_a.h_palud", "wind_i.g", "wind_i.g_silvop", "wind_i.g_energy", "wind_i.g_organic", "wind_i.g_organic_silvop", "wind_i.g_palud", "solar")))

# Create grid
v_grid <- st_make_grid(v_uk, cellsize = 20000) %>%
  st_as_sf() %>%
  mutate(id = row_number())

# Stack
lcm_stack <- stack(r_baseline_low, r_baseline_high, r_nbs_low, r_nbs_high)


# Loop over each raster layer
v_fracs <- tibble(id = as.integer(),
                  wind = as.double(),
                  solar = as.double(),
                  scenario = as.character())


for (i in 1:nlayers(lcm_stack)) {

  # Extract values
  df_fracs <- exactextractr::exact_extract(lcm_stack[[i]], v_grid); gc()

  df_fracs <- df_fracs %>%
    enframe("id") %>%
    mutate(value = purrr::map(value, ~{.x %>%
        group_by(value) %>%
        summarise(prop = sum(coverage_fraction)) %>%
        ungroup()})) %>%
    unnest(cols = c('value')) %>%
    rename(lcm_layer = value) %>%
    left_join(lookup_lcm, by = "lcm_layer") %>%
    group_by(id, lcm) %>%
    summarise(prop = sum(prop), .groups = "drop") %>%
    group_by(id) %>%
    mutate(prop = prop / sum(prop)) %>%
    ungroup() %>%
    spread(lcm, prop, fill = 0) %>%
    mutate_all(list(~ifelse(is.na(.), 0, .))); gc()

  # Tidy
  v_fracs_loop <- df_fracs %>%
    select(id, contains("wind"), solar) %>%
    replace_na(list(wind_a.h = 0, wind_c.w = 0, wind_i.g = 0)) %>%
    group_by(id) %>%
    mutate(wind = sum(wind_a.h, wind_c.w, wind_i.g)) %>%
    ungroup() %>%
    select(id, wind, solar) %>%
    mutate(scenario = names(lcm_stack[[i]]))

  v_fracs <- bind_rows(v_fracs, v_fracs_loop)

}


# Add fractions to v_grid data
v_fracs_tidy <- v_grid %>%
  left_join(v_fracs %>%
              select(id, wind, solar, scenario),
            by = "id") %>%
  replace_na(list(wind = 0, solar = 0)) %>%
  filter(!is.na(scenario)) %>%
  mutate(name = case_when(str_detect(scenario, "_a_") ~ "Baseline",
                          str_detect(scenario, "_i_") ~ "NBS Extra"),
         ambition = case_when(str_detect(scenario, "low") ~ "Low",
                              str_detect(scenario, "high") ~ "High"),
         scenario = str_to_lower(str_c(name, ambition, sep = "_"))) %>%
  filter(wind > 0 & solar > 0)


# Save
save(v_fracs_tidy, file = here("outputs", "df_fracs.RData"))


# FRACTIONAL PLOTS
# Load
load(here("outputs", "df_fracs.RData"))
v_uk <- st_read(here("vectors", "gadm36_GBR_shp", "gadm36_GBR_0.shp")) %>% st_transform(27700) 

# Plotting 
#install.packages("biscale")
library(biscale)


# Cut data
breaks_wind <- classInt::classIntervals(v_fracs_tidy$wind[v_fracs_tidy$scenario == "baseline_high"], n = 3, style = "kmeans")$brks
breaks_solar <- classInt::classIntervals(v_fracs_tidy$solar[v_fracs_tidy$scenario == "baseline_high"], n = 3, style = "kmeans")$brks

breaks_solar2 <-round(breaks_solar, 2)
breaks_wind2 <-round(breaks_wind, 2)

# Add breaks to v_frac_tidy data
v_fracs_tidy$solar_bin <- cut(v_fracs_tidy$solar, breaks = breaks_solar2, include.lowest = TRUE)
v_fracs_tidy$wind_bin <- cut(v_fracs_tidy$wind, breaks = breaks_wind2, include.lowest = TRUE)

# Create bi_class object
bi_dat <- bi_class(v_fracs_tidy, x = solar_bin, y = wind_bin, style = "quantile", dim = 3)

# Plot
bi_plot <- bi_dat %>% 
  #filter(scenario == "baseline_high") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = bi_class), 
          color = NA,
          lwd = NA,
          show.legend = FALSE) +
  geom_sf(data = v_uk,
          fill = NA,
          lwd = 0.05,
          colour = "black") +
  bi_scale_fill(pal = "BlueYl", dim = 3, na.value = "transparent") +
  coord_sf() + 
  scale_x_continuous(limits = c(-80000, 740000),
                     sec.axis = sec_axis(~ . , name = "Scenario", breaks = NULL, labels = NULL)) + 
  scale_y_continuous(limits = c(5400, 1222000),
                     sec.axis = sec_axis(~ . , name = "Energy Ambition", breaks = NULL, labels = NULL)) +
  facet_grid(vars(ambition), vars(name)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = 8,
                                  colour = "black",
                                  face = "bold"),
        axis.text = element_text(colour = "black",
                                 size = 7),
        axis.title.x.top = element_text(colour = "black",
                                        size = 10,
                                        face = "bold",
                                        margin = margin(10,0,5,0)),
        axis.title.y.right = element_text(colour = "black",
                                          size = 10,
                                          face = "bold",
                                          margin = margin(0,0,0,10)),
        panel.spacing.x = unit(1, "lines")) 


# legend 
breaks3 <- list(bi_x = breaks_solar2 * 100,
                bi_y = breaks_wind2 * 100)

legend2 <- 
bi_legend(pal = "BlueYl",
          xlab = "% Solar",
          ylab = "% Wind",
          size = 12,
          dim = 3,
          breaks = breaks3,
          arrows = TRUE) +
  theme(axis.text = element_text(colour = "black",
                                 size = 8),
        axis.title = element_text(colour = "black",
                                  size = 9),
        plot.margin = margin(0, 10, 0, 50)) 


# Join plot and legend
legend2 + bi_plot + 
  plot_layout(widths = c(0.2, 1))

# Save
ggsave(here("plots", "Paper Figures", "Figure_2.png"), 
       width = 172, 
       height = 130,
       units = "mm",
       dpi = 300)



# FIGURE 3 - GHGs ---------------------------------------------

load(here("outputs", "gwp_combined.RData")) # GHGs
ambition_combos <- read_csv(here("data", "ambition_combinations_lusp.csv")) # Ambition combos for names 

colour_pal <- c("darkorange2", "cyan4")


gwp_combined$gwp_combined %>% 
  filter(year == 2050) %>% 
  filter(ag_sector == 1 | is.na(ag_sector)) %>% 
  group_by(scenario, lusp_scenario) %>% 
  summarise(total_gwp = sum(gwp_t)) %>% 
  ungroup() %>%
  left_join(gwp_combined$gwp_combined %>% 
              filter(year == 2015 & scenario == "a") %>% 
              filter(ag_sector == 1 | is.na(ag_sector)) %>% 
              summarise(base_gwp = sum(gwp_t)/1e6) %>% 
              crossing(scenario = unique(gwp_combined$gwp_combined$scenario)), by = "scenario")%>% 
  mutate(total_gwp = total_gwp/1e6,
         rel_emissions = (total_gwp-base_gwp)/base_gwp) %>% 
  mutate(lusp_scenario = str_sub(scenario, 1,1),
         national_park = case_when(str_detect(scenario, "np") ~ "excluded",
                                   !str_detect(scenario, "np") & nchar(scenario) > 1 ~ "included",
                                   TRUE ~ "None"),
         energy_ambition = case_when(str_detect(scenario, "high") ~ str_to_title(str_sub(scenario, -4)),
                                     str_detect(scenario, "low") ~ str_to_title(str_sub(scenario, -3)),
                                     TRUE ~ "None")) %>%
  group_by(lusp_scenario) %>%
  mutate(lusp_emissions = first(rel_emissions)) %>% 
  ungroup() %>% 
  # SCENARIO NAMES
  left_join(ambition_combos %>% 
              select(scenario, name) %>% 
              mutate(scenario = letters[row_number()],
                     name = case_when(name == "BAU" ~ "Baseline",
                                      name == "Widespread Engagement" ~ "WidespreadEngagement",
                                      name == "NbS-bioenergy" ~ "NbSNoBiomass",
                                      name == "NbS+organic" ~ "NbSOrganic",
                                      name == "NbS+HNVf" ~ "NBS Extra",
                                      name == "OnFarmSharing" ~ "OnFarmOrganic",
                                      TRUE ~ name)),
            by = c("lusp_scenario" = "scenario")) %>% 
  mutate(name_energy = str_c(name, energy_ambition, sep = " - "),
         name = factor(name,
                       levels = c("Baseline", "BNZP", "WidespreadEngagement", "NbS", "NbSNoBiomass", "NbSOrganic", "NBS Extra", "OnFarmOrganic", "OnFarmBNZP"),
                       ordered = TRUE)) %>% 
  arrange(name) %>% 
  mutate(direction = case_when(rel_emissions > 0 ~ 1,
                               rel_emissions < 0 ~ -1,
                               TRUE ~ 0),
         label = case_when(direction == 1 ~  paste0("+", sprintf('%.2f', rel_emissions*100), "%"),
                           direction == -1 ~ paste0(sprintf('%.2f', rel_emissions*100), "%"))) %>% 
  filter(str_detect(scenario, "np") | !str_detect(scenario, "_"),
         name %in% c("Baseline", "NBS Extra")) %>% 
  ungroup() %>% 
  filter(!str_detect(scenario, "max")) %>% 
  arrange(name, desc(energy_ambition)) %>% 
  mutate(id = row_number()) %>%  
  # PLOT
  ggplot(aes(x = reorder(name_energy, -id))) +
  geom_col(aes(y = rel_emissions,
               fill = lusp_scenario),
           alpha = 0.8,
           width = 0.8)+
  annotate("segment", x = 0.5, xend = 6.5, y = 0, yend = 0,
           linetype = "dashed",
           colour = "grey20",
           size = 0.45) +
  geom_shadowtext(aes(y = rel_emissions-direction*-0.09,
                      label = label,
                      colour = lusp_scenario),
                  bg.colour = "white",
                  fontface = "bold",
                  size = 3,
                  hjust = 0.5) +
  coord_flip() + 
  scale_y_continuous(limits = c(-1.25, 0.52),
                     breaks = seq(-1.2, 0.4, by = 0.2),
                     expand = c(0, 0),
                     labels = scales::percent_format(accuracy = 1L)) +
  scale_fill_manual(values = colour_pal) +
  scale_colour_manual(values = colour_pal) +
  labs(y = "Percentage change in AFOLU GHG emissions, relative to 2015") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(size = 8, 
                                    colour = "black"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, 
                                 colour = "black"),
        plot.margin = margin(10, 20, 5, 10),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size = 0.3)) 

ggsave(here("plots", "Paper Figures", "Figure_3.png"), 
       width = 172, 
       height = 86,
       units = "mm",
       dpi = 300)


# FIGURE 4 - FOOD ---------------------------------------------

load(here("outputs", "food_results_basic_all.RData")) # Food
ambition_combos <- read_csv(here("data", "ambition_combinations_lusp.csv")) # Ambition combos for names 


colour_pal <- c("darkorange2", "cyan4")


food_results_basic %>%
  # Grab first item
  mutate(food_type_sum = purrr::map(food, ~.[[1]])) %>%
  select(-food) %>%
  unnest(cols = c(data, food_type_sum)) %>% # View
  group_by(scenario, year) %>%
  summarise(energy = sum(energy)) %>%
  ungroup() %>%
  mutate(rel_energy = energy / energy[year == 2015]) %>%
  filter(year == 2050) %>% 
  mutate(lusp_scenario = str_sub(scenario, 1,1),
         national_park = case_when(str_detect(scenario, "np") ~ "excluded",
                                   !str_detect(scenario, "np") & nchar(scenario) > 1 ~ "included",
                                   TRUE ~ "None"),
         energy_ambition = case_when(str_detect(scenario, "high") ~ str_to_title(str_sub(scenario, -4)),
                                     str_detect(scenario, "low") ~ str_to_title(str_sub(scenario, -3)),
                                     TRUE ~ "None")) %>%
  group_by(lusp_scenario) %>%
  mutate(lusp_energy = first(rel_energy)) %>% 
  ungroup() %>% 
  # SCENARIO NAMES
  left_join(ambition_combos %>% 
              select(scenario, name) %>% 
              mutate(scenario = letters[row_number()],
                     name = case_when(name == "BAU" ~ "Baseline",
                                      name == "Widespread Engagement" ~ "WidespreadEngagement",
                                      name == "NbS-bioenergy" ~ "NbSNoBiomass",
                                      name == "NbS+organic" ~ "NbSOrganic",
                                      name == "NbS+HNVf" ~ "NBS Extra",
                                      name == "OnFarmSharing" ~ "OnFarmOrganic",
                                      TRUE ~ name)),
            by = c("lusp_scenario" = "scenario")) %>% 
  mutate(name_energy = str_c(name, energy_ambition, sep = " - "),
         name = factor(name,
                       levels = c("Baseline", "BNZP", "WidespreadEngagement", "NbS", "NbSNoBiomass", "NbSOrganic", "NBS Extra", "OnFarmOrganic", "OnFarmBNZP"),
                       ordered = TRUE)) %>% 
  mutate(rel_energy = rel_energy-1,
         lusp_energy = lusp_energy-1,
         energy_label = rel_energy*100) %>% 
  filter(str_detect(scenario, "np") | !str_detect(scenario, "_"),
         name %in% c("Baseline", "NBS Extra")) %>% 
  ungroup() %>% 
  filter(!str_detect(scenario, "max")) %>% 
  arrange(name, desc(energy_ambition)) %>% 
  mutate(id = row_number()) %>%  
  # PLOT
  ggplot(aes(x = reorder(name_energy, -id))) +
  geom_col(aes(y = rel_energy,
               fill = lusp_scenario),
           alpha = 0.8,
           width = 0.8) +
  annotate("segment", x = 0.5, xend = 6.5, y = 0, yend = 0,
           size = 0.45,
           linetype = "dashed",
           colour = "grey20") +
  geom_shadowtext(aes(y = rel_energy,
                      label = paste0(sprintf('%.2f', energy_label), "%"),
                      colour = lusp_scenario),
                  bg.colour = "white",
                  fontface = "bold",
                  size = 3,
                  hjust = 1,
                  nudge_y = -0.005) +
  coord_flip() +
  scale_y_continuous(limits = c(-0.25, 0.01),
                     breaks = seq(-0.25, 0, by = 0.05),
                     expand = c(0, 0),
                     labels = scales::percent_format(accuracy = 1L)) +
  scale_fill_manual(values = colour_pal) +
  scale_colour_manual(values = colour_pal) +
  labs(y = "Percentage change in food production, relative to 2015") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(size = 8, 
                                    colour = "black"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, 
                                 colour = "black"),
        plot.margin = margin(10, 20, 5, 10),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size = 0.3)) 


ggsave(here("plots", "Paper Figures", "Figure_4.png"), 
       width = 172, 
       height = 86,
       units = "mm",
       dpi = 300)


# FIGRUE 5 - BIRDS ---------------------------------------------
load(here("outputs", "bird_results_tidy.RData"))
ambition_combos <- read_csv(here("data", "ambition_combinations_lusp.csv")) # Ambition combos for names

colour_pal <- c("darkorange2", "cyan4")


birds_sum_geomean %>%
  select(-pop_hi_rel, -n) %>% 
  rename(population_rel = pop_lo_rel) %>% 
  bind_rows(birds_sum_geomean %>%
              select(-pop_lo_rel, -n) %>% 
              filter(str_detect(scenario, "np_high")) %>% 
              mutate(scenario = str_sub(scenario, 1, 1)) %>% 
              rename(population_rel = pop_hi_rel)) %>% 
  filter(country == "UK" & group %in% c("All species", "BoCC Red/Amber", "Farmland specialists", "Woodland specialists")) %>%
  mutate(population_rel = population_rel-1,
         lusp_scenario = str_sub(scenario, 1, 1)) %>%
  # SCENARIO NAMES
  left_join(ambition_combos %>%
              select(scenario, name) %>%
              mutate(scenario = letters[row_number()],
                     name = case_when(name == "BAU" ~ "Baseline",
                                      name == "Widespread Engagement" ~ "WidespreadEngagement",
                                      name == "NbS-bioenergy" ~ "NbSNoBiomass",
                                      name == "NbS+organic" ~ "NbSOrganic",
                                      name == "NbS+HNVf" ~ "NBS Extra",
                                      name == "OnFarmSharing" ~ "OnFarmOrganic",
                                      TRUE ~ name)),
            by = c("lusp_scenario" = "scenario")) %>%
  mutate(energy_ambition = case_when(str_detect(scenario, "high") ~ str_to_title(str_sub(scenario, -4)),
                                     str_detect(scenario, "low") ~ str_to_title(str_sub(scenario, -3)),
                                     TRUE ~ "None"),
         name_energy = str_c(name, energy_ambition, sep = " - "),
         name = factor(name,
                       levels = c("Baseline", "BNZP", "WidespreadEngagement", "NbS", "NbSNoBiomass", "NbSOrganic", "NBS Extra", "OnFarmOrganic", "OnFarmBNZP"),
                       ordered = TRUE)) %>%
  filter(str_detect(scenario, "np") | !str_detect(scenario, "_"),
         name %in% c("Baseline", "NBS Extra")) %>% 
  arrange(name, energy_ambition) %>%
  mutate(id = row_number()) %>%
  mutate(position = case_when(population_rel > 0 ~ population_rel+0.01,
                              population_rel < 0 ~ population_rel-0.01,
                              TRUE ~ 0),
         label = case_when(population_rel > 0 ~ paste0("+", sprintf('%.2f', population_rel*100), "%"),
                           population_rel < 0 ~ paste0(sprintf('%.2f', population_rel*100), "%")),
         direction = case_when(population_rel > 0 ~ 1,
                               population_rel < 0 ~ -1,
                               TRUE ~ 0)) %>% 
  filter(!str_detect(scenario, "max")) %>%
  arrange(name, desc(energy_ambition)) %>%
  mutate(id = row_number()) %>%
  filter(group == "All species") %>% 
  ggplot(aes(x = reorder(name_energy, -id))) +
  geom_col(aes(y = population_rel,
               fill = lusp_scenario),
           alpha = 0.8,
           width = 0.8) +
  annotate("segment", x = 0.5, xend = 6.5, y = 0, yend = 0,
           linetype = "dashed",
           colour = "grey20",
           size = 0.45) +
  geom_shadowtext(aes(y = population_rel-direction*-0.02,
                      label = label,
                      colour = lusp_scenario),
                  bg.colour = "white",
                  fontface = "bold",
                  size = 3,
                  hjust = 0.5) +
  coord_flip() +
  scale_y_continuous(limits = c(-0.1, 0.22),
                     breaks = seq(-0.1, 0.2, by = 0.05),
                     expand = c(0, 0),
                     labels = scales::percent_format(accuracy = 1L)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = colour_pal) +
  scale_colour_manual(values = colour_pal) +
  labs(y = "Percentage change in breeding bird habitat availability, relative to 2015") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(size = 8, 
                                    colour = "black"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, 
                                 colour = "black"),
        plot.margin = margin(10, 20, 5, 10),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size = 0.3)) 


ggsave(here("plots", "Paper Figures", "Figure_5.png"), 
       width = 172, 
       height = 86,
       units = "mm",
       dpi = 300)

