# 2c UPDATE COUNTRY PROPORTIONS ######################
source("./Code/0_setup.R")

# UPDATE PROPORTION OF WIND/SOLAR PER COUNTRY -------------------------
# Dataframe of scenario abbreviations for loop
scenario_names <- tibble(
  name = c("a", "a_np", "b", "b_np", "c", "c_np", "d", "d_np", "e", "e_np", "f", "f_np", "g", "g_np", "h", "h_np", "i", "i_np")
)

# New renewable country props list  
country_props_renewables <- list()

# Loop over each country abbr and calculate country proportions 
for (i in 1:nrow(scenario_names)) {
  
  scenario_abbr <- scenario_names$name[i]
  
  # Load opportunity rasters per loop
  wind_files <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*windop_*_{scenario_abbr}.tif$")), recursive = TRUE)
  wind_list <- lapply(here("rasters", "scenarios", wind_files), raster)
  
  solar_files <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*solarop_*_{scenario_abbr}.tif$")), recursive = TRUE)
  solar_list <- lapply(here("rasters", "scenarios", solar_files), raster)
  
  # Calculate proportion of wind power per country 
  renewable_props <- 
    tibble(country = c("England", "Northern Ireland", "Scotland", "Wales"),
           wind_n = c(freqDT(wind_list[[1]] > 0)$freq[2],
                      freqDT(wind_list[[2]] > 0)$freq[2],
                      freqDT(wind_list[[3]] > 0)$freq[2],
                      freqDT(wind_list[[4]] > 0)$freq[2]),
           wind_prop = wind_n / sum(wind_n),
           solar_n = c(freqDT(solar_list[[1]] > 0)$freq[2],
                       freqDT(solar_list[[2]] > 0)$freq[2],
                       freqDT(solar_list[[3]] > 0)$freq[2],
                       freqDT(solar_list[[4]] > 0)$freq[2]),
           solar_prop = solar_n / sum(solar_n))
  
  # Update country props list 
  country_props_renewables[[glue("renewable_props_{scenario_abbr}")]] <- renewable_props

  # Progress update 
  print(paste("Finished country props loop", i, "of 18 - ", Sys.time()))
  
}

# Save updated country props
save(country_props_renewables, file = here("data", "country_props_renewables.RData"))

