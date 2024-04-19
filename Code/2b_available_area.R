# 2b CALCULATE AVAILABLE AREA FOR WIND AND SOLAR FARMS ######################
source("./Code/0_setup.R")

# LAND COVER TYPE AREA CALCULATION -------------------------
## CREATE/SET FUNCTION ----
landcover_area_fun <- function(countries){
  
  # Get country names input in dataframe
  country_df <- tibble(
    name = countries
  )
  
  # Create empty data frame to fill in loops
  area_df <- tibble(
    lcm_layer = character(),
    opportunity_layer = character(),
    country = character(),
    scenario = character(),
    opportunity_type = character(),
    national_parks = character(),
    lcm_class = double(),
    alc = double(),
    Freq = integer()
  )
  
  # Loop to select country if multiple used in function input
  for (i in 1:nrow(country_df)) {
    
    country_abbr <- country_df$name[i]
    
    # Load wind opportunity rasters
    wind_files_all <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*wind*{country_abbr}*.tif$")), recursive = TRUE)
    
    wind_np_files <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*wind*{country_abbr}*np.tif$")), recursive = TRUE)
    wind_np_list <- lapply(here("rasters", "scenarios", wind_np_files), raster)
    
    wind_files <- setdiff(wind_files_all, wind_np_files)
    wind_list <- lapply(here("rasters", "scenarios", wind_files), raster)
    
    # Load solar opportunity rasters
    solar_files_all <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*solar*{country_abbr}*.tif$")), recursive = TRUE)
    
    solar_np_files <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*solar*{country_abbr}*np.tif$")), recursive = TRUE)
    solar_np_list <- lapply(here("rasters", "scenarios", solar_np_files), raster)
    
    solar_files <- setdiff(solar_files_all, solar_np_files)
    solar_list <- lapply(here("rasters", "scenarios", solar_files), raster)
    
    # Load LCM rasters
    lcm_files <-list.files(here("rasters", "scenarios"), pattern = glob2rx(glue("*lcm*{country_abbr}*.tif$")), recursive = TRUE)
    lcm_list <- lapply(here("rasters", "scenarios", lcm_files), stack)
    
    # Load ALC rasters 
    alc_files <-list.files(here("rasters", "other"), pattern = glob2rx(glue("r_alc_{country_abbr}.tif$")))
    r_alc <- raster(here("rasters", "other", alc_files))
    
    
    # Loop to select scenario a:i
    for(j in 1:length(lcm_list)) {
      
      r_lcm <- stack(lcm_list[[j]][[7]]) # select 2050
      r_renew_stack <- stack(wind_list[[j]], wind_np_list[[j]], solar_list[[j]], solar_np_list[[j]])
      
      # Loop to select opportunity type
      for(l in 1:nlayers(r_renew_stack)){
        
        # Cross tabulation per loop - results as tibble 
        loop_df <- tibble(crosstabDT(stack(r_lcm, r_renew_stack[[l]], r_alc), digits = 2, long = TRUE, useNA = FALSE))
        
        # Convert wrangle and select wanted variables 
        loop_df <- loop_df %>% 
          mutate("lcm_layer" = colnames(loop_df[1]),
                 "opportunity_layer" = colnames(loop_df[2]))%>%
          rename("lcm_class" = !!names(.[1]),
                 "alc" = !!names(.[3])) %>% 
          mutate(country = case_when(grepl("eng", lcm_layer) ~ "England",
                                     grepl("nir", lcm_layer) ~ "Northern Ireland",
                                     grepl("sct", lcm_layer) ~ "Scotland",
                                     grepl("wal", lcm_layer) ~ "Wales"),
                 scenario = str_sub(lcm_layer, 7,7),
                 number = str_sub(lcm_layer, -1),
                 opportunity_type = case_when(grepl("windop", opportunity_layer) ~ "wind",
                                              grepl("solarop", opportunity_layer) ~ "solar"),
                 national_parks = case_when(grepl("np", opportunity_layer) ~ "masked",
                                            TRUE ~ "not_masked")) %>% 
          select(c("lcm_layer", "opportunity_layer", "country", "scenario", "opportunity_type" ,"national_parks" , "lcm_class", "alc", "Freq"))
        
        # Update main area dataframe 
        area_df <- rbind(area_df, loop_df)
        
      }
      
      # Progress update
      print(paste("finished loop", j, "for", country_abbr))
      
    }
    
    # Progress update
    print(paste("FINISHED", country_abbr, "---"))
    
  }
  
  # Convert freq to hectares
  area_df <- area_df %>% 
    mutate(ha = Freq*0.0625) %>% 
    select(-Freq)
  
  print("--- FINISHED ALL ---")
  
  # Save
  write_csv(area_df, here("outputs", "df_renewable_landcover_area.csv"))
  
  return(df_renewable_landcover_area = area_df)
  
}
  

## RUN FUNCTION ----
df_renewable_landcover_area <- landcover_area_fun(c("eng", "nir", "sct", "wal"))

