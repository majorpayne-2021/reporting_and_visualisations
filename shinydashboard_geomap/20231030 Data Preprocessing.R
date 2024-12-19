# -------------------------------------------------------------------------
# Assignment 3 (40%) - Data Preprocessing
#   -----------------------------------------------------------------------

# Get the directory of the current R script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the directory of the R script
setwd(script_dir)
# set working directory to data folder
setwd("data/")

# Import libraries --------------------------------------------------------

#install.packages('rsconnect')

# Spacial mapping and visualisations
library(leaflet)
library(leaflet.extras)
library(sf)
library(plotly)
library(ggplot2)
library(shiny)
library(RColorBrewer)


# data wrangling libraries
library(tidyverse)
library(dplyr)
library(stringr)

# import file libaries
library(readr)
library(readxl)

# Read in spacial data ------------------------------------------------------------

# Read in greater melbourne lookup
file_path <- "greater_melb_lkup.xlsx"
sheet_name <- "lookup"
df_greatermelb <- read_excel(file_path, sheet = sheet_name)

# Read in spatial data
victoria_sf <- st_read("GDA94/vic_lga.shp")
victoria_sf$ABB_NAME[victoria_sf$ABB_NAME == "Merri-Bek"] = "Merri-bek"

# remove duplicates
# Remove duplicates based on selected columns (ID and Value in this case)
victoria_sf <- victoria_sf %>% group_by(LGA_NAME,ABB_NAME) %>% mutate(count_dup = row_number()) %>% ungroup()
victoria_sf <- victoria_sf %>% filter(!(ABB_NAME == "Murrindindi" & count_dup == 2))

#st_write(victoria_sf, "vic_shape_clean.shp", append = FALSE)

# Simplify Shape File -----------------------------------------------------

# Read the shapefile
shapefile <- st_read("GDA94/vic_lga.shp")

# Simplify the shapes
simplified_shapes <- st_simplify(shapefile, dTolerance = 0.1)  # Adjust dTolerance as needed

# remove duplicates
# Remove duplicates based on selected columns (ID and Value in this case)
simplified_shapes <- simplified_shapes %>% group_by(LGA_NAME,ABB_NAME) %>% mutate(count_dup = row_number()) %>% ungroup()
simplified_shapes <- simplified_shapes %>% filter(!(ABB_NAME == "Murrindindi" & count_dup == 2))

simplified_shapes$ABB_NAME[simplified_shapes$ABB_NAME == "Merri-Bek"] = "Merri-bek"

# Write the simplified shapes to a new shapefile
st_write(simplified_shapes, "vic_shape_simple.shp", append = FALSE)

#victoria_sf <- st_read("vic_shape_simple.shp")

# Low Crime for Year Ending Dec 2022 --------------------------------------
# Note: latest victim report by harm level data is the 

# Read in original graph data
file_path <- "Data_Tables_LGA_Harm_Caused_Visualisation_Year_Ending_December_2022.xlsx"
sheet_name <- "Table 01"
df <- read_excel(file_path, sheet = sheet_name)

# filter on Year ending December 2022
df <- df %>% filter(Year == 2022, `Year ending` == 'December')

# Convert the data into high, medium and low proportions per Victorian LGA
df_harm <- df %>% select(Year, `Year ending`, `Local Government Area`, `Harm Caused Flag`, `Victim Reports`)

# Rename columns
df_harm <- df_harm %>% 
  rename("year" = "Year"
         , "year_ending" = `Year ending`
         , "LGA" = `Local Government Area`
         , "harm_caused" = `Harm Caused Flag`
         , "victim_reports" = `Victim Reports`
         )

# Add proportion of crime per LGA
df_harm <- df_harm %>% 
  group_by(LGA) %>% 
  mutate(prop_LGA = round(victim_reports/sum(victim_reports),2)) %>% 
  ungroup()

# join to greater melbourne lookup
df_harm_gm <- left_join(df_harm, df_greatermelb, by = "LGA")

# Filter on Low harm only for each LGA
df_harm_low <- df_harm_gm %>% 
  filter(harm_caused == 'Low Harm'
         , greater_melb_flag == "Y") %>% 
  select(LGA
         , harm_caused 
         , prop_LGA)

# rank suburbs for highest to lowest proportion of low crime
df_harm_low <- df_harm_low %>% arrange(desc(prop_LGA)) %>% mutate(rank_safest = row_number())
df_harm_low <- df_harm_low %>% filter(rank_safest <=10)

# Top 10 suburbs where there is the lowest percentage of crime out of all crimes committed.
df_harm_safest10 <- df_harm %>% filter(LGA %in% df_harm_low$LGA)

# GRAPH
# Reorder the levels of 'harm_caused' for facetting
df_harm_safest10$harm_caused <- factor(df_harm_safest10$harm_caused,
                                       levels = c('High Harm','Medium Harm','Low Harm'))

# Reorder the levels of 'LGA' by the sum of 'victim_reports'
df_harm_safest10$LGA <- factor(df_harm_safest10$LGA,
                               levels = names(sort(tapply(df_harm_safest10$victim_reports, df_harm_safest10$LGA, sum))))

# Export safest 10 LGAs 
write_csv(df_harm_safest10, "df_harm_safest10.csv")

#   -----------------------------------------------------------------------
# Top 5 Lowest Crime, Top 5 Highest Crime --------------------------------
#   -----------------------------------------------------------------------

# Read in original graph data
file_path <- "Data_Tables_LGA_Recorded_Offences_Year_Ending_June_2023.xlsx"
sheet_offence_lga_total <- "Table 01"
df_offence_lga <- read_excel(file_path, sheet = sheet_offence_lga_total)

df_offence_lga <- df_offence_lga %>% 
  rename("year" = "Year"
         , "year_ending" = `Year ending`
         , "police_region" = `Police Region`
         , "LGA" = `Local Government Area`
         , "offence_count" = `Offence Count`
         , "rate_100k" = `Rate per 100,000 population`
  )
  
# filter on greater melbourne only
df_offence_top5_lga <- df_offence_lga %>% 
  select(year, year_ending, LGA, offence_count, rate_100k) %>% 
  filter(LGA %in% df_greatermelb$LGA)

# Rank most dangerous (highest rate per 100k pop) to safest
df_offence_top5_lga <- df_offence_top5_lga %>% 
  arrange(year, year_ending, desc(rate_100k)) %>% 
  group_by(year, year_ending) %>% 
  mutate(unsafe_safe = row_number()) %>% 
  ungroup()

# Rank safest (lowest rate per 100k pop) to worst
df_offence_top5_lga <- df_offence_top5_lga %>% 
  arrange(year, year_ending, rate_100k) %>% 
  group_by(year, year_ending) %>% 
  mutate(safe_unsafe = row_number()) %>% 
  ungroup()

# Filter on safest 5 and unsafest 5 LGAs for each year
#df_offence_top5_lga <- df_offence_top5_lga %>% 
#  filter(unsafe_safe <= 5 | safe_unsafe <= 5)

# Create labels
df_offence_top5_lga$safe_label <- case_when(df_offence_top5_lga$safe_unsafe <= 5 ~ "Top5_Safe"
                                            , df_offence_top5_lga$unsafe_safe <= 5 ~ "Top5_Unsafe"
                                            , TRUE ~ NA)

# remove extra columns
df_offence_top5_lga$unsafe_safe <- NULL
df_offence_top5_lga$safe_unsafe <- NULL

# export data
write_csv(df_offence_top5_lga, "df_offence_top5_lga.csv")

#   -----------------------------------------------------------------------
# All LGA by crime volume and crime rate ----------------------------------
#   -----------------------------------------------------------------------

df_offence_recentyear <- df_offence_lga %>% 
  filter(year == 2023
         , !is.na(rate_100k)
         , LGA %in% df_greatermelb$LGA) %>% 
  group_by(police_region = ifelse(LGA == "Melbourne", "0 Melbourne CBD", police_region)) %>% 
  summarise(sum_offences = sum(offence_count)) %>% 
  ungroup() %>% 
  mutate(percentage = sum_offences/sum(sum_offences))

# Sort the data in descending order of offence count
df_offence_recentyear <- df_offence_recentyear %>%
  arrange(desc(sum_offences))

# Assuming police_region is a factor
df_offence_recentyear$police_region <- factor(df_offence_recentyear$police_region)

# export data
write_csv(df_offence_recentyear, "df_offence_recentyear.csv")

# -------------------------------------------------------------------------
# Family crime proportion by LGA ------------------------------------------
#   -----------------------------------------------------------------------

# Read in family indicents
file_path <- "Data_Tables_LGA_Family_Incidents_Year_Ending_June_2023.xlsx"
sheet_family <- "Table 01"
df_family_lga <- read_excel(file_path, sheet = sheet_family)

# read in total incidents
file_path <- "Data_Tables_LGA_Criminal_Incidents_Year_Ending_June_2023.xlsx"
sheet_incidents <- "Table 01"
df_incidents_lga <- read_excel(file_path, sheet = sheet_incidents)

# cleaning
df_family_lga <- df_family_lga %>% 
  rename("year" = "Year"
         , "year_ending" = `Year ending`
         , "police_region" = `Police Region`
         , "LGA" = `Local Government Area`
         , "family_incidents" = `Family Incidents`
         , "rate_100k" = `Rate per 100,000 population`) %>% 
  filter(year == 2023
         , LGA %in% df_greatermelb$LGA)

df_incidents_lga <- df_incidents_lga %>% 
  rename("year" = "Year"
         , "year_ending" = `Year ending`
         , "police_region" = `Police Region`
         , "LGA" = `Local Government Area`
         , "total_incidents" = `Incidents Recorded`
         , "rate_100k" = `Rate per 100,000 population`) %>% 
  filter(year == 2023
         , LGA %in% df_greatermelb$LGA)

# join family and total incidents together
df_family_merged <- left_join(df_family_lga, df_incidents_lga[c("LGA", "total_incidents")], by = "LGA")

# sum to police region and Melbourne CBD
df_family_merged$police_region <- case_when(df_family_merged$LGA == "Melbourne" ~ "0 Melbourne CBD", TRUE ~ df_family_merged$police_region)
df_family_merged$prop_family <- df_family_merged$family_incidents/df_family_merged$total_incidents

# Order the dataset by 'prop_family' in descending order
df_family_merged <- df_family_merged %>%
  arrange(desc(prop_family)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 5)

# export data
write_csv(df_family_merged, "df_family_incidents.csv")


# End of Data Preprocessing -----------------------------------------------


