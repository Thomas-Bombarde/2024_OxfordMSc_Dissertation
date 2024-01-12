library(sf)
library(tidyverse)
library("ggplot2")
library("sf")

# set directory 
setwd("C:/Users/TomBo/Documents/2024/2024 Diss code/input")
# map theme
theme_set(theme_bw())

# DATA
# conflicts
db_hiscod <- read.csv("hiscod-project-main/data_csv/db_hiscod_csv_v1_en.csv", encoding="UTF-8", row.names=NULL, sep=";")
france <- db_hiscod |> filter(country_name=="France") |>
  mutate(riot_date = ymd(riot_date))
fr_shp <- st_read("France_shapefile/contour-des-departements.geojson")
fr_cities_shp <-  st_read("france-places-shape/places.shp")
# CLEAN
# choose relevant vars
france_sf_t <- france |>
  select(id,id_riot_hiscod,
         id_riot_original_database,
         title,
         year,
         month,
         day,
         riot_date,
         women_participation,
         city_latitude,
         city_longitude,
         country_name,
         city_name,
         nb_participants,
         geo_precision,
         admin_level_1, # this is the modern region it took place in 
         admin_level_2, # this is the equivalent of the modern department the event took place
         historical_admin_level_1, 
         # code to indicate the precision of coordinates, 
         #1: exact   2: nearest location   3: imprecise
         riot_type_hiscod_num, 
         riot_type_hiscod
         # riot_type_original_database_1,
         # riot_type_original_database_2,
         # riot_type_original_database_3 # three different classifications with 
         # same description, unsure why. 
         ) |>
  filter(is.na(city_longitude) == FALSE,
         is.na(city_latitude) == FALSE, 
         is.finite(city_longitude) == TRUE,
         is.finite(city_latitude) == TRUE) |> 
  st_as_sf(coords = c("city_longitude","city_latitude"),
           crs=4326)

# convert unknown particpants to -1, and take lowest bounds to all others approximative estimates
france_sf_t <- france_sf_t |> 
  mutate(nb_participants = case_when(
    nb_participants == "Unknown" ~ -1,
    nb_participants == "around 100" ~ 100, 
    nb_participants == "around 12" ~ 12,
    nb_participants == "around 150" ~ 150,
    nb_participants == "around 20" ~ 20,
    nb_participants == "around 200" ~ 200,
    nb_participants == "around 30" ~ 30,
    nb_participants == "around 300" ~ 300,
    nb_participants == "around 40" ~ 40,
    nb_participants == "around 50" ~ 50,
    nb_participants == "around 60" ~ 60,
    nb_participants == "more than 100" ~ 100,
    nb_participants == "more than 1000" ~ 1000,
    nb_participants == "more than 120" ~ 120,
    nb_participants == "more than 150" ~ 150,
    nb_participants == "more than 1500" ~ 1500,
    nb_participants == "more than 20" ~ 20,
    nb_participants == "more than 200" ~ 200,
    nb_participants == "more than 2000" ~ 2000,
    nb_participants == "more than 22" ~ 22,
    nb_participants == "more than 250" ~ 250,
    nb_participants == "more than 3" ~ 3,
    nb_participants == "more than 30" ~ 30,
    nb_participants == "more than 300" ~ 300,
    nb_participants == "more than 4" ~ 4,
    nb_participants == "more than 40" ~ 40,
    nb_participants == "more than 400" ~ 400,
    nb_participants == "more than 5" ~ 5,
    nb_participants == "more than 50" ~ 50,
    nb_participants == "more than 500" ~ 500,
    nb_participants == "more than 60" ~ 60,
    nb_participants == "more than 600" ~ 600,
    nb_participants == "more than 800" ~ 800,
    nb_participants == "several dozen" ~ 24, # You can set this value accordingly
    nb_participants == "several hundred" ~ 200, # You can set this value accordingly
    nb_participants == "several thousand" ~ 2000, # You can set this value accordingly
    nb_participants == "10-12" ~ 10,
    nb_participants == "10-15" ~ 10,
    nb_participants == "100-120" ~ 100,
    nb_participants == "100-150" ~ 100,
    nb_participants == "100-250" ~ 100,
    nb_participants == "1000-1200" ~ 1000,
    nb_participants == "101-200" ~ 101,
    nb_participants == "11-15" ~ 11,
    nb_participants == "12-15" ~ 12,
    nb_participants == "1200-1500" ~ 1200,
    nb_participants == "15-20" ~ 15,
    nb_participants == "150-200" ~ 150,
    nb_participants == "150-300" ~ 150,
    nb_participants == "1500-1600" ~ 1500,
    nb_participants == "15000-16000" ~ 15000,
    nb_participants == "16-18" ~ 16,
    nb_participants == "16-20" ~ 16,
    nb_participants == "1600-3000" ~ 1600,
    nb_participants == "20-25" ~ 20,
    nb_participants == "20-30" ~ 20,
    nb_participants == "20-40" ~ 20,
    nb_participants == "200-300" ~ 200,
    nb_participants == "200-400" ~ 200,
    nb_participants == "2000-3000" ~ 2000,
    nb_participants == "2000-4000" ~ 2000,
    nb_participants == "21-50" ~ 21,
    nb_participants == "25-30" ~ 25,
    nb_participants == "25-35" ~ 25,
    nb_participants == "250-300" ~ 250,
    nb_participants == "3-5" ~ 3,
    nb_participants == "30-40" ~ 30,
    nb_participants == "300-400" ~ 300,
    nb_participants == "35-40" ~ 35,
    nb_participants == "4-5" ~ 4,
    nb_participants == "40-50" ~ 40,
    nb_participants == "400-500" ~ 400,
    nb_participants == "400-600" ~ 400,
    nb_participants == "4000-4500" ~ 4000,
    nb_participants == "4000-5000" ~ 4000,
    nb_participants == "401-800" ~ 401,
    nb_participants == "45-50" ~ 45,
    nb_participants == "5-6" ~ 5,
    nb_participants == "50-100" ~ 50,
    nb_participants == "50-60" ~ 50,
    nb_participants == "50-80" ~ 50,
    nb_participants == "500-600" ~ 500,
    nb_participants == "5000-10000" ~ 5000,
    nb_participants == "5000-6000" ~ 5000,
    nb_participants == "51-100" ~ 51,
    nb_participants == "6-10" ~ 6,
    nb_participants == "6-7" ~ 6,
    nb_participants == "6-8" ~ 6,
    nb_participants == "60-70" ~ 60,
    nb_participants == "60-80" ~ 60,
    nb_participants == "600-700" ~ 600,
    nb_participants == "7-8" ~ 7,
    nb_participants == "70-80" ~ 70,
    nb_participants == "700-800" ~ 700,
    nb_participants == "7000-8000" ~ 7000,
    nb_participants == "8-10" ~ 8,
    nb_participants == "80-100" ~ 80,
    nb_participants == "80-90" ~ 80,
    nb_participants == "800-1000" ~ 800,
    nb_participants == "800-900" ~ 800,
    TRUE ~ as.double(nb_participants)
  ))
riots_data <- france_sf_t |> mutate(riot_type_hiscod = case_when(riot_type_hiscod == "" ~"Unspecified",
                                               TRUE ~ as.character(riot_type_hiscod)))
riots_data$riot <- rep(1, nrow(riots_data))
# count riots per departement
locate_department <- function(fr_shp,
                                 data){
  "add index of the department to the data"
  within_df <- st_within(data, fr_shp, sparse =F) |>
    as.data.frame()
  data$department_id <- apply(within_df, 1, function(x) which(x)[1])
  return(data)
}
riots_data <- locate_department(fr_shp,riots_data)
riots_data <- riots_data |> group_by(department_id, year) |> 
  mutate(department_riots_in_year = sum(riot))
riots_data <- riots_data |> group_by(department_id, year, riot_type_hiscod) |> 
  mutate(department_riots_in_year_type = sum(riot))
st_write(riots_data, "riots_data.geojson")
6