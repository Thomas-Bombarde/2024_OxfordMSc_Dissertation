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
# take only French conflicts
france <- db_hiscod |> filter(country_name=="France")
# france shape
fr_shp <- st_read("France_shapefile/contour-des-departements.geojson")
fr_cities_shp <-  st_read("france-places-shape/places.shp")
# CLEAN
# choose relevant vars
france_sf_t <- france |>
  select(id,id_riot_hiscod,
         id_riot_original_database,
         title,
         year,
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
france_sf_t <- france_sf_t |> mutate(riot_type_hiscod = case_when(riot_type_hiscod == "" ~"Unspecified",
                                               TRUE ~ as.character(riot_type_hiscod)))

departments <- france_sf_t
# count riots per departement
locate_department <- function(fr_shp,
                                 data){
  "add index of the department to the data"
  within_df <- st_within(data, fr_shp, sparse =F) |>
    as.data.frame()
  data$department_id <- apply(within_df, 1, function(x) which(x)[1])
  return(data)
}
departments <- locate_department(fr_shp,france_sf_t)
count_departements_riots <- function(riots_sf,
                                     departments,
                                 year1=1750,
                                 year2=1800, 
                                 riottype="Feudal conflict"){
  "add a column to the departments data that counts the number of riots
  per baillages"
  df <- departments |> filter(year>year1 &
                             year<year2 &
                             riot_type_hiscod %in% riottype)
  table <- table(df$department_id) |>
    as.data.frame() |> mutate(Var1=as.numeric(Var1))
  n <- nrow(fr_shp)
  fr_shp <- cbind(fr_shp,"riots"=rep.int(0,n))
  for(i in table$Var1){
    fr_shp$riots[i] <- table$Freq[i] 
  }
  return(fr_shp)
}


departments <- count_departements_riots(riots_sf,
                         departments,
                         year1=1780,
                         year2=1791, 
                         riottype="Feudal conflict")
departments <- departments |> rename("Feudal conflict" = riots)

# check structure 
st_crs(fr_shp)
st_crs(fr_cities_shp)
bairoch.1988.tidy <- read.csv("~/2024/2024 Diss code/input/bairoch-1988-tidy.csv")
bairoch_fr_1750 <- bairoch.1988.tidy |> filter(country=="France", year==1750)
# 1. Join Cities to Bairoch data
fr_cities_shp <- fr_cities_shp |> select(name,geometry) |> rename("city"="name")
bairoch_fr_1750 <- merge(fr_cities_shp,bairoch_fr_1750,by = "city") |> 
  st_as_sf(coords=geometry)
missing_bairoch_fr_1750 <- bairoch_fr_1750 |> is.na() |> which()

stwithin.5 <- function(x,y){
  return(st_is_within_distance(x,y,dist=10000))
  }

df_full <- st_join(x=bairoch_fr_1750,y=france_sf_t, stwithin.5, left = T) 
rm(bairoch.1988.tidy, bairoch_fr_1750, fr_cities_shp, france)
df <- df_full |> mutate(riot=case_when(is.na(city_name) ~ 0, TRUE ~ 1))
df <- df |> 
  mutate(pop_share = population/sum(population))
df <- df |> 
  group_by(city, year.y) |> 
  mutate(riots_count = sum(riot))
df <- df |> mutate(riots_pc = riots_count/population)

# Note: WGS uses meters (type st_crs() to heck) 0.1 difference in latitude coresspendonds to about 10km https://www.omnicalculator.com/other/latitude-longitude-distance
# 2. Plot
# 3. Search Function


# Plot riots
year1 = 1770
year2 = 1780
year1 = 1787
year2 = 1788
ggplot() + #sets map
  geom_sf(data = fr_shp)  +
  theme_bw() + geom_sf(data=df) +
  geom_sf(data=subset(df,year.y>=year1 & year.y<=year2), aes(size=riots_pc, col = riot_type_hiscod), col="red")  +
  coord_sf(xlim = c(-5,9),
           ylim = c(41.5,51.5)) +
labs(title= paste("Riots in French cities,", year1,"-",year2), size = "riots per capita")
  

ggplot() + #sets map
  geom_sf(data = fr_shp)  +
  theme_bw() + geom_sf(data=df) +
  geom_sf(data=subset(df,year.y>=1787 & year.y<=1792), aes(size=riots_pc), col="red")  +
  coord_sf(xlim = c(-5,9),
           ylim = c(41.5,51.5))+
  labs(title= "Riots in French cities, 1787-1792", size = "riots per capita")

ggplot() + #sets map
  geom_sf(data = fr_shp)  +
  theme_bw() +  
  geom_sf(data = subset(france_sf_t, year %in% (1788:1789)),
                aes(col = riot_type_hiscod,size = nb_participants)) +
  geom_sf(data=bairoch_fr_1750, aes(size=population)) +
  labs(title= "Riots in France, 1789", color = "Type of conflict", size = "Population")

st_crs(france_sf_t)
# Plot city pop
ggplot() + #sets map
  geom_sf(data = fr_shp) + geom_sf(data = fr_cities_shp, aes(size=population))

left_join(fr_cities_shp)
# Plot by Department
ggplot() + geom_sf(data = fr_shp) +  geom_sf(fr_cities_shp)
plot <- ggplot() + geom_sf(data = departments,
                          aes(fill=`Feudal conflict`,
                              colour = NULL)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title= paste("Feudal conflict in France"), fill = "Count") +
  theme(panel.background = element_rect(fill = "white"))

ggplot(subset(france_sf_t, year<1900 & year>1600)) + geom_density(aes(x=year, col =riot_type_hiscod))
ggplot(subset(france_sf_t, year<1900 & year>1600)) + geom_density(aes(x=year, col =city_name))

