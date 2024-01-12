library(sf)
library(tidyverse)
library("ggplot2")
library("sf")

# set directory 
setwd("C:/Users/TomBo/Documents/2024/2024 Diss code/input")
# data
fr_shp <- st_read("France_shapefile/contour-des-departements.geojson")
fr_cities_shp <-  st_read("france-places-shape/places.shp")
bairoch.1988.tidy <- read.csv("~/2024/2024 Diss code/input/bairoch-1988-tidy.csv")
pop_ev <- bairoch.1988.tidy |> filter(year==1750|year==1800) |> 
  filter(country=="France") |> 
  pivot_wider(names_from=year, values_from=population) |> 
  na.omit()
pop_ev <- pop_ev |> mutate("percent_change" = (`1800`-`1750`)/`1750`*100)

bairoch_fr_1750 <- bairoch.1988.tidy |> filter(country=="France", year==1750)
setwd("C:/Users/TomBo/Documents/2024/2024 Diss code/output")

# check structure 
st_crs(fr_shp)
st_crs(fr_cities_shp)

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
df <- df |> filter(riot_type_hiscod_num %in% c(1,2,4,5,7,8)) |> 
  group_by(city, year.y) |> 
  mutate(riots_count = sum(riot))
df <- df |> mutate(riots_pc = riots_count/population)
# Plot riots
year1 = 1785
year2 = 1786
ggplot() + #sets map
  geom_sf(data = fr_shp)  +
  theme_bw() + geom_sf(data=df) +
  geom_sf(data=subset(df,year.y>=year1 & year.y<=year2), aes(size=riots_pc, col = riot_type_hiscod), col="red")  +
  coord_sf(xlim = c(-5,9),
           ylim = c(41.5,51.5)) +
  labs(title= paste("Select Riots in French cities,", year1,"-",year2, "(inclusive)"), 
       size = "riots per capita") + 
  theme(legend.position = c(0.35, 0.08), legend.direction = "horizontal",
        panel.grid = element_blank())

year1 = 1787
year2 = 1788
ggplot() + #sets map
  geom_sf(data = fr_shp)  +
  theme_bw() + geom_sf(data=df) +
  geom_sf(data=subset(df,year.y>=year1 & year.y<=year2), aes(size=riots_pc, col = riot_type_hiscod), col="red")  +
  coord_sf(xlim = c(-5,9),
           ylim = c(41.5,51.5)) +
  labs(title= paste("Select Riots in French cities,", year1,"-",year2, "(inclusive)", 
       size = "riots per capita")) + 
  theme(legend.position = c(0.35, 0.08), legend.direction = "horizontal",
        panel.grid = element_blank())

sum(df$riot)

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
