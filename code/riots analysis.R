library(sf)
library(tidyverse)
library("ggplot2")
library("sf")
library(lubridate)
library(xtable)


# set directory 
setwd("C:/Users/TomBo/Documents/2024/2024 Diss code/input")
# map theme
theme_set(theme_bw())

# DATA
# conflicts
db_hiscod <- read.csv("hiscod-project-main/data_csv/db_hiscod_csv_v1_en.csv", encoding="UTF-8", row.names=NULL, sep=";")

# france shape
fr_shp <- st_read("France_shapefile/contour-des-departements.geojson")
fr_cities_shp <-  st_read("france-places-shape/places.shp")
db_hiscod <- read.csv("hiscod-project-main/data_csv/db_hiscod_csv_v1_en.csv", encoding="UTF-8", row.names=NULL, sep=";")
riots_data <- st_read("riots_data.geojson")

# take only French conflicts

france <- db_hiscod |> filter(country_name=="France") |>
  mutate(riot_date = ymd(riot_date))
france <- france |> 
  filter(riot_date>ymd("1780-01-01")&riot_date<ymd("1789-05-01")) |> 
  filter(riot_type_hiscod_num %in% c(1,2,4,5,7,8)) |> 
  group_by(year, month) |>
  mutate("count" = n())
france |> 
  filter(year>1785&year<1789) |> 
  filter(riot_type_hiscod_num %in% c(1,2,4,5,7,8)) |> 
  group_by(year) |>
  count()

ggplot(subset(france)) + 
  geom_area(aes(x=riot_date, y=count, fill=riot_type_hiscod)) +
  scale_fill_viridis_d(option = "veridis")+
  labs(x="Date", y="Number of Riots", fill = "Riot Type") +
  theme(text = element_text(size=14), legend.position = c(.3, 0.8), legend.direction = "vertical",
        panel.grid = element_blank())

table <- france |> 
  filter(year>1785&year<1789) |> 
  filter(riot_type_hiscod_num %in% c(1,2,4,5,7,8)) |> 
  group_by(year, month) |> 
  count()
table <- table |> pivot_wider(names_from=year, values_from =n)
names(table) <- c("Month", (1786:1788))
latex_table <- xtable(table)

ggplot() + geom_sf(data=fr_shp) +
  geom_sf(data=riots_data, aes(fill=`department_riots_in_year`)) +
  scale_fill_gradient(low="grey", high="red") +
  theme_bw() + labs(title= "Riots in 1789", fill = "Number of Riots") + theme_bw()

