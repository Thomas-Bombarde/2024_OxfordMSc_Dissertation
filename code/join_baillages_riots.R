library(tidyverse)
library("ggplot2")
library("sf")

# set directory 
setwd("C:/Users/TomBo/Documents/2024/2024 Diss code/output")

# get Data
baillages_sf <- read_sf("~/2024/2024 Diss code/input/dataverse_files/Bailliages/BAILLIAGES_1789_BRETTE/BAILLIAGES_1789_BRETTE.shp") |> 
  st_as_sf(baillages, coords = c("cl_position_x", "cl_position_y"), crs = 2154)
riots_sf <- readRDS(
  "C:/Users/TomBo/Documents/2024/2024 Diss code/output/france_sf_t.rds") |> 
  filter(country_name == "France")

# global vars
all <- unique(riots_sf$riot_type_hiscod)
institutional_riots <- c("Feudal conflict",
                        "Labour conflict",
                        "Conflict with local and/or national authorities",
                        "Religious conflict",
                        "Political conflict")

# function
boundary_of_riots <- function(riots_sf,
                              baillages_sf){
  "add index of the county to the riots data for which county the 
  riot happens in"
  within_df <- st_within(riots_sf, baillages_sf, sparse =F) |>
    as.data.frame()
  riots_sf$boundary_id <- apply(within_df, 1, function(x) which(x)[1])
  return(riots_sf)
}

count_baillage_riots <- function(riots_sf,
                                 baillages_sf,
                                 year1=1750,
                                 year2=1800, 
                                 riottype=all){
  "add a column to the baillage data that counts the number of riots
  per baillages"
  df <- riots_sf |> filter(year>year1 &
                             year<year2 &
                             riot_type_hiscod %in% riottype)
  table <- table(df$boundary_id) |>
    as.data.frame() |> mutate(Var1=as.numeric(Var1))
  n <- nrow(baillages_sf)
  baillages_sf$riots <- rep.int(0,n)
  for(i in table$Var1){
    baillages_sf$riots[i] <- table$Freq[i] 
  }
  return(baillages_sf)
}
plot_riots_in_baillages <- function(baillages_sf,
                                    year1,
                                    year2, 
                                    riottype=all){
  "Plot conflicts by baillages occuring between year1 and year2"
  plot <- ggplot() +geom_sf(data = baillages_sf,
                            aes(fill=riots,
                                colour = NULL)) +
    scale_fill_gradient(low = "white", high = "red") +
    labs(title= paste("Riots of", riottype, "type in France", year1, "-", year2), fill = "Count") +
    theme(panel.background = element_rect(fill = "white"))
  return(plot)
}
plot_count_riots_in_baillages <- function(year1,
                                          year2,
                                          riottype=all){
  "Aggregate function to count riots in baillages and plot"
  baillages_sf <- count_baillage_riots(riots_sf,
                                       baillages_sf,
                                       year1,
                                       year2, 
                                       riottype)
  plot_riots_in_baillages(baillages_sf,
                          year1,
                          year2, 
                          riottype)
}



# CLEAN
# check sf objects same coordinate system
st_crs(riots_sf)
st_crs(baillages_sf) 
# they aren't so let's fix that
baillages_sf <- st_transform(baillages_sf, st_crs(riots_sf))
# now add index of baillages to riots and count riots per baillages
riots_sf <- boundary_of_riots(riots_sf, baillages_sf)
baillages_sf <- count_baillage_riots(riots_sf, baillages_sf)
# save data
saveRDS(riots_sf, file ="france_sf_t.rds")
saveRDS(baillages_sf, file ="baillages_sf.rds")

# plot
plot_count_riots_in_baillages(year1=1780,year2=1790, riottype = "Feudal conflict")

# plot counties above and below median
med <- baillages_sf$riots |> median()
baillages_sf <- baillages_sf |> mutate("abovemedriots" = ifelse(riots>med, 1, 0))
