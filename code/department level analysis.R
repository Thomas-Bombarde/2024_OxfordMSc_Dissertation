library(haven)
library(tidyverse)
library(sandwich)
library(sf)
library(stargazer)
library(AER)
library(ggplot2)
# set directory and data
setwd("C:/Users/TomBo/Documents/2024/2024 Diss code/input")
fr_shp <- st_read("France_shapefile/contour-des-departements.geojson") |> 
  rename("department" = "nom")
juhasz_pretreat <- read_dta("Juhasz/department_pretreatment_panel.dta")
juhasz_pretreat <- juhasz_pretreat |> filter(year==1794)
setwd("C:/Users/TomBo/Documents/2024/2024 Diss code/output")
riots_data <- st_read("riots_data.geojson")
# for regressions
stargaze <- function(fit1, se.type = "HC1"){
se_fit1 <- sqrt(diag(vcovHC.default(fit1, type = se.type)))
stargazer(fit1, se = list(se_fit1, NULL), type = "text")
}

# riots data with department geometry
riots_data_departments <- st_join(fr_shp, riots_data, join = st_contains)
riots <- riots_data_departments |> filter(year==1789)
#plot
ggplot() + geom_sf(data=fr_shp) +
  geom_sf(data=riots, aes(fill=`department_riots_in_year`)) +
  scale_fill_gradient(low="grey", high="red") +
  theme_bw() + labs(title= "Riots in 1789", fill = "Number of Riots") + theme_bw()
# see if effective distance predicts riots in 1789
df <- riots_data_departments |> filter(year>1776 & year<1792) |> 
  group_by(year, department) |> summarize(riot_count=sum(riot))
df <- df |> as.data.frame() |> select(1:3)
df <- right_join(df, juhasz_pretreat, by = c("department"))
df <- df |> na.omit()
df <- df |> mutate("treatment" = case_when(year.x>1787 ~ 1, TRUE ~0))
saveRDS(df, "regression_data.rds")
model <- lm(riot_count ~ factor(year.x) + department + lnshortestLo:treatment, df) 
stargaze(model)

df <- readRDS("regression_data.rds") |> rename("year" = "year.x") 
df <- df |> mutate(riot_count = case_when(is.na(driot_count) ~ 0, TRUE ~ df$riot_count))
ggplot(filter(df,year>1780&year<1790)) + 
  geom_point(aes(x=year, y=riot_count, col=department)) + 
  geom_line(aes(x=year, y=riot_count, col=department), alpha=0.3) + 
  geom_vline(xintercept = 1787, linetype = "dashed", color = "red", lwd=1) +
  scale_x_continuous(breaks = seq(1770, 1797, 1)) +
  geom_text(aes(x=1787, y=30, label="Eden Treaty"))
  
  
?geom_vline
# we find a negative relationship between effective distance in 1794
# and riots after treatment, supporting Henderson rather than Weir's claim.
# we can also back up Henderson's claim by looking at actual imports. 