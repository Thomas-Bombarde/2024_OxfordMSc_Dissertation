library(haven)
library(sandwich)
library(stargazer)
library(AER)
library(ggplot2)

# https://cepr.org/voxeu/columns/long-run-spatial-inequality-france-evolution-and-determinants

# set directory and data
setwd("C:/Users/TomBo/Documents/2024/2024 Diss code/input")
fr_shp <- st_read("France_shapefile/contour-des-departements.geojson") |> 
  rename("department" = "nom")
juhasz_pretreat <- read_dta("Juhasz/department_pretreatment_panel.dta")
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
ggplot() + geom_sf(data=fr_shp) +
  geom_sf(data=riots, aes(fill=`department_riots_in_year`)) +
  scale_fill_gradient(low="yellow", high="red") +
  theme_bw() + labs(title= "Riots in 1789", fill = "Number of Riots") + theme_bw()







first_stage <- lm(thspindles ~ lnshortestLo + department + year, df)
stargaze(first_stage)


df_aug <- full_join(df,france_sf_t, by=c("department" = "admin_level_2"))
iv_reg <- ivreg()


# before any of this, i want to plan

# check naive reg: does + industries lead to more conflict 
# find insturment 

# caveats to my eventual regression - smuggling might have increased violence itself, 
# if smugglers bread an underground economy