library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(readxl)
mpd2020 <- read_excel("C:\Users\TomBo\Documents\2024\mpd2020.xlsx",
sheet = "GDP pc")

mpd2020 |> names()
eu <- mpd2020 |> select(1, "France", "United Kingdom", "Germany", "Netherlands") |> 
  mutate(Year = as.double(`GDP pc 2011 prices`))

eu <- eu |> 
  mutate(across(c("France", "Germany", "United Kingdom", "Netherlands"),
                as.double)) |>
  na.omit() |> 
  select(-1)

melteu <- melt(eu,id="Year")
ggplot(subset(melteu, Year < 1900),aes(x=Year,y=value,colour=variable)) + geom_line()


