library(sf)
library(tidyverse)
library("forcats")
library("ggplot2")
library("sf")

# set directory 
setwd("C:/Users/TomBo/Documents/2024/2024 Diss code/input")
british_trade_1780 <- read_dta("~/2024/2024 Diss code/input/british_trade_1780.dta")
df_century <- read_dta("2024/2024 Diss code/input/Anglo-French trade, 1700-1800.dta")
df <- british_trade_1780 |> filter(destination="France")
unique(british_trade_1780$com)

# What does trade look like in 1780 for 
brtofr <- british_trade_1780 |> filter(destination %in% c("France", "Guernsey", "Jersey"))
brtofr <- brtofr |> group_by(year,origin,com,com2,com3) |> reframe(sum_value = sum(value))
brtofr$destination <- rep("France", nrow(brtofr))
brtofr <- brtofr |> mutate(com=as.factor(com))
ggplot(brtofr) + 
  geom_col(aes(x=fct_reorder(com, desc(sum_value)), y=sum_value)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Exports from Britain to Channel Islands & France in 1780",
       x = "Categories", y = "Values") +
  scale_y_continuous(breaks = seq(0, 8500, 500))

# 2. Is Channel Island and French trade counter-cyclical?
# 2.1. Looking only at Guernsey
df_century <- df_century |> 
  filter(destination%in%c("France", "Guernsey"))
df_century <- df_century |> 
  group_by(year,origin,destination) |> 
  reframe(sum_value = sum(value))
ggplot(subset(df_century)) + 
  geom_point(aes(x=year, y=sum_value, col=destination)) +
  geom_line(aes(x=year, y=sum_value, col=destination), alpha=0.5) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Exports from Britain to Channel Islands & France 1700-1800",
       x = "Year", y = "Volume of trade in estimated GBP", col = "Exports to") + 
  scale_y_continuous(breaks = seq(0, 600000, 50000)) + 
  scale_x_continuous(breaks = seq(1700, 1800, 10))
x <- df_century |> filter(destination=="France") |> 
  select(sum_value)
y <- df_century |> filter(destination=="Guernsey") |> 
  select(sum_value)
j
cor(x,y)

#2.2. Aggregating channel islands
df_century <- df_century |> 
  mutate(destination = case_when(destination == "France" ~ "France",
                                 T ~ "Channel Islands"))
df_century <- df_century |> 
  group_by(year,origin,destination) |> 
  reframe(sum_value = sum(value))

ggplot(df_century) + 
  geom_point(aes(x=year, y=sum_value, col=destination)) +
  geom_line(aes(x=year, y=sum_value, col=destination), alpha=0.5) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Exports from Britain to Channel Islands & France 1700-1800",
       x = "Year", y = "Volume of trade in estimated GBP", col = "Exports to") + 
  scale_y_continuous(breaks = seq(0, 600000, 50000)) + 
  scale_x_continuous(breaks = seq(1700, 1800, 10))

x <- df_century |> filter(destination=="France") |> 
  select(sum_value)
y <- df_century |> filter(destination=="Channel Islands") |> 
  select(sum_value)
cor(x,y)
# positive correlation, so I cannot infer Channel Islands and France as substitutes

                      