library(tidyverse)
library(ggplot2)
library(readxl)
library(stringr)
  
  # cotton <- read_excel("~/printed cotton exports Britain to select Europe.xlsx")
  # cotton <- cotton |> mutate(Region = as.factor(Region))
  # ggplot(subset(cotton, Year!=1800)) + geom_line(aes(x=Year, y=Quantity,col=Region))
  
  df <- read_csv("~/2024/2024 Diss code/input/data Eden Treaty.csv")
  unique(df$partner)
  unique(df$product)
  # clean product classification (unfunished)  
  df <- df |>
    mutate(product = case_when(str_detect(product, "Coton") ~ "Coton",
                               str_detect(product, "Laine") ~ "Laine",
                               str_detect(product, "Lin") ~ "Lin",
                               str_detect(product, "Bois") ~ "Bois",
                               str_detect(product, "Gomme") ~ "Gommes",
                               str_detect(product, "Graine") ~ "Graines",
                               .default = product))
  # clean partner classification
  df <- df |> mutate(partner = case_when(str_detect(partner, "Espag") ~ "Spain",
                               str_detect(partner, "(?i)Toscane") ~ "Tuscany",
                               str_detect(partner, "(?i)Gène") | str_detect(partner, "(?i)Genes") ~ "Genoa",
                               str_detect(partner, "(?i)Naples") ~ "Naples",
                               str_detect(partner, "(?i)Sardaigne") ~ "Sardinia",
                               str_detect(partner, "Autrich") ~ "Austria",
                               str_detect(partner, "(?i)Angleterre") ~ "England",
                               str_detect(partner, "(?i)Rome") | str_detect(partner, "(?i)Italie")  ~ "Italy",
                               str_detect(partner, "(?i)Hollande") ~ "Hollande",
                               str_detect(partner, "Portugal") | str_detect(partner, "Potugal") ~ "Portugal",
                               str_detect(partner, "(?i)Colonies Fran[çc][oa]ises") | str_detect(partner, "Isles Françoises") ~ "French Colonies",
                               str_detect(partner, "(?i)Etats[- ]Unis") ~ "USA",
                               str_detect(partner, "(?i)suisse") | str_detect(partner, "(?i)G[èe]n[eè]ve") ~ "Switzerland",
                               str_detect(partner, "(?i)Su[eè]de") ~ "Sweden",
                               str_detect(partner, "(?i)Venise") ~ "Venice", #15
                               str_detect(partner, "(?i)Etats-Unis") ~ "USA",
                               str_detect(partner, "Danemar[ck]") ~ "Denmark",
                               str_detect(partner, "(?i)ottoman") ~ "Ottoman Empire",
                               str_detect(partner, "Flandre") ~ "Flandres",
                               str_detect(partner, "(?i)indes") ~ "Indies",
                               str_detect(partner, "(?i)ans[ée]atique") ~ "Anseatic states",
                               str_detect(partner, "(?i)Russie") ~ "Russia",
                               str_detect(partner, "(?i)Levant") | str_detect(partner, "(?i)Barbarie") ~ "Levant", 
                               str_detect(partner, "(?i)Prusse") |  str_detect(partner, "(?i)Allemagne") ~ "German states", 
                               .default = partner))
  
  partner_names <- df$partner |> unique() 
  partner_names <- partner_names[(1:21)]
df <- df |> filter(partner %in% partner_names && import == TRUE)
aggr <- aggregate(formula = value ~ partner + year, data = df, FUN=sum)

ggplot(subset(aggr, partner %in% c("England", "Austria", "Flandres", "Italy", "Portugal", "German states"))) +
  geom_point(aes(x=year, y=value, col = partner), size=1) +
  geom_line(aes(x=year, y=value, col = partner), size=1, alpha=0.3) +
  scale_x_continuous(breaks = seq(1721, 1797, 2)) + 
  geom_vline(xintercept = 1786, linetype = "dashed", color = "red") +
  labs(title= "Number of trade flows with European Partners 1740-1800", x = "Year", y = "", col = "Trading Region")


ggplot(subset(df, product %in% c("Laine","Coton","Lin","Bois","Gommes","Graines"))) +
         geom_line(aes(x=year, y="")) + facet_wrap(~product)

ggplot(subset(df, product %in% c("Laine","Coton","Lin", "Graines") & year>1766)) +
  geom_point(aes(x=year, y=value, col = product), size=1) +
  geom_line(aes(x=year, y=value, col = product), size=1, alpha=0.3) +
  scale_x_continuous(breaks = seq(1721, 1797, 2))

