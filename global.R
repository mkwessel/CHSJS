options(dplyr.summarise.inform = FALSE)
library(shinyWidgets)
library(shiny) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(leaflegend)
library(plotly)
library(bslib)
library(sf)

percentile <- function(x){
  rank_x = rank(x)
  rank_x/max(rank_x) * 100
}

df_raw = readRDS(url("https://github.com/mkwessel/CHSJS/raw/main/CHSJS_WQ_Data.rds"))

site_locs = df_raw |> 
  select(Site, Lat, Lon) |> 
  distinct()

df = df_raw |> 
  mutate(Parameter_Units = ifelse(is.na(Units), Parameter, paste0(Parameter, " (", Units, ")"))) |> 
  select(Date, Level, Parameter = Parameter_Units, Site, Value = Result)

min_date = min(df[["Date"]], na.rm = TRUE)
max_date = max(df[["Date"]], na.rm = TRUE)
date_seq =  seq(from = min_date, to = max_date, by = "day")
date_lab = format(date_seq, "%b %d")

