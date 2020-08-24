library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(scales)
library(dplR)
library(devtools)
library(DT)
library(lubridate)
library(reshape2)
library(ggpubr)
library(aspace)
library(plotly)

#b 
#x <- "C:/Users/corvi/Nextcloud-Stiftung/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage/solar-germany-nuts-short.csv"
#iw 
#x <- "C:/Users/saschaw/NC/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage/solar-germany-nuts-short.csv"
#m
#x <- "/Volumes/BOOTCAMP/Users/sascha/Nextcloud/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage/solar-germany-nuts-short.csv"
x <- "/Users/sascha/NC/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage/solar-germany-nuts-short.csv"

path_sedn_slpc <- x
#path_sedn_slpc <- "C:/Users/corvi/Nextcloud-Stiftung/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage/sedn_slpc_bu.csv"



# sedn_slpc = SolarEuropeDeutschlandNuts_StandarLastProfilConsumer

sedn_slpc <- read_delim(path_sedn_slpc, delim = ",")
sedn_slpc <- sedn_slpc %>%
  mutate(utc_timestamp = as.POSIXct(utc_timestamp), format = c("%Y-%m-%d %H:%M:%S")) %>% 
  mutate(u1 = utc_timestamp %>% as.character() %>% substr(6, 19))

nuts_123_germany <- read.csv("/Users/sascha/NC/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage/nuts_123_germany.csv", encoding = "UTF-8")

nuts_123_germany <- nuts_123_germany %>% 
  mutate(nuts2 = nuts2 %>% as.character()) %>% 
  mutate(place = place %>% as.character())

# Sicherheitsfaktor 95% der voraussichtlichen Leistung - Dies wird pauschal angesetzt.
sf = 1

# Leistungsabnahme des Moduls ueber 20 Jahre durchschnittlich 0,05% = 20 * 0,005% = 10%. Wenn die Abnahme über 20 Jahre 10 % ist, dann kann man den Startwert der Module * 95% rechnen.
module_reduce = 0.95

# Quadratmeter für 1 kWp
m2kwp = 5.2

startyear <- 2010
endyear <- 2012
years = 3

#Losses

anti_reflection_modul = 0.95 # Die Reflektionsverluste entstehen durch Reflektion des Lichts durch das Modul. Diese wird durch eine spezielle Schicht im Modul auc ca.5% reduziert, sodass ein Ertrag von 95% bleibt = 0.95
wechselrichter_wigrad = 0.98 # Der maximale Wirkungsgrad von Wechselrichtern ist 98%. Dies bedeutet, dass 98% des Stroms umgewandelt werden und 2% verloren gehen. F?r die Berechnungen sind pauschal 0.95% anzusetzen. Dies kann beispielsweise durch l?ngere Kabelwege passieren.

#Eigenverbrauch  = consum
#Netzeinspeisung = sale