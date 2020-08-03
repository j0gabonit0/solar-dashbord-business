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



path_sedn_slpc <- "C:/Users/corvi/Nextcloud-Stiftung/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage/solar-germany-nuts.csv"
#path_sedn_slpc <- "C:/Users/corvi/Nextcloud-Stiftung/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage/old/sedn_slpc_bu.csv"

# sedn_slpc = SolarEuropeDeutschlandNuts_StandarLastProfilConsumer

sedn_slpc <- read_delim(path_sedn_slpc, delim = ",")
sedn_slpc <- sedn_slpc %>%
  mutate(utc_timestamp = as.POSIXct(utc_timestamp), format = c("%Y-%m-%d %H:%M:%S")) %>% 
  mutate(u1 = utc_timestamp %>% as.character() %>% substr(6, 19))

# Sicherheitsfaktor 95% der voraussichtlichen Leistung - Dies wird pauschal angesetzt.
sf = 1

# Leistungsabnahme des Moduls ueber 20 Jahre durchschnittlich 0,05% = 20 * 0,005% = 10%. Wenn die Abnahme über 20 Jahre 10 % ist, dann kann man den Startwert der Module * 95% rechnen.
module_reduce = 1

# Quadratmeter für 1 kWp
m2kwp = 5

years = 21

#Eigenverbrauch  = consum
#Netzeinspeisung = sale