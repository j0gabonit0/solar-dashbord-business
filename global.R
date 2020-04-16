library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(scales)
library(dplR)
library(devtools)

#path <- "/Users/sascha/Nextcloud/17_solar_dashbord/solar-dashbord-business/solar_europe_de_nuts.csv"
#path_slpc <- "/Users/sascha/Nextcloud/17_solar_dashbord/solar-dashbord-business/slpc_c.csv"
#path_slpc_h <- "/Users/sascha/Nextcloud/17_solar_dashbord/solar-dashbord-business/slpc_h.csv"
#path_sedn_slpc <- "/Users/sascha/Nextcloud/17_solar_dashbord/solar-dashbord-business/sedn_slpc.csv"

path <- "C:/Users/corvi/Nextcloud/17_solar_dashbord/solar-dashbord-business/solar_europe_de_nuts.csv"
#path_slpc <- "C:/Users/corvi/Nextcloud/17_solar_dashbord/solar-dashbord-business/slpc_c.csv"
#path_slpc_h <- "C:/Users/corvi/Nextcloud/17_solar_dashbord/solar-dashbord-business/slpc_h.csv"
path_sedn_slpc <- "C:/Users/corvi/Nextcloud/17_solar_dashbord/solar-dashbord-business/sedn_slpc_bu.csv"



#solar_europe_de_nuts <- read_delim(file = path,delim = ",")
#slpc <- read_delim(file = path_slpc,delim = ",")
#slpc_h <- read_delim(file = path_slpc_h, delim =",")
sedn_slpc <- read_delim(path_sedn_slpc, delim = ",")


