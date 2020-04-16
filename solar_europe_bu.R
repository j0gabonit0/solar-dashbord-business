# install package tidyverse
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(scales)
library(dplR)
library(lubridate)

#############
#Shinydashbord neu installieren
install.packages("devtools")
library("devtools")
install_github("rstudio/shinydashboard")

##############
#Error
#2 Filtering hart nicht richtig funktioniert, ich habe es überall manuell eingefügt

############
#1.Schritt - Rohdaten in eine neue Tabelle schreiben

path <- "/Users/sascha/Nextcloud/17_solar_dashbord/opsd-weather_data-2019-04-09_WerteTemperatur-Einstrahlung/weather_data.csv"

raw_opsd <- read_delim(file = path,delim = ",") # Rohdaten 
solar_europe <-raw_opsd # Ursprungstabelle

########################
# 2. Schritt - Umwandeln der Rohdaten in tidydata

solar_europe_de <- solar_europe %>%
  select(utc_timestamp, starts_with("DE")) %>%
  filter(utc_timestamp >= "1989-12-31 24:00:00") %>%
  distinct(utc_timestamp, .keep_all = TRUE) %>%
  pivot_longer(-utc_timestamp, names_to = "country", values_to = "wm2") %>% # Rohdaten umgeändert in drei Spalten
  separate(country, into = c("country","metric"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = metric, values_from = wm2)

rlang::last_error
####################################
#3.Schritt - Überflüssige Daten entfernen Windspeed/

solar_europe_de_w <- solar_europe_de %>%
  select(-windspeed_10m) %>% # Windspeed entfernen
  mutate(global_radiation = (radiation_direct_horizontal + radiation_diffuse_horizontal) / 1000 ,
         solar_watt  = global_radiation * (0.68 * ((-0.583 * temperature + 115)/100))) %>%
  select(-radiation_direct_horizontal,-radiation_diffuse_horizontal)
####################################
#3.1 Wikipedia NUTS Daten hinzufügen

path_wiki <- "/Users/sascha/Nextcloud/17_solar_dashbord/wiki_nuts_tidy.csv"
wiki_nuts <- read_delim(file = path_wiki,delim = ",")
solar_europe_de_nuts <- inner_join(solar_europe_de_w, wiki_nuts, by = c("country" = "NUTS2"))

####################################
#Standardlastprofil

#slpc aus https://swbt-netz.de/
# Addieren Sie die 4 Werte einer Stunde, und teilen Sie das Ergebnis durch 4. Damit erhalten Sie den Wert eines Kilowatt pro Stunde = kWh. 


path_slpc_d <- "/Users/sascha/Nextcloud/17_solar_dashbord/slpc.csv"
slpc_d <- read_delim(file = path_slpc_d, delim = ";") %>%
  #mutate(date = Datum %>% as.Date("%Y.%m.%d") %>% as.character() %>% substr(6,10) %>% paste0("2019-", .) %>% as.Date()) %>%
  mutate(date = Datum) %>%
  group_by(date) %>%
  mutate(watt = gsub(",", ".",kw) %>% as.numeric()) %>%
  summarize(standardlast = sum(watt) / 4 )



#####################################
#3.3 Datei abspeichern für Shiny
write_csv(slpc_d,"/Users/sascha/Nextcloud/17_solar_dashbord/slpc_d.csv")
write_csv(solar_europe_de_nuts,"/Users/sascha/Nextcloud/17_solar_dashbord/solar_europe_de_nuts.csv")
################################################
#4.Schritt - Durchschnittliche Sonneneinstrahlung vor Ort m2

durchschnitt <- function(startdate, enddate, selected_country) {
  solar_europe_de_nuts %>%
  filter(country == selected_country) %>%
  filter(utc_timestamp >= startdate, utc_timestamp <= enddate) %>%
  summarize(
    rad = mean(global_radiation, na.rm = TRUE),
    max = max(global_radiation),
    min = min(global_radiation[which(global_radiation > 0)]),
    summe = sum()
      )
}
durchschnitt("2014-01-01 06:00:00","2015-01-01 24:00:00","DE11")

############################################################
#5.Schritt - Erzeugte kwH / m2 und erforderliche m2 für Jahresverbrauch

m2 <- function(kwhy, efficency = 0.2, startyear, endyear, selected_country){
  years <- endyear - startyear + 1
  solar_europe_de_nuts %>%
    filter(country == selected_country) %>%
    filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00")) %>%
    summarise(
      yieldm2 = sum(solar_watt) / years * efficency / 1000, # Die kWh die pro Jahr vom Modul pro m2 erzeugt werden können
      m = kwhy/yieldm2 # die benötigte Fläche für die angegebenen kwH im jahr
           )
}

m2(kwhy = 100,startyear = 2005, endyear = 2015, selected_country = "DE60") # 

 ########################################################################
#6.Schritt - Erzeugte Jahresertrag bei angegebener Dachfläche

kwhyield <-function(startyear,endyear,efficency,selected_country,m2,price){
  years <- endyear - startyear + 1
  solar_europe_de_nuts %>%
    filter(country == selected_country) %>%
    filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00")) %>%
    summarise(
      yield = sum(solar_watt) / years * efficency * m2 / 1000,
    )    
}
  
kwhyield(startyear = 2009, endyear = 2016,efficency = 0.2, selected_country = "DE60", m2 = 384) # 

###########################################################################################
#7.Schritt ggplot

solar_europe_de_nuts %>%
  filter(country == "DE11") %>%
  filter(utc_timestamp >= startdate, utc_timestamp <= enddate)
  mutate(day = utc_timestamp %>% as.character() %>% substr(6,10)) %>%
  group_by(day) %>%
  summarize(avg = mean(solar_watt, na.rm = TRUE), std = sd(solar_watt, na.rm = TRUE) / sqrt(n())) %>%
  mutate(date = as.Date(paste0("2019-", day))) %>%
  ggplot() + 
    aes(x = date, y = avg) +
    geom_ribbon(aes(ymin = avg - 1.96 * std, ymax = avg + 1.96 * std), fill = "grey70") +
    geom_line()

#########################

  
  #1. Das Standardlastprfil des Konsumenten auf eine Stunde in kwh umrechnen
  
  path_slpc_r <- "/Users/sascha/Nextcloud/17_solar_dashbord/slpc.csv"
  slpc_r <- read_delim(file = path_slpc_r, delim = ";")
  slpc_r$Datum <- as.Date(slpc_r$Datum,"%Y-%m-%d")
  
  sedn_r <- read_delim(file = path_sedn_r, delim = ",")
  path_sedn_r <- "/Users/sascha/Nextcloud/17_solar_dashbord/solar_europe_de_nuts.csv"
  
  slpc_r_2 <- slpc_r %>% #Das Standardlastprofil von 15 Minuten auf eine Stunde umrechnen
    mutate(t = as.POSIXct(paste(Datum, Zeit), format="%Y-%m-%d %H:%M:%S")) %>%
    group_by(datetime = floor_date(t, unit = "hour")) %>%
    summarize( kwh = sum(kw))
  write_csv(slpc_r_2,"/Users/sascha/Nextcloud/17_solar_dashbord/slpc_h.csv")
  
  
#########################
  
  
  Business
  
  #2. Join von Standardlastprofil des Konsumenten und von Werk3 und der Jahresstrahlung. Wir haben zwei Datensätze. Einmal das Profil eines durchschnittlichen Konsumenten und unsere Satellitendaten von 190-2016. Das Profil ist nur von 2011. 
  # die Datensätze müssen gejoint werden, dabei muss das datum entfernt werden und nur auf stundenbasis gejoint.
  
  
  slp_w3 <- read_delim("C:/Users/corvi/Nextcloud/17_solar_dashbord/solar-dashbord-business/slp_w3.csv", delim = ",")
  solar_europe_de_nuts <- read_delim("C:/Users/corvi/Nextcloud/17_solar_dashbord/solar-dashbord-business/solar_europe_de_nuts.csv", delim = ",")
  
  #1 Solarstrahlungsdaten um spalte day erweitert
  sedn_t <- solar_europe_de_nuts %>%
    mutate(day = utc_timestamp %>% as.character() %>% substr(5,18))
  
  #2 Standardlastprofil pro Stunde um day erweitert
   sedn_slp_werk3 <- slp_w3 %>%
    mutate(day = date %>% as.character() %>% substr(0,16))%>%
    mutate(day1 = date %>% as.character() %>% substr(20,22)) %>% 
    unite("z", day, day1, sep = "") %>% 
    select(-date) %>% 
    mutate(date = as.POSIXct(z, format="%d-%m-%Y %H:%M:%S")) %>% 
    select(-z) %>% 
    mutate(day = date %>% as.character() %>% substr(5,18))
    colnames(sedn_slp_werk3)[1] <- "consumw1"
    
  
  #3Standardlastprofil an Solarstrahlungsdaten gejoint
 
  sedn_slpc <- left_join(sedn_t, sedn_slp_werk3, by = "day") %>% 
    select(-date, -day)
  
  #4 Neue Datentabelle in csv geschrieben
  write_csv(sedn_slpc,"C:/Users/corvi/Nextcloud/17_solar_dashbord/sedn_slpc.csv")
##########################################################################################################
  

  #if schleife Eigenverbrauch
  #Selbstverbrauch = 1. Produktion < Verbrauch = Produktion e1
  #Selbstverbrauch = 2. Produktion > Verbrauch = Verbrauch e2
  #Verkauf 1 = Produktion > Verbrauch v1
 
  
  t1 <- sedn_slpc %>%
  mutate(e1 = ifelse(solar_watt < kwh, solar_watt, ifelse(solar_watt > kwh, kwh , 0))) %>%
  mutate(v1 = ifelse(solar_watt > kwh, solar_watt - kwh, 0)) %>%
  mutate(day = utc_timestamp %>% as.character() %>% substr(6,19)) 
  group_by(day) %>%
  summarize(e = mean(e1), v = mean(v1))
  
  
  erlös <- sedn_slpc %>%
      mutate(swm2 = solar_watt * 1) %>%
      mutate(kwhd = kwh / 4000 * 4000) %>%
      mutate(ec = swm2 - kwhd)%>%
      mutate(el = ifelse(swm2 < kwhd, swm2, ifelse(swm2 > kwhd, kwhd , 0))) %>%
      mutate(vk = ifelse(swm2 > kwhd, swm2 - kwhd, 0)) %>%
      summarise(ev = sum(el, na.rm = TRUE), es = sum(vk, na.rm = TRUE))
  
  zin = 5*3*3
zin  

cy = (((50/5) * (1300/20) + (((20 * 10) * (50/5)) / 20)))
cy

