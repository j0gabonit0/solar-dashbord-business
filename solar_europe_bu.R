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
library(reshape2)

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

path <- "weather_data.csv"

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
  select(-windspeed_10m) %>%
  mutate(solar_watt = (radiation_direct_horizontal + radiation_diffuse_horizontal) / 1000) #* (0.68 * ((-0.583 * temperature + 115)/100))) %>%
####################################
#3.1 Wikipedia NUTS Daten hinzufügen

path_wiki <- "wiki_nuts_tidy.csv"
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
write_csv(solar_europe_de_nuts,"/Users/sascha/NC/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage/solar-germany-nuts.csv")
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

  
  #1. Das Standardlastprfil des Konsumenten auf eine Stunde in kwh 
  
  
  
  ###Pro Tag sind es 24 * 4 = 92 Werte, pro Jahr 365 * 24 * 4 = 35.040 Messwerte. Jeder Viertelstundenwert gibt Ihre durchschnittliche Leistung in KW an. 
  ###Addieren Sie die 4 Werte einer Stunde, und teilen Sie das Ergebnis durch 4. Damit erhalten Sie den Wert eines Kilowatt pro Stunde = kWh.
  
  path_slpc_r <- "C:/Users/saschaw/Nextcloud/17_solar_dashbord/solar-dashbord-business/stursula_gym.csv"
  slpc_r <- read_delim(file = path_slpc_r, delim = ";")
  slpc_r$Datum <- as.Date(slpc_r$Datum,"%Y-%m-%d")
  
  sedn_r <- read_delim(file = path_sedn_r, delim = ",")
  path_sedn_r <- "/Users/sascha/Nextcloud/17_solar_dashbord/solar_europe_de_nuts.csv"
  
  slpc_r_2 <- slpc_r %>% #Das Standardlastprofil von 15 Minuten auf eine Stunde umrechnen
    mutate(t = as.POSIXct(paste(Datum, Zeit), format="%Y-%m-%d %H:%M:%S")) %>%
    group_by(date = floor_date(t, unit = "hour")) %>%
    summarize( kwh = sum(kw)/4)
  write_csv(slpc_r_2,"C:/Users/saschaw/Nextcloud/17_solar_dashbord/solar-dashbord-business/stursula_1.csv")
  
  
#########################
  
  
  Business
  
  #2. Join von Standardlastprofil des Konsumenten und von Werk3 und der Jahresstrahlung. Wir haben zwei Datensätze. Einmal das Profil eines durchschnittlichen Konsumenten und unsere Satellitendaten von 190-2016. Das Profil ist nur von 2011. 
  # die Datensätze müssen gejoint werden, dabei muss das datum entfernt werden und nur auf stundenbasis gejoint.
  
  
  slpc_r_2 <- read_delim("/Users/saschaw/Nextcloud/17_solar_dashbord/solar-dashbord-business/stursula_1.csv", delim = ",")
  solar_europe_de_nuts <- read_delim("C:/Users/saschaw/Nextcloud/17_solar_dashbord/solar-dashbord-business/solar_europe_de_nuts.csv", delim = ",")
  
  #1 Solarstrahlungsdaten um spalte day erweitert
  sedn_t <- solar_europe_de_nuts %>%
    mutate(day = utc_timestamp %>% as.character() %>% substr(5,19))
  
  #2 Standardlastprofil pro Stunde um day erweitert
   sedn_slp_werk1 <- slpc_r_2 %>%
    mutate(date = as.POSIXct(date, format="%d-%m-%Y %H:%M:%S")) %>%
    mutate(day = date %>% as.character() %>% substr(5,19))#%>%
    colnames(sedn_slp_werk1)[2] <- "consumw1"
    select(-z) %>% 
    mutate(day = date %>% as.character() %>% substr(5,19))
    
###############################
  #  KleineDatei erstellen
    
    
    jop <- read_delim("solar-germany-nuts.csv",delim = ",")
    
    jop <- jop %>% 
      select(-X1,-X.1,-X,-country,-Bundesland.x,-u1,-date)
    write.csv(jop, "solar-germany-nuts.csv", fileEncoding = 'UTF-8')
      
    
    jop <- jop %>% 
      unite(xday1,day1,day2,day3, sep = "-") %>% 
      unite(yday1,xday1,day4, sep = " ") %>% 
      select(-utc_timestamp) %>% 
      rename(utc_timestamp = yday1) %>% 
      mutate(utc_timestamp = as.POSIXct(utc_timestamp, format="%Y-%m-%d %H:%M:%S"))
    write_csv(jop,"solar-germany-nuts.csv")
    
    s <- jop %>% 
      filter(utc_timestamp >= "2010-01-01 00:00:00", utc_timestamp <="2015-12-31 24:00:00") 
      
    write.csv(s, "solar-germany-nuts-short.csv", fileEncoding = 'UTF-8')
    

    
######################### Bearbeiten
    
    sedn_slpc <- read.delim("solar-germany-nuts.csv", sep = ",")
    write.csv(sedn_slpc1, "solar-germany-nuts.csv", fileEncoding = 'UTF-8')

    
    un <- read.delim("sun.csv", sep= ";")
    sedn_slpc <- sedn_slpc %>% 
    mutate(u1 = utc_timestamp %>% as.character() %>% substr(6,19))
    sun <- sun %>% 
      mutate(u1 = woz %>% as.character() %>% substr(6,19))
    
    
    
    sedn_slpc1 <- sedn_slpc %>% 
      left_join(sun, by = "u1")
      write.csv(sedn_slpc1, "solar-germany-nuts.csv",  fileEncoding = 'UTF-8'))
    
    
    x <- read.delim("new.csv", sep = ",")
    x <- x %>%
      select(-X.1, -X, -Bundesland.y)
    
    write.csv(y, "solar-germany-nuts.csv", fileEncoding = 'UTF-8')
  

  

##########################################################################################################
  
  sedn_slpc <- read_delim("C:/Users/saschaw/Nextcloud/17_solar_dashbord/solar-dashbord-business/sedn_slpc_bu_w3.csv", delim = ",")
  #if schleife Eigenverbrauch
  #Selbstverbrauch = 1. Produktion < Verbrauch = Produktion e1
  #Selbstverbrauch = 2. Produktion > Verbrauch = Verbrauch e2
  #Verkauf 1 = Produktion > Verbrauch v1

      er <- sedn_slpc %>%
      filter(Stadt == "Stuttgart") %>%
      filter(utc_timestamp >= "1990-01-01 00:00:00", utc_timestamp <="1990-12-31 24:00:00") %>% 
      mutate(swm2 = solar_watt * 500 * 0.2) %>%
      mutate(e1 = ifelse(swm2 < consumw1, swm2, consumw1)) %>%
      mutate(v1 = ifelse(swm2 > consumw1, swm2 - consumw1, 0)) %>%
      summarise(production = sum(swm2),ev = (sum(e1, na.rm = TRUE) / 1), (es = sum(v1, na.rm = TRUE) / 1), ge = ev + es, test = mean(v1)) 
      mean(sedn_slpc$consumw1)
      write_csv(er,"C:/Users/corvi/Nextcloud/17_solar_dashbord/solar-dashbord-business/teest2.csv")
      
      #### fehlende Werte in Schaltjahren -Problem
      
      j <- sedn_slpc %>%
        filter(Stadt == "Stuttgart") %>%
        filter(utc_timestamp >= "1990-01-01 00:00:00", utc_timestamp <="2010-12-31 24:00:00") %>% 
        mutate(day = utc_timestamp %>% as.character() %>% substr(6,19)) %>%
        mutate(swm2 = solar_watt * 0.2 * 500) %>%
        mutate(e1 = ifelse(swm2 < consumw1 , swm2, ifelse(swm2 > consumw1, consumw1 , 0))) %>%
        mutate(v1 = ifelse(swm2 > consumw1, swm2 - consumw1, 0)) %>%
        group_by(day) %>%
        summarise(e = mean(e1), eq = sum(e1), v = mean(v1)) %>% 
        mutate(date = as.POSIXct(paste0("2020-", day), format = c("%Y-%m-%d %H:%M:%S"))) %>%
        mutate(month = month(date)) %>% 
        group_by(month) %>% 
        summarize(em = floor(sum(e, na.rm = TRUE)), vm = floor((sum(v, na.rm = TRUE)))) #%>% 
        mutate(em_perc = em/sum(em,na.rm = TRUE)) %>% 
        mutate(vm_perc = vm/sum(vm,na.rm = TRUE)) %>% 
        select(-em,-vm) #%>% 
          ggplot(j, aes(x = month)) +
          geom_line(aes(y = em),stat = "identity") +
          geom_line(aes(y = vm),stat = "identity") +
          geom_text(mapping = aes(x = month, y = em, label = em)) +
          geom_text(mapping = aes(x = month, y = vm, label = vm))
            
          
          
         jop <- read_delim("/Users/corvi/Nextcloud-Stiftung/17_solar_dashbord/solar-dashbord-business/proof_Kopie.csv",delim = ";")
         sedn_slpc <- sedn_slpc %>%
           mutate(day = utc_timestamp %>% as.character() %>% substr(5,19))
         l <- jop %>% 
           mutate(date_full = seq(ymd_hm('2019-01-01 00:00'),ymd_hm('2019-12-31 23:45'), by = '15 mins')) %>% 
           mutate(date_full = as.POSIXct(date_full, format="%Y-%m-%d %H:%M:%S")) %>% 
           group_by(date = floor_date(date_full, unit = "hour")) %>%
           summarize( kwh = sum(kwh)/4) %>% 
           mutate(date = as.POSIXct(date, format="%Y-%m-%d %H:%M:%S")) %>% 
           mutate(day = date %>% as.character() %>% substr(5,19)) %>% 
           right_join(sedn_slpc,by = "day")  %>% 
           select(-day,-date,-consumw1) %>% 
           rename(consumw1 = kwh)


         data <- read.csv(input$file$datapath)
         sedn_slpc2 %>% 
           data %>%
           mutate(date_full = seq(
             ymd_hm('2019-01-01 00:00'),
             ymd_hm('2019-12-31 23:45'),
             by = '15 mins'
           )) %>%
           mutate(date_full = as.POSIXct(date_full, format = "%Y-%m-%d %H:%M:%S")) %>%
           group_by(date = floor_date(date_full, unit = "hour")) %>%
           summarize(kwh = sum(kwh) / 4) %>%
           mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) %>%
           mutate(day = date %>% as.character() %>% substr(5, 19)) %>%
           right_join(sedn_slpc2, by = "day")  %>%
           select(-day, -date, -consumw1) %>%
           rename(consumw1 = kwh) %>%
           summarise(jo = sum(consumw1, na.rm = TRUE))
         })
     