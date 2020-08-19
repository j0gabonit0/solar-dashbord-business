
# Join des Standardlastprfils des Kunden mit den Solardaten

# Berechnung kw in kWh
#   Pro Tag sind es 24 * 4 = 92 Werte, pro Jahr 365 * 24 * 4 = 35.040 Messwerte. Jeder Viertelstundenwert gibt Ihre durchschnittliche Leistung in KW an. 
#   Addieren Sie die 4 Werte einer Stunde, und teilen Sie das Ergebnis durch 4. Damit erhalten Sie den Wert eines Kilowatt pro Stunde = kWh.



slpc_r <- read_delim("/Users/sascha/NC/17_solar_dashbord/solar-dashbord-business/Rohdaten/stursula_real_slp.csv", delim = ";")
sedn_r <- read_delim("/Users/sascha/NC/17_solar_dashbord/solar-dashbord-business/solar_europe_de_nuts.csv", delim = ",")
sedn_r <- solar_europe_de_nuts
sedn_t <- sedn_r %>%
  mutate(day = utc_timestamp %>% as.character() %>% substr(5,19))

sedn_slpc <- slpc_r %>% 
  mutate(date = as.POSIXct(date, format="%Y-%m-%d %H:%M:%S")) %>%
  group_by(date = floor_date(date, unit = "hour")) %>%
  summarise(kwh = sum(consumw1) / 4) %>% 
  mutate(day = date %>% as.character() %>% substr(5,19)) %>% 
  right_join(sedn_t,by = "day") %>% 
  select(-day,-date) %>% 
  rename(consumw1 = kwh) %>% 
  #select(-country,-temperature,-global_radiation) 
  write_csv("C:/Users/corvi/Nextcloud/17_solar_dashbord/solar-dashbord-business/sedn_slpc_bu.csv")


  
 # Proof
  
  sedn_slpc <- slpc_r %>% 
    mutate(t = as.POSIXct(paste(Datum, Zeit), format="%Y-%m-%d %H:%M:%S")) %>%
    group_by(date = floor_date(t, unit = "hour")) %>%
    summarize( kwh = sum(kwh)/4) %>% 
    mutate(date = as.POSIXct(date, format="%d-%m-%Y %H:%M:%S")) %>%
    mutate(day = date %>% as.character() %>% substr(5,19)) %>%
    right_join(sedn_t,by = "day") %>% 
    select(-day,-date) %>% 
    rename(consumw1 = kwh) %>% 
    filter(Stadt == "M?nster") %>%
    filter(utc_timestamp >= "1990-01-01 00:00:00", utc_timestamp <="1995-12-31 00:00:00") %>% 
    select(consumw1, solar_watt, utc_timestamp) %>% 
    write_csv("C:/Users/corvi/Nextcloud/17_solar_dashbord/solar-dashbord-business/proof.csv")
  
  #colnames(sedn_slp_werk1)[2] <- "consumw1"
