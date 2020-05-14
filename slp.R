
# Join des Standardlastprfils des Kunden mit den Solardaten

# Berechnung kw in kWh
#   Pro Tag sind es 24 * 4 = 92 Werte, pro Jahr 365 * 24 * 4 = 35.040 Messwerte. Jeder Viertelstundenwert gibt Ihre durchschnittliche Leistung in KW an. 
#   Addieren Sie die 4 Werte einer Stunde, und teilen Sie das Ergebnis durch 4. Damit erhalten Sie den Wert eines Kilowatt pro Stunde = kWh.



slpc_r <- read_delim("C:/Users/saschaw/Nextcloud/17_solar_dashbord/solar-dashbord-business/stursula_gym.csv", delim = ";")
sedn_r <- read_delim("/Users/saschaw/Nextcloud/17_solar_dashbord/solar_europe_de_nuts.csv", delim = ",")
sedn_t <- solar_europe_de_nuts %>%
  mutate(day = utc_timestamp %>% as.character() %>% substr(5,19))

sedn_slpc <- slpc_r %>% 
  mutate(t = as.POSIXct(paste(Datum, Zeit), format="%Y-%m-%d %H:%M:%S")) %>%
  group_by(date = floor_date(t, unit = "hour")) %>%
  summarize( kwh = sum(kw)/4) %>% 
  mutate(date = as.POSIXct(date, format="%d-%m-%Y %H:%M:%S")) %>%
  mutate(day = date %>% as.character() %>% substr(5,19))# %>%
  right_join(sedn_t,by = "day") %>% 
  select(-day,-date) %>% 
  rename(consumw1 = kwh)
  write_csv(sedn_slpc,"C:/Users/saschaw/Nextcloud/17_solar_dashbord/solar-dashbord-business/sedn_slpc_bu_w3_proof.csv")

  #colnames(sedn_slp_werk1)[2] <- "consumw1"
  
