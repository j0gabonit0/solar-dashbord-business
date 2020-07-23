####### Sonnenstandsdiagramm ######
# Mithilfe dieses Diagramms wird der Sonnenstand und die Sonnenh?he erfasst. Mit der Sonnenh?he kann der Einfalsswinkel der direktstrahlung der Sonne auf ein Photovoltaikmodul berechnet werden.
#/Users/sascha/NC/17_solar_dashbord/solar-dashbord-business/Sonnenstand
#C:/Users/sascha/Nextcloud/17_solar_dashbord/solar-dashbord-business/Sonnenstand/sun.csv
#C:/Users/corvi/Nextcloud-Stiftung/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage


# Strahlung der Sonne auf einem PV-Modul = Direktstrahlung + diffuse Strahlung + reflektierende Strahlung

direct_radiation_pv = direct_radiation * (sin(sonnenhoehe + neigung) / sin(sonnenhoehe))
diffuse_radiation_pv = diffuse_radiation * 1/2 * (1 + cos(sonnenhoehe))
reflective_radiation_pv = direct_radiation + diffuse_radiation * 0.5 * (1 - cos(sonnenhoehe)) * alb

# Leistungsfähigkeit eines PV-Moduls ist abhängig von der Außentemperatur
perform_modul = global_radiation * (0.68 * ((-0.583 * temperature + 115)/100))
  
#Sonnentabelle einlesen
sun_raw <- read.delim(file = "C:/Users/corvi/Nextcloud-Stiftung/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage/sun.csv", sep = ";")

# Sonnentabelle Datum formatieren  
sun <- sun_raw %>% 
    mutate(date = as.POSIXct(date, format="%Y-%m-%d %H:%M:%S")) %>%
    mutate(woz = as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"))

#Globalstrahlungskarte Deutschland unterteilt in Nuts einlesen  
solar_germany_nuts <- read.delim(file ="C:/Users/corvi/Nextcloud-Stiftung/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage/solar-germany-nuts-2.csv", sep = ",")
solar_germany_nuts <- solar_germany_nuts %>% 
  mutate(day = utc_timestamp %>% as.character() %>% substr(0,10)) %>% 
  mutate(time = utc_timestamp %>% as.character() %>% substr(12,19)) %>% 
  unite( x,day,time, sep = " ", remove = TRUE) %>% 
  mutate(utc_timestamp = as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S",na.rm = TRUE)) %>%
  mutate(Stadt = Stadt %>% as.character())  %>% 
  mutate(Bundesland = Bundesland %>% as.character()) %>% 
  mutate(country = country %>% as.character()) %>% 
  select(-x)
  
  
  #filter(utc_timestamp >= "2010-12-31 24:00:00")



#Globale Variablen zum testen - Ort = Münster
latitude = 52
alb = 0.20

#Variabeln

#Werte
#0,017453 ist zur Umrechnung 

#Deklination
#Formel: 23,45*COS(0,017453*360*(daynumber+10)/365)
sun <- sun %>%
  mutate(dec = -23.45 * cos(0.017453 * 360 * (daynumber + 10) / 365))

#Zeitgleichung
#=60*(-0,171*SIN(0,0337*daynumber + 0,465) - 0,1299*SIN(0,01787*daynumber  - 0,168))
sun <- sun %>%
  mutate(zeitgl = 60 * (
    -0.171 * sin(0.0337 * daynumber + 0.465) - 0.1299 * sin(0.01787 * daynumber  - 0.168)
  ))

#Stundenwinkel
#=15*(stunde(Date)+minute(date)/60-(15-latitude)/15-12+I2/60)
sun <- sun %>%
  mutate(stundenwinkel = 
    15*(hour(date)+minute(date)/60-(15-latitude)/15-12+zeitgl/60)
  )

#sin(Sonnenh?he)
#=SIN(0,017453*$B$3)*SIN(0,017453*H2)+COS(0,017453*$B$3)*COS(0,017453*H2)*COS(0,017453*J2)
sun <- sun %>%
  mutate(
    sin_sonnenhoehe = ((sin(0.017453 * latitude) * sin(0.017453 * dec)) + (cos(0.017453 * latitude) * cos(0.017453 * dec) * cos(0.017453 * stundenwinkel)))
  )

#Sonnenh?he
#ARCSIN(sin(Sonnenh?he))/0,017453
sun <- sun %>%
  mutate(sonnenhoehe = (asin(sin_sonnenhoehe) / 0.017453) 
  )

#cos(Azimut)
# -(SIN(0,017453*Latitude)*K3-SIN(0,017453*dec))/(COS(0,017453*Latitude)*SIN(ARCCOS(sin(sonnenhoehe))))
sun <- sun %>%
  mutate(cos_azimut = -(sin(0.017453 * 52) * sin_sonnenhoehe - sin(0.017453 * dec)) / (cos(0.017453 * 52) * sin(acos(sin_sonnenhoehe)))
  )

#Azimut
#WENN(Stunde + Minuten/60<=12+(15-Latitude)/15-Zeitgleichung/60;ARCCOS(cos_Azimut)/0,017453;360-ARCCOS(cos-Azimut)/0,017453)
sun <- sun %>%
  mutate(azimut = ifelse((hour(date) + (minute(date) / 60)) <= 12 + ((15 - latitude) / 15) - (zeitgl / 60) ,
                         acos(cos_azimut) / 0.017453 ,
                         360 - (acos(cos_azimut) / 0.017453)
  )
  )




