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
sun_raw <- read.delim(file = "sun.csv", sep = ";")

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
latitude = 52.51
longitude = 13.41
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


#############
latitude = 40
longitude = -100
tilt_angle_modul = 40
azimuth_angle_modul = 90


sedn_slpc <- read_delim(path_sedn_slpc, delim = ",")

x <- sedn_slpc %>% 
  filter(Stadt == "Berlin") %>%
  filter(
    utc_timestamp >= "1995-01-01 00:00:00",
    utc_timestamp <= "2015-12-31 24:00:00"
  ) %>% 
  mutate(dec = -23.45 * cos(0.017453 * 360 * (daynumber + 10) / 365)) %>% 
  mutate(zeitgl = 60 * (-0.171 * sin(0.0337 * daynumber + 0.465) - 0.1299 * sin(0.01787 * daynumber  - 0.168))) %>% 
  mutate(stundenwinkel = 15*(hour(utc_timestamp) + (minute(utc_timestamp)/60) - ((15 - longitude)/15)- 12 + (zeitgl / 60))) %>% 
  mutate(sin_sonnenhoehe = ((sin_d(latitude) * sin_d(dec)) + (cos_d(latitude) * cos_d(dec) * cos_d(stundenwinkel)))) %>% 
  mutate(sonnenhoehe = (asin_d(sin_sonnenhoehe))) %>% 
  mutate(cos_azimut = -((sin_d(latitude) * sin_sonnenhoehe) - (sin_d(dec))) / (cos_d(latitude) * sin_d(acos_d(sin_sonnenhoehe)))) %>% 
  mutate(azimut = ifelse((hour(utc_timestamp) + (minute(utc_timestamp) / 60)) <= 12 + ((15 - longitude) / 15) - (zeitgl / 60) , acos_d(cos_azimut) , 360 - (acos_d(cos_azimut)))) %>%  
  mutate(zenith_angle = 90 - sonnenhoehe) %>% 
  mutate(angle_of_incidance = (cos_d(sonnenhoehe) * cos_d(azimuth - azimuth_angle_modul) * sin_d(tilt_angle_modul)) + (sin_d(sonnenhoehe) * cos_d(tilt_angle_modul))) %>% 
  mutate(direct_radiation_pv = ifelse(sonnenhoehe > 0, radiation_direct_horizontal * (sin((sonnenhoehe + 10)*0.017453) / sin(sonnenhoehe * 0.017453)),0)) %>% 
  mutate(diffuse_radiation_pv = radiation_diffuse_horizontal * 1/2 * (1 + cos(sonnenhoehe * 0.017453))) %>% 
  mutate(reflective_radiation_pv = (direct_radiation_pv + diffuse_radiation_pv) * 0.5 * (1 - cos(sonnenhoehe * 0.017453)) * 0.2) %>% 
  mutate(solar_watt = ((direct_radiation_pv + diffuse_radiation_pv + reflective_radiation_pv) / 1000) * (-0.583 * temperature + 115)/100) %>% 
  filter(utc_timestamp == "2015-07-15 18:00:00")

  
y = 436.6431 * sin_d((41.378 + 10))/sin_d(41.378)
y

stundenwinkel = 25.89
azimuth_angle_modul = 0
sonnenhoehe = 61.27
azimuth = 57.67

q = (cos_d(sonnenhoehe) * cos_d(azimuth - azimuth_angle_modul) * sin_d(tilt_angle_modul)) + (sin_d(sonnenhoehe) * cos_d(tilt_angle_modul))
q
