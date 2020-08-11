####### Sonnenstandsdiagramm ######
# Mithilfe dieses Diagramms wird der Sonnenstand und die Sonnenh?he erfasst. Mit der Sonnenh?he kann der Einfalsswinkel der direktstrahlung der Sonne auf ein Photovoltaikmodul berechnet werden.
#/Users/sascha/NC/17_solar_dashbord/solar-dashbord-business/Sonnenstand
#C:/Users/sascha/Nextcloud/17_solar_dashbord/solar-dashbord-business/Sonnenstand/sun.csv
#C:/Users/corvi/Nextcloud-Stiftung/17_solar_dashbord/solar-dashbord-business/Kalkulationsgrundlage


#Direct radiation is defined as radiation that has not experienced scattering in the atmosphere, so that it is directionally fixed, coming from the disc of the Sun.


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
latitude = 52.51
longitude = 13.41
tilt_angle_modul = 10
azimuth_angle_modul = 180
m2 = 500
module_reduce = 0.9
efficency = 0.2


t = cos_d(100 - azimuth_angle_modul)
t

#Apparent Extraterrestrial Solar Insolation 
# Verwendet Formel aus Excel-Sheet Engineering New Mexico Resource Network 
# Verluste der Extraterrestrischen Starhlung von durchschnittlich 1324,67 W/m2
# 1160+75*SIN(0.017214206 * (julian_day - 275)))

#Optical depth
# Verwendet Formel aus Excel-Sheet Engineering New Mexico Resource Network 
# Verluste der Strahlung durch die Dicker der Atmosphere
# 0.174 + 0.035 * SIN(0.017214206 * (julian_day - 100)))
op =

#AirMassRatio
#The Air Mass is the path length which light takes through the atmosphere normalized to the shortest possible path length (that is, when the sun is directly overhead).
h = abs(1/sin(2 * pi/360 * 71.19))

Clear Sky Beam Radiation
# c_beam_radiation
=WENN(sonnenhoehe>0;aesi*EXP(-Optical Depth * AirMassRatio);0)
k = 1086.66 * exp(-0.208 * 1.056)
k
sedn_slpc <- read_delim(path_sedn_slpc, delim = ",")

x <- sedn_slpc %>% 
  filter(Stadt == "Berlin") %>%
  filter(
    utc_timestamp >= "1995-01-01 00:00:00",
    utc_timestamp <= "2015-12-31 24:00:00"
  ) %>% 
  mutate(aesi =  + 75 * sin(0.017214206 * (daynumber - 275))) %>% 
  mutate(optical_depth = 0.174 + 0.035 * sin(0.017214206 * (daynumber - 100))) %>% 
  mutate(dec = -23.45 * cos(0.017453 * 360 * (daynumber + 10) / 365)) %>% 
  mutate(zeitgl = 60 * (-0.171 * sin(0.0337 * daynumber + 0.465) - 0.1299 * sin(0.01787 * daynumber  - 0.168))) %>% 
  mutate(stundenwinkel = 15*(hour(utc_timestamp) + (minute(utc_timestamp)/60) - ((15 - longitude)/15)- 12 + (zeitgl / 60))) %>% 
  mutate(sin_sonnenhoehe = ((sin_d(latitude) * sin_d(dec)) + (cos_d(latitude) * cos_d(dec) * cos_d(stundenwinkel)))) %>% 
  mutate(sonnenhoehe = (asin_d(sin_sonnenhoehe))) %>% 
  mutate(cos_azimut = -((sin_d(latitude) * sin_sonnenhoehe) - (sin_d(dec))) / (cos_d(latitude) * sin_d(acos_d(sin_sonnenhoehe)))) %>% 
  mutate(azimut = ifelse((hour(utc_timestamp) + (minute(utc_timestamp) / 60)) <= 12 + ((15 - longitude) / 15) - (zeitgl / 60) , acos_d(cos_azimut) , 360 - (acos_d(cos_azimut)))) %>%  
  mutate(zenith_angle = 90 - sonnenhoehe) %>% 
  mutate(air_mass_ratio = abs (1 / sin(2 * pi / 360 * sonnenhoehe))) %>%
  mutate(c_beam_radiation = ifelse(sonnenhoehe > 0, aesi * exp(-1 *optical_depth * air_mass_ratio), 0 )) %>% 
  mutate(ratio_c_beam_solar_watt = c_beam_radiation / aesi) %>% 
  mutate(angle_of_incidance = acos_d(cos_d(sonnenhoehe) * cos_d(azimut - azimuth_angle_modul) * sin_d(tilt_angle_modul)) + (sin_d(sonnenhoehe) * cos_d(tilt_angle_modul))) %>% 
  mutate(IAM = ifelse(angle_of_incidance <= 90, 1 + (-0.0019386 * angle_of_incidance) + (0.00025854 * ((angle_of_incidance)^2)) + -0.000011229 * ((angle_of_incidance)^3) + 0.00000019962 * ((angle_of_incidance) ^ 4) + -0.0000000012818 * ((angle_of_incidance)^5),0)) %>%  # ASHRAHE MODEL 
  mutate(direct_radiation_pv = ifelse(sonnenhoehe > 0, (radiation_direct_horizontal/1000 * (sin_d(sonnenhoehe + 10) / sin_d(sonnenhoehe))) * IAM,0)) %>% 
  mutate(diffuse_radiation_pv = radiation_diffuse_horizontal/1000 * 1/2 * (1 + cos_d(sonnenhoehe))) %>% 
  mutate(reflective_radiation_pv = (direct_radiation_pv + diffuse_radiation_pv) * 0.5 * (1 - cos_d(sonnenhoehe)) * 0.2) %>% 
  mutate(solar_watt = ((direct_radiation_pv + diffuse_radiation_pv + reflective_radiation_pv) * (1+((-0.5 * temperature + 12.5) / 100 ))) * efficency * module_reduce) %>% 
 filter(
    utc_timestamp >="2014-07-15 02:00:00",
    utc_timestamp <= "2015-07-16 01:00:00"
    
  )%>% 
  summarise(kwh = sum(solar_watt), kwh_old = sum((radiation_direct_horizontal + radiation_diffuse_horizontal) / 1000))
  
  
32y = 436.6431 * sin_d((41.378 + 10))/sin_d(41.378)
y

stundenwinkel = 25.89
azimuth_angle_modul = 0
sonnenhoehe = 61.27
azimuth = 57.67

q = (cos_d(sonnenhoehe) * cos_d(azimuth - azimuth_angle_modul) * sin_d(tilt_angle_modul)) + (sin_d(sonnenhoehe) * cos_d(tilt_angle_modul))
q




IAM with ARC

c0 = 1
c1 = -1.9386 * 10 ^ -3
c2 = 2.5854 * 10 ^ -4
c3 = -1.1229 * 10  ^  -5
c4 = 1.9962 * 10 ^ -7
c5 = -1.2818 * 10 ^ -9
aoi = 90

# Incident Angle Modifier (IAM) Korrektur des Einfallswinkels auf ein PV-Modul. Je st?rker der Winkel ist desto h?her ist der Verlust der direkten Strahlung.

IAM = c0 + (c1 * aoi) + (c2 * ((aoi)^2)) + c3 * ((aoi)^3) + c4 * ((aoi)^4) + c5 * ((aoi)^5)
IAM = ifelse(aoi <=90, 1 + (-0.0019386 * aoi) + (0.00025854 * ((aoi)^2)) + -0.000011229 * ((aoi)^3) + 0.00000019962 * ((aoi) ^ 4) + -0.0000000012818 * ((aoi)^5),0)
IAM


