####### Sonnenstandsdiagramm ######
# Mithilfe dieses Diagramms wird der Sonnenstand und die Sonnenhöhe erfasst. Mit der Sonnenhöhe kann der Einfalsswinkel der direktstrahlung der Sonne auf ein Photovoltaikmodul berechnet werden.

sun_raw <- read.delim(file = "C:/Users/sascha/Nextcloud/17_solar_dashbord/solar-dashbord-business/Sonnenstand/sun.csv", sep = ";")
sun <- sun_raw %>% 
  mutate(date = as.POSIXct(date, format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(woz = as.POSIXct(date, format="%Y-%m-%d %H:%M:%S"))
#Variabeln
#Ort
  #Münster
#Breitengrad
  #52*
#Längengrad
  #7.62

#Werte
#0,017453 ist zur Umrechnung 

#Deklination
  #Formel: 23,45*COS(0,017453*360*(daynumber+10)/365)

sun <- sun %>%
  mutate(dec = 23.45 * cos(0.017453 * 360 * (daynumber + 10) / 365))

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
    15*(hour(date)+minute(date)/60-(15-52)/15-12+zeitgl/60)
  )
#sin(Sonnenhöhe)
  #SIN(0,017453*Latitude)*SIN(0,017453*Deklination)+COS(0,017453*Latitude)*COS(0,017453*Deklination)*COS(0,017453*Stundenwinkel)
#=SIN(0,017453*$B$3)*SIN(0,017453*H2)+COS(0,017453*$B$3)*COS(0,017453*H2)*COS(0,017453*J2)
sun <- sun %>%
  mutate(
    sin_sonnenhoehe = ((sin(0.017453 * 52) * sin(0.017453 * dec)) + (cos(0.017453 * 52) * cos(0.017453 * dec) * cos(0.017453 * stundenwinkel)))
  )

#Sonnenhöhe
#ARCSIN(sin(Sonnenhöhe))/0,017453
sun <- sun %>%
  mutate(sonnenhoehe = (asin(sin_sonnenhoehe) / 0.017453) 
  )
#cos(Azimut)
#Azimut

