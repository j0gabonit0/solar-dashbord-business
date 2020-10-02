#Provis

profvis({ runApp('/Users/sascha/solar-dashbord-business') })

# Errorsuche
latitude = 52.51
longitude = 13.41
azimuth_angle_modul = 180
tilt_angle_modul = 10
efficency = 0.2
m2 = 500

error_dt <- sedn_slpc %>% 
  filter(country_nuts2 == "DE11") %>%
  mutate(aesi =  + 75 * sin(0.017214206 * (daynumber - 275))) %>% 
  mutate(optical_depth = 0.174 + 0.035 * sin(0.017214206 * (daynumber - 100))) %>%
  mutate(dec = -23.45 * cos(0.017453 * 360 * (daynumber + 10) / 365)) %>%
  mutate(zeitgl = 60 * (-0.171 * sin(0.0337 * daynumber + 0.465) - 0.1299 * sin(0.01787 * daynumber  - 0.168) )) %>%
  mutate(stundenwinkel = 15 * (hour(utc_timestamp) + 
                                 (minute(utc_timestamp) /60) -
                                 ((15 - longitude) / 15) - 
                                 12 + 
                                 (zeitgl / 60))) %>%
  mutate(sin_sonnenhoehe = ((sin_d(latitude) * sin_d(dec)) + (
    cos_d(latitude) * cos_d(dec) * cos_d(stundenwinkel)))) %>%
  mutate(sonnenhoehe = (asin_d(sin_sonnenhoehe))) %>%
  mutate(cos_azimut = -((sin_d(latitude) * sin_sonnenhoehe) - (sin_d(dec))) / 
           (cos_d(latitude) * sin_d(acos_d(sin_sonnenhoehe)))) %>%
  mutate(azimut = ifelse((hour(utc_timestamp) + (minute(utc_timestamp) / 60)) <= 12 + ((15 - longitude) / 15) - (zeitgl / 60) ,
                         acos_d(cos_azimut) ,
                         360 - (acos_d(cos_azimut)))) %>%
  mutate(zenith_angle = 90 - sonnenhoehe) %>%
  mutate(air_mass_ratio = abs (1 / sin(2 * pi / 360 * sonnenhoehe))) %>%
  mutate(c_beam_radiation = ifelse(sonnenhoehe > 0, 
                                   aesi * exp(-1 *optical_depth * air_mass_ratio),
                                   0)) %>%
  mutate(ratio_c_beam_solar_watt = c_beam_radiation / aesi) %>%
  mutate(angle_of_incidance = acos_d(
    cos_d(sonnenhoehe) * cos_d(azimut - azimuth_angle_modul) * sin_d(tilt_angle_modul)) + 
      (sin_d(sonnenhoehe) * cos_d(tilt_angle_modul))) %>%
  mutate(IAM = ifelse(angle_of_incidance <= 90,
                      1 + (-0.0019386 * angle_of_incidance) + (0.00025854 * ((angle_of_incidance) ^2)) +
                        -0.000011229 * ((angle_of_incidance) ^ 3) + 0.00000019962 * ((angle_of_incidance) ^ 4) +
                        -0.0000000012818 * ((angle_of_incidance) ^5),
                      0)) %>%  # ASHRAHE MODEL
  mutate(direct_radiation_pv = ifelse(sonnenhoehe > 0, (
    radiation_direct_horizontal / 1000 * (sin_d(sonnenhoehe + 10) / sin_d(sonnenhoehe))) * IAM,
    0)) %>%
  mutate(diffuse_radiation_pv = radiation_diffuse_horizontal / 1000 * 1 /2 * (1 + cos_d(sonnenhoehe))) %>%
  mutate(reflective_radiation_pv = (direct_radiation_pv + diffuse_radiation_pv) * 0.5 * (1 - cos_d(sonnenhoehe)) * 0.2) %>%
  mutate(solar_watt = ((
    direct_radiation_pv + diffuse_radiation_pv + reflective_radiation_pv) *
      (1 + ((-0.5 * temperature + 12.5) / 100))) * 
      efficency * 
      module_reduce * 
      anti_reflection_modul * 
      wechselrichter_wigrad * 
      m2) %>% 
  mutate(day = utc_timestamp %>% as.character() %>% substr(6, 19)) %>%
  mutate(swm2 = solar_watt) %>%
  mutate(consum = ifelse(swm2 < consumw1 , swm2, ifelse(swm2 > consumw1, consumw1 , 0))) %>%
  mutate(sale = ifelse(swm2 > consumw1, swm2 - consumw1, 0))

###################




