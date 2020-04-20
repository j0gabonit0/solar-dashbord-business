# Solar Rendite Rechner
# Der Rendite Rechner errechnet die voraussichtlichen Gewinne einer PV-Anlage basierend auf dem Standort, dem Lastgang und der Leistung der PV-Anlage.
# Annahmen für die Berechnung - Diese sind konservativ gehalten, sodass voraussichtlich die untere Grenze der erwarteten Gewinne errechnet wird.

# Sicherheitsfaktor 95% der voraussichtlichen Leistung - Dies wird pauschal angesetzt.
sf = 0.95

# Leistungsabnahme des Moduls über 20 Jahre durchschnittlich 5% = 20 * 5% = 10%. Wenn die Abnahme über 20 Jahre 10 % ist, dann kann man den Startwert der Module * 95% rechnen. - Dies entspricht den Erfahrungswerten aus den Modulangaben der Hersteller
module_reduce = 0.90

# Quadratmeter für 1 kWp
m2kwp = 5

# Funktionen

function(input, output) {
 
  filtering <- reactive({
    startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
    endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()
    years <- endyear - startyear + 1
    sedn_slpc %>%
      filter(Stadt == input$selected_country) %>%
      filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00"))
  })
   
   kwhyield <-reactive({
     startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     years <- endyear - startyear + 1
     sedn_slpc %>%
       filter(Stadt == input$selected_country) %>%
       filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00")) %>% 
       summarise(
        ms = sum(solar_watt) / years * input$efficency * input$m2 * sf * module_reduce, 
        kwhm2 = sum(solar_watt) / years * input$efficency * sf * module_reduce,
        kwhkwp = sum(solar_watt) / years * input$efficency * sf * m2kwp * module_reduce)
   })
   
   
   #Erlösfunktion berechnet folgende Ergebnisse
   # 1 Eigenverbrauch kWh
   # 2 Netzeinspeisung kWh
   # 3 Eigenverbrauch * Strompreis - EEG-Umlage
   # 4 Netzeinspeisung * EEG-Umlage 
   
   erlös <- reactive({
     startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     years <- endyear - startyear + 1
     sedn_slpc %>%
       filter(Stadt == input$selected_country) %>%
       filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00")) %>% 
       mutate(swm2 = solar_watt * input$m2 * input$efficency * sf * module_reduce) %>%
       mutate(e1 = ifelse(swm2 < consumw1, swm2, consumw1)) %>%
       mutate(v1 = ifelse(swm2 > consumw1, swm2 - consumw1, 0)) %>%
       summarise(ekwh = (sum(e1, na.rm = TRUE) / years),
                 ev = (sum(e1, na.rm = TRUE) / years * input$cost), 
                 ekwh_percent = ((sum(e1, na.rm = TRUE) / years) / ((sum(e1, na.rm = TRUE) / years) + (sum(v1, na.rm = TRUE) / years))),
                 vkwh = (sum(v1, na.rm = TRUE) / years),
                 vkwh_percent = ((sum(v1, na.rm = TRUE) / years) / ((sum(e1, na.rm = TRUE) / years) + (sum(v1, na.rm = TRUE) / years))),
                 es = (sum(v1, na.rm = TRUE) / years * input$price), 
                 ge = ev + es)
                 
     })
   
   #Investfunktion berechnet folgende Ergebnisse
   # 1 Annuität
   # 2 Laufende Kosten
   invest <- reactive({
     startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     years <- endyear - startyear + 1
     annu <- (input[["invest"]] * input[["m2"]]/ m2kwp) * (1 - input[["ek"]]) * ((((1+input[["zi"]])^years)*input[["zi"]])/(((1+input[["zi"]])^years)-1))
     sedn_slpc %>%
       summarise(cy = (input[["m2"]]/ m2kwp * input[["lk"]]), zins = annu, gk = zins + cy)
       
  })
   

   
# Output
   
   output$kwhm2 <- renderInfoBox({
     kwhyield <- kwhyield()
     valueBox("Erzeugte kWh pro m2",prettyNum(kwhyield$kwhm2))
   })
   
   output$kwhkwp <- renderInfoBox({
     kwhyield <- kwhyield()
     valueBox("Erzeugte kWh pro kWp",prettyNum(kwhyield$kwhkwp))
   })
  
  output$ms <- renderInfoBox({
    kwhyield <- kwhyield()
    valueBox("Erzeugte kWh p.a.",prettyNum(kwhyield$ms))
  })
      
  output$ev <- renderInfoBox({
    erlös <- erlös()
    valueBox("Einsparung € p.a.",prettyNum(erlös$ev))
  })
  
  output$es <- renderInfoBox({
    erlös <- erlös()
    valueBox("Vergütung EEG € p.a.",prettyNum(erlös$es))
  })  
  
  output$ekwh_percent <- renderInfoBox({
    erlös <- erlös()
    valueBox("Anteil Eigenverbrauch",prettyNum(erlös$ekwh_percent))
  })  
  
  output$vkwh_percent <- renderInfoBox({
    erlös <- erlös()
    valueBox("Anteil Netzeinspeisung",prettyNum(erlös$vkwh_percent))
  })  
  
  output$ge <- renderInfoBox({
    erlös <- erlös()
    valueBox("Erlös € p.a.",prettyNum(erlös$ge))
  })  
  
  output[["ekwh"]] <- renderInfoBox({
    erlös <- erlös()
    valueBox("Eigenverbrauch kWh",prettyNum(erlös[["ekwh"]]))
  })  
  
  output[["vkwh"]] <- renderInfoBox({
    erlös <- erlös()
    valueBox("Netzeinspeisung kWh",prettyNum(erlös[["vkwh"]]))
  })
  
  output[["cy"]] <- renderInfoBox({
    invest <- invest()
    valueBox("Laufende Kosten € p.a.",prettyNum(invest[["cy"]]))
  })  
  
  output[["result"]] <- renderInfoBox({
    invest <- invest()
    valueBox("Annuitaet p.a.",prettyNum(invest[["zins"]]))
  })
  
  output[["gk"]] <- renderInfoBox({
    invest <- invest()
    valueBox("Gesamtkosten € p.a.",prettyNum(invest[["gk"]]))
  })

# Plot
  
  output$radiation_chart <- renderPlot({
    data <- filtering()
    data %>%
      mutate(day = utc_timestamp %>% as.character() %>% substr(6,19)) %>%
      mutate(swm2 = solar_watt * input$efficency * input$m2) %>%
      mutate(e1 = ifelse(swm2 < consumw1 , swm2, ifelse(swm2 > consumw1, consumw1 , 0))) %>%
      mutate(v1 = ifelse(swm2 > consumw1, swm2 - consumw1, 0)) %>%
      group_by(day) %>%
      summarize(avg = mean(solar_watt, na.rm = TRUE) * input$m2, e = mean(e1), v = mean(v1), stdv1 = sd(v1, na.rm = TRUE) , slp = mean(consumw1)) %>%
      mutate(date = as.POSIXct(paste0("2019-", day), format = c("%Y-%m-%d %H:%M:%S"))) %>%
      ggplot() + 
      aes(x = date) +
      geom_smooth(aes(y = slp, colour = "Standardlastrofil  p.a")) +
      geom_smooth(aes(y = e, colour = "Eigenverbrauch")) +
      geom_smooth(aes(y = v, colour = "Einspeisung ins Netz  p.a")) +
      xlab("") +
      ylab("")
  })
}