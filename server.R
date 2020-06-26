# Solar Rendite Rechner
# Der Rendite Rechner errechnet die voraussichtlichen Gewinne einer PV-Anlage basierend auf dem Standort, dem Lastgang und der Leistung der PV-Anlage.
# Die Solardaten habe ich von "https://data.open-power-system-data.org/weather_data/2019-04-09" bezogen. In dem Datenset sind stuendliche Werte fuer Solareinstrahlung,Temperatur nach europaeischen Laendern. Deutschland ist zusaetzlich in NUTS Zonen unterteilt.
# Das Dashbord berechnet bei Beginn eine Solaranlage mit 100kWp und einem Lastprofil von 140.000 kWh p.a. Dies ist fuer oeffentliche Gebaeude wie Schulen repraesentativ. Fuer Haushalte muss dies angepasst werden. Es gibt im Internet kostenlose Lastprofile fuer viele Anwendungsfaelle wie Landwirtschaft,Haushalt oder Gewerbe.
# Wenn ein eigenes Lastprofil vorliegt (was sehr leicht beim Stromanbieter eingefordert werden kann), kann man den Upload Button verwenden. Die Kalkulationen werden dann neu durchgefuehrt.
# In Zukunft sollen mehrere Lastprofile zur Auswahl stehen.
# Die Lastprofile dienen dazu, eine genauere Berechnung des Eigenverbrauchs durchzufuehren. Eure stuendlichen Verbraeuche werden mit den stuendlichen Einstrahlungswerten der PV-Anlage verrechnet. Somit koennt ihr auf stuendlicher Basis vorhersagen, wie viel Strom in den letzten 20 Jahren abgenommen worden waere.
#

# Funktionen

function(input, output, session) {
  
# Entweder werden die Default Daten verwendet oder als Datengrundlage dient das Upload CSV.   
  
  newcsv <- reactive({
    file1 <- input$file
    if (is.null(file1)) {
      return(sedn_slpc)
    }
    read.table(
      file = file1$datapath,
      sep = input$sep,
      header = input$header,
      stringsAsFactors = input$stringAsFactors
    )
    data <- read.csv(input$file$datapath)
    #sedn_slpc <- sedn_slpc %>%
     # mutate(day = utc_timestamp %>% as.character() %>% substr(5, 19))
    data %>%
      mutate(date_full = seq(
        ymd_hm('2019-01-01 00:00'),
        ymd_hm('2019-12-31 23:45'),
        by = '15 mins'
      )) %>%
      mutate(date_full = as.POSIXct(date_full, format = "%Y-%m-%d %H:%M:%S")) %>%
      group_by(date = floor_date(date_full, unit = "hour")) %>%
      summarize(kwh = sum(kwh)) %>%
      mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(day = date %>% as.character() %>% substr(5, 19)) %>%
      right_join(sedn_slpc, by = "day")  %>%
      select(-day, -date, -consumw1) %>%
      rename(consumw1 = kwh)
  })
  
  
  # Die Daten werden nach einer Stadt und dem Zeitraum gefiltert.  
  
  filtering <- reactive({
    startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1, 4) %>% as.numeric()
    endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1, 4) %>% as.numeric()
    #years = (as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()) - (as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()) + 1
    newcsv() %>%
      filter(Stadt == input$selected_country) %>%
      filter(
        utc_timestamp >= paste0(startyear, "-01-01 00:00:00"),
        utc_timestamp <= paste0(endyear, "-12-31 24:00:00")
      )
  })
  
  

    kwhyield <- reactive({
    filtering() %>%
      summarise(
        ms = sum(solar_watt) / years * input$efficency * input$m2 * sf * module_reduce,
        kwhm2 = sum(solar_watt) / years * input$efficency * sf * module_reduce,
        kwhkwp = sum(solar_watt) / years * input$efficency * sf * m2kwp * module_reduce,
        consum_proof = (sum(consumw1, na.rm = TRUE) / years)
      )
  })
  
  
  #Erloesfunktion berechnet folgende Ergebnisse
  # 1 Eigenverbrauch kWh
  # 2 Netzeinspeisung kWh
  # 3 Eigenverbrauch * Strompreis - EEG-Umlage
  # 4 Netzeinspeisung * EEG-Umlage
    
  #Grundlegende Berechnungen zu den Einnahmen und Ausgaben der Solaranlage
  
  erlös <- reactive({
    filtering() %>%
      mutate(swm2 = solar_watt * input$m2 * input$efficency * sf * module_reduce) %>%
      mutate(e1 = ifelse(swm2 <= consumw1, swm2, consumw1)) %>%
      mutate(v1 = ifelse(swm2 >= consumw1, swm2 - consumw1, 0)) %>%
      summarise(
        ekwh = (sum(e1, na.rm = TRUE) / years),
        ev = (sum(e1, na.rm = TRUE) / years * input$cost),
        ekwh_percent = ((sum(e1, na.rm = TRUE) / years) / ((
          sum(e1, na.rm = TRUE) / years
        ) + (
          sum(v1, na.rm = TRUE) / years
        ))),
        vkwh = (sum(v1, na.rm = TRUE) / years),
        vkwh_percent = ((sum(v1, na.rm = TRUE) / years) / ((
          sum(e1, na.rm = TRUE) / years
        ) + (
          sum(v1, na.rm = TRUE) / years
        ))),
        es = (sum(v1, na.rm = TRUE) / years * input$price),
        ge = ev + es
      )
    
  })
  
  #Investfunktion berechnet folgende Ergebnisse
  # 1 Annuität
  # 2 Laufende Kosten
  
  invest <- reactive({
    annu <-
      (input[["invest"]] * input[["m2"]] / m2kwp) * (1 - input[["ek"]]) * ((((1 + input[["zi"]]) ^ years) * input[["zi"]]) / (((1 + input[["zi"]]) ^ years) -1))
    sedn_slpc %>%
      summarise(
        cy = (input[["m2"]] / m2kwp * input[["lk"]]),
        zins = annu,
        gk = zins + cy
      )
    
  })

  output$files <- renderTable(input$file)
  
  
  ####  Output  ####
  
  output$kwhm2 <- renderInfoBox({
    kwhyield <- kwhyield()
    valueBox("Erzeugte kWh pro m2", prettyNum(kwhyield$kwhm2), color = "yellow")
  })
  
  output$kwhkwp <- renderInfoBox({
    kwhyield <- kwhyield()
    valueBox("Erzeugte kWh pro kWp", prettyNum(kwhyield$kwhkwp), color = "yellow")
  })
  
  output$consum_proof <- renderInfoBox({
    kwhyield <- kwhyield()
    valueBox("Verbrauchte kWh p.a.", prettyNum(kwhyield$consum_proof), color = "yellow")
  })
  
  output$ms <- renderInfoBox({
    kwhyield <- kwhyield()
    valueBox("Erzeugte kWh p.a.", prettyNum(kwhyield$ms))
  })
  
  output$ev <- renderInfoBox({
    erlös <- erlös()
    valueBox("Einsparung € p.a.", prettyNum(erlös$ev), color = "green")
  })
  
  output$es <- renderInfoBox({
    erlös <- erlös()
    valueBox("Vergütung EEG € p.a.", prettyNum(erlös$es), color = "green")
  })
  
  output$ekwh_percent <- renderInfoBox({
    erlös <- erlös()
    valueBox("Anteil Eigenverbrauch", prettyNum(erlös$ekwh_percent), color = "olive")
  })
  
  output$vkwh_percent <- renderInfoBox({
    erlös <- erlös()
    valueBox("Anteil Netzeinspeisung", prettyNum(erlös$vkwh_percent), color = "olive")
  })
  
  output$ge <- renderInfoBox({
    erlös <- erlös()
    valueBox("Erlös € p.a.", prettyNum(erlös$ge),color = "green")
  })
  
  output[["ekwh"]] <- renderInfoBox({
    erlös <- erlös()
    valueBox("Eigenverbrauch kWh", prettyNum(erlös[["ekwh"]]))
  })
  
  output[["vkwh"]] <- renderInfoBox({
    erlös <- erlös()
    valueBox("Netzeinspeisung kWh", prettyNum(erlös[["vkwh"]]))
  })
  
  output[["cy"]] <- renderInfoBox({
    invest <- invest()
    valueBox("Laufende Kosten € p.a.", prettyNum(invest[["cy"]]), color = "red")
  })
  
  output[["result"]] <- renderInfoBox({
    invest <- invest()
    valueBox("Annuitaet p.a.", prettyNum(invest[["zins"]]), color = "red")
  })
  
  output[["gk"]] <- renderInfoBox({
    invest <- invest()
    valueBox("Gesamtkosten € p.a.", prettyNum(invest[["gk"]]), color = "red")
    
  })
  
  #### Plot ####
  
  # Plot von Eigenverbrauch und Netzeinspeisung in kWh
  
  output$bar_chart <- renderPlot({
    data <- filtering()
    data %>%
      mutate(day = utc_timestamp %>% as.character() %>% substr(6, 19)) %>%
      mutate(swm2 = solar_watt * input$efficency * input$m2 * sf * module_reduce) %>%
      mutate(e1 = ifelse(swm2 < consumw1 , swm2, ifelse(swm2 > consumw1, consumw1 , 0))) %>%
      mutate(v1 = ifelse(swm2 > consumw1, swm2 - consumw1, 0)) %>%
      group_by(day) %>%
      summarise(e = mean(e1), v = mean(v1)) %>%
      mutate(date = as.POSIXct(paste0("2020-", day), format = c("%Y-%m-%d %H:%M:%S"))) %>%
      mutate(month = month(date)) %>%
      group_by(month) %>%
      summarize(em = floor(sum(e, na.rm = TRUE)), vm = floor((sum(v, na.rm = TRUE)))) %>%
      ggplot(aes(x = month)) +
      geom_line(aes(y = em, colour = "Eigenverbrauch in kWh"), stat = "identity") +
      geom_line(aes(y = vm, colour = "Netzeinspeisung in kWh"), stat = "identity") +
      geom_point(aes(y = em), stat = "identity") +
      geom_point(aes(y = vm), stat = "identity") +
      geom_text(mapping = aes(x = month, y = em, label = em)) +
      geom_text(mapping = aes(x = month, y = vm, label = vm)) +
      xlab("") +
      ylab("") +
      scale_colour_manual(name = "Legende",
                          values = c("blue", "green", "yellow")) +
      theme(
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank()
      )
    
    
  }, bg = "transparent")
  
  
  #Plot von Netzeinspeisung Eigenverbrauch und Lastgang in kWh
  
  output$radiation_chart <- renderPlot({
    data <- filtering()
    data %>%
      mutate(day = utc_timestamp %>% as.character() %>% substr(6, 19)) %>%
      mutate(swm2 = solar_watt * input$efficency * input$m2 * sf * module_reduce) %>%
      mutate(e1 = ifelse(swm2 < consumw1 , swm2, ifelse(swm2 > consumw1, consumw1 , 0))) %>%
      mutate(v1 = ifelse(swm2 > consumw1, swm2 - consumw1, 0)) %>%
      group_by(day) %>%
      summarize(
        avg = mean(solar_watt, na.rm = TRUE) * input$m2,
        e = mean(e1),
        v = mean(v1),
        stdv1 = sd(v1, na.rm = TRUE) ,
        slp = mean(consumw1)
      ) %>%
      mutate(date = as.POSIXct(paste0("2020-", day), format = c("%Y-%m-%d %H:%M:%S"))) %>%
      ggplot() +
      aes(x = date) +
      geom_smooth(aes(y = slp, colour = "Standardlastprofil")) +
      geom_smooth(aes(y = e, colour = "Eigenverbrauch")) +
      geom_smooth(aes(y = v, colour = "Netzeinspeisung")) +
      xlab("") +
      ylab("") +
      scale_colour_manual(name = "legend",
                          values = c("blue", "green", "yellow")) +
      theme(
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA)
      )
  }, bg = "transparent")
  
  #Plot einer Tabelle mit Eigenverbrauch und Netzeinspeisung pro Monat
  
  output$dataTable <- renderDT({
    data <- filtering()
    data %>%
      mutate(day = utc_timestamp %>% as.character() %>% substr(6, 19)) %>%
      mutate(swm2 = solar_watt * input$efficency * input$m2 * sf * module_reduce) %>%
      mutate(consum = ifelse(swm2 < consumw1 , swm2, ifelse(swm2 > consumw1, consumw1 , 0))) %>%
      mutate(sale = ifelse(swm2 > consumw1, swm2 - consumw1, 0)) %>%
      group_by(day) %>%
      summarise(
        consum_mean = mean(consum, na.rm = TRUE),
        sale_mean = mean(sale, na.rm = TRUE),
        swm2 = mean(swm2, na.rm = TRUE),
        consumw1 = mean(consumw1, na.rm = TRUE)
      ) %>%
      mutate(date = as.POSIXct(paste0("2020-", day), format = c("%Y-%m-%d %H:%M:%S"))) %>%
      mutate(month = month(date)) %>%
      group_by(month) %>%
      summarize(
        consum_month = floor(sum(consum_mean, na.rm = TRUE)),
        sale_month = floor(sum(sale_mean, na.rm = TRUE)),
        Erzeugung_Anlage_kwh = floor(sum(swm2, na.rm = TRUE)),
        Strombedarf_kwh = floor(sum(consumw1, na.rm = TRUE))
      ) %>%
      mutate(consum_Anteil = 100 * (consum_month / (
        sum(consum_month, na.rm = TRUE) + sum(sale_month, na.rm = TRUE)
      ))) %>%
      mutate(Sale_Anteil = 100 * (sale_month / (
        sum(sale_month, na.rm = TRUE) + sum(consum_month, na.rm = TRUE)
      )))
    
 #Plot der Daten aus der CSV Tabelle. Nur zur Kontrolle
    
  })
  
  output$dtcsv <- renderDT({
    file1 <- input$file
    if (is.null(file1)) {
      return()
    }
    read.table(
      file = file1$datapath,
      sep = input$sep,
      header = input$header,
      stringsAsFactors = input$stringAsFactors
    )
    data <- read.csv(input$file$datapath)
    sedn_slpc <- sedn_slpc %>%
      mutate(day = utc_timestamp %>% as.character() %>% substr(5, 19))
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
      right_join(sedn_slpc, by = "day")  %>%
      select(-day, -date, -consumw1) %>%
      rename(consumw1 = kwh)
    
    
  })
  
}
