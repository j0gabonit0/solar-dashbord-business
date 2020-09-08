
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
        ymd_hm('2019-12-31 23:00'),
        by = 'hour'
      )) %>%
      mutate(date_full = as.POSIXct(date_full, format = "%Y-%m-%d %H:%M:%S")) %>%
      group_by(date = floor_date(date_full, unit = "hour")) %>%
      summarise(kwh = sum(kwh)) %>%
      mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(u1 = date %>% as.character() %>% substr(6, 19)) %>%
      right_join(sedn_slpc, by = "u1")  %>%
      select(-date, -consumw1) %>%
      rename(consumw1 = kwh)
  })
  
  

  # Die Daten werden nach einer Stadt und dem Zeitraum gefiltert.  
  
  filtering <- reactive({
    

      nuts3 <- nuts_123_germany %>% 
        filter(place == input$selected_country) %>% 
        summarise(city = first(nuts2)) %>% 
        mutate(city = city %>% as.character()) %>% 
        summarise(city = first(city))  
      nuts2 = nuts3[1, "city"]
       newcsv() %>% 
      filter(country_nuts2 == nuts2) %>%
      filter(
        utc_timestamp >= paste0(startyear, "-01-01 00:00:00"),
        utc_timestamp <= paste0(endyear, "-12-31 24:00:00")
        ) %>% 
        mutate(aesi =  + 75 * sin(0.017214206 * (daynumber - 275))) %>% 
      mutate(optical_depth = 0.174 + 0.035 * sin(0.017214206 * (daynumber - 100))) %>%
      mutate(dec = -23.45 * cos(0.017453 * 360 * (daynumber + 10) / 365)) %>%
      mutate(zeitgl = 60 * (-0.171 * sin(0.0337 * daynumber + 0.465) - 0.1299 * sin(0.01787 * daynumber  - 0.168) )) %>%
      mutate(stundenwinkel = 15 * (hour(utc_timestamp) + 
                                  (minute(utc_timestamp) /60) -
                                  ((15 - input$longitude) / 15) - 
                                  12 + 
                                  (zeitgl / 60))) %>%
      mutate(sin_sonnenhoehe = ((sin_d(input$latitude) * sin_d(dec)) + (
                               cos_d(input$latitude) * cos_d(dec) * cos_d(stundenwinkel)))) %>%
      mutate(sonnenhoehe = (asin_d(sin_sonnenhoehe))) %>%
      mutate(cos_azimut = -((sin_d(input$latitude) * sin_sonnenhoehe) - (sin_d(dec))) / 
                            (cos_d(input$latitude) * sin_d(acos_d(sin_sonnenhoehe)))) %>%
      mutate(azimut = ifelse((hour(utc_timestamp) + (minute(utc_timestamp) / 60)) <= 12 + ((15 - input$longitude) / 15) - (zeitgl / 60) ,
                             acos_d(cos_azimut) ,
                             360 - (acos_d(cos_azimut)))) %>%
      mutate(zenith_angle = 90 - sonnenhoehe) %>%
      mutate(air_mass_ratio = abs (1 / sin(2 * pi / 360 * sonnenhoehe))) %>%
      mutate(c_beam_radiation = ifelse(sonnenhoehe > 0, 
                                       aesi * exp(-1 *optical_depth * air_mass_ratio),
                                       0)) %>%
      mutate(ratio_c_beam_solar_watt = c_beam_radiation / aesi) %>%
      mutate(angle_of_incidance = acos_d(
        cos_d(sonnenhoehe) * cos_d(azimut - input$azimuth_angle_modul) * sin_d(input$tilt_angle_modul)) + 
        (sin_d(sonnenhoehe) * cos_d(input$tilt_angle_modul))) %>%
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
          input$efficency * module_reduce * anti_reflection_modul * wechselrichter_wigrad * input$m2)
     
    
      })
  
  

    kwhyield <- reactive({
    filtering() %>%
      summarise(
        ms = sum(solar_watt) / years,
        kwhm2 = sum(solar_watt) / input$m2 / years ,
        kwhkwp = sum(solar_watt) / input$m2  / years,
        consum_proof = (sum(consumw1, na.rm = TRUE) / years)
      )
  })
  
  
  #Erloesfunktion berechnet folgende Ergebnisse
  # 1 Eigenverbrauch kWh
  # 2 Netzeinspeisung kWh
  # 3 Eigenverbrauch * Strompreis - EEG-Umlage
  # 4 Netzeinspeisung * EEG-Umlage
    
  #Grundlegende Berechnungen zu den Einnahmen und Ausgaben der Solaranlage
  
  erloes <- reactive({
    filtering() %>%
      mutate(swm2 = solar_watt) %>%
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
    erloes <- erloes()
    valueBox("Einsparung € p.a.", prettyNum(erloes$ev), color = "green")
  })
  
  output$es <- renderInfoBox({
    erloes <- erloes()
    valueBox("Vergütung EEG € p.a.", prettyNum(erloes$es), color = "green")
  })
  
  output$ekwh_percent <- renderInfoBox({
    erloes <- erloes()
    valueBox("Anteil Eigenverbrauch", prettyNum(erloes$ekwh_percent), color = "olive")
  })
  
  output$vkwh_percent <- renderInfoBox({
    erloes <- erloes()
    valueBox("Anteil Netzeinspeisung", prettyNum(erloes$vkwh_percent), color = "olive")
  })
  
  output$ge <- renderInfoBox({
    erloes <- erloes()
    valueBox("erloes € p.a.", prettyNum(erloes$ge),color = "green")
  })
  
  output[["ekwh"]] <- renderInfoBox({
    erloes <- erloes()
    valueBox("Eigenverbrauch kWh", prettyNum(erloes[["ekwh"]]))
  })
  
  output[["vkwh"]] <- renderInfoBox({
    erloes <- erloes()
    valueBox("Netzeinspeisung kWh", prettyNum(erloes[["vkwh"]]))
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
  
  #https://stackoverflow.com/questions/56530842/open-pie-chart-donut-chart-in-r-using-plotly-with-count-and-percentage
  
  #GaugeChart
  
  output$fig <- renderPlotly({ 
    data <- filtering()
    data %>% 
    mutate(swm2 = solar_watt) %>%
      mutate(e1 = ifelse(swm2 <= consumw1, swm2, consumw1)) %>%
      mutate(v1 = ifelse(swm2 >= consumw1, swm2 - consumw1, 0)) %>%
      summarise(
        ekwh = (sum(e1, na.rm = TRUE) / years),
        vkwh = (sum(v1, na.rm = TRUE) /years))
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = (ekwh[1] / (ekwh[1] + ~vkwh[1]) * 100),

        title = list(text = "Eigenverbrauch in %"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 5),
        gauge = list(
          axis =list(range = list(NULL, 100)),
          steps = list(
            list(range = c(0, 250), color = "gray"),
            list(range = c(250, 400), color = "blue")),
          threshold = list(
            line = list(color = "blue", width = 2),
            thickness = 0.8,
            value = 10)),
        margin = list(l=20,r=30))
    
  })
  
  
  # BarChart
  
  output$lastprofil_bar_chart <- renderPlot({
    data <- filtering()
    data %>%
      mutate(day = utc_timestamp %>% as.character() %>% substr(6, 19)) %>%
      mutate(month = month(utc_timestamp)) %>%
      group_by(month) %>% 
      summarise(consum_month = sum(consumw1), 
                production_month = sum(solar_watt)) %>% 
    ggplot(aes(x = month)) +
      geom_bar(aes(y = consum_month, colour = "Stromverbrauch in kWh"), stat = "identity", position="dodge") +
      geom_bar(aes(y = production_month, colour = "Stromproduktion in kWh"), stat = "identity",position="dodge")+
      theme(
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank()
      )
  })
  
  
  #Tortendiagramm
  output$pie_rent <- renderPlotly({  
    data <- filtering()
    data %>% 
    mutate(swm2 = radiation_direct_horizontal) %>%
    mutate(e1 = ifelse(swm2 <= consumw1, swm2, consumw1)) %>%
    mutate(v1 = ifelse(swm2 >= consumw1, swm2 - consumw1, 0)) %>%
    summarise(
      ekwh = (sum(e1, na.rm = TRUE) / years),
      vkwh = (sum(v1, na.rm = TRUE) / years),
      z = 1) %>% 
    pivot_longer(-z, names_to = "name", values_to = "values") %>% 
    plot_ly(labels = ~name, values = ~values) %>%
    add_pie(hole = 0.95) %>%
    layout(title = "Eigenverbrauch/Netzeinspeisung",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
           paper_bgcolor='transparent')
  })

  
  
  
  output$bar_chart <- renderPlot({
    data <- filtering()
    data %>%
      mutate(day = utc_timestamp %>% as.character() %>% substr(6, 19)) %>%
      mutate(swm2 = solar_watt) %>%
      mutate(e1 = ifelse(swm2 < consumw1 , swm2, ifelse(swm2 > consumw1, consumw1 , 0))) %>%
      mutate(v1 = ifelse(swm2 > consumw1, swm2 - consumw1, 0)) %>%
      group_by(day) %>%
      summarise(e = mean(e1), v = mean(v1)) %>%
      mutate(date = as.POSIXct(paste0("2020-", day), format = c("%Y-%m-%d %H:%M:%S"))) %>%
      mutate(month = month(date)) %>%
      group_by(month) %>%
      summarise(em = floor(sum(e, na.rm = TRUE)), vm = floor((sum(v, na.rm = TRUE)))) %>%
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
      mutate(swm2 = solar_watt) %>%
      mutate(e1 = ifelse(swm2 < consumw1 , swm2, ifelse(swm2 > consumw1, consumw1 , 0))) %>%
      mutate(v1 = ifelse(swm2 > consumw1, swm2 - consumw1, 0)) %>%
      group_by(day) %>%
      summarise(
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
      mutate(swm2 = solar_watt) %>%
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
      summarise(
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
    

  })

  
}
