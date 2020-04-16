function(input, output) {
 
  filtering <- reactive({
    startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
    endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()
    years <- endyear - startyear + 1
    sedn_slpc %>%
      filter(Stadt == input$selected_country) %>%
      filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00"))
      
  })  

   m2 <- reactive({
     startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     years <- endyear - startyear + 1
     sedn_slpc %>%
       filter(Stadt == input$selected_country) %>%
       filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00")) %>% 
       summarise(
       yieldm2 = sum(solar_watt) / years * input$efficency,
       m = input$kwhy/yieldm2
       )
  })
   
   ############# Ursprüngliche Version. Filtering scheint nicht korrekt übernommen zu werden
#m2 <- reactive({
 #    data <- filtering()
  #   data %>% summarise(
   #    yieldm2 = sum(solar_watt) / years * input$efficency /1000,
    #   m = input$kwhy/yieldm2
    # )
   #})
   #############
   
   kwhyield <-reactive({
     startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     years <- endyear - startyear + 1
     sedn_slpc %>%
       filter(Stadt == input$selected_country) %>%
       filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00")) %>% 
       summarise(
        ms = sum(solar_watt) / years * input$efficency * input$m2)    
   })
   
   erlös <- reactive({
     startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     years <- endyear - startyear + 1
     sedn_slpc %>%
       filter(Stadt == input$selected_country) %>%
       filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00")) %>% 
       mutate(swm2 = solar_watt * input$m2) %>%
       mutate(kwhd = kwh / 4000 *input$kwhy) %>%
       mutate(ec = swm2 - kwhd) %>%
       mutate(e1 = ifelse(swm2 < kwhd, swm2, ifelse(swm2 > kwhd, kwhd , 0))) %>%
       mutate(v1 = ifelse(swm2 > kwhd, swm2 - kwhd, 0)) %>%
       summarise(ev = sum(e1, na.rm = TRUE) / years * input$cost * input$efficency, es = sum(v1, na.rm = TRUE) / years * input$price * input$efficency, ge = ev + es)
   })
   
   invest <- reactive({
     startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     years <- endyear - startyear + 1
     annu <- (input[["invest"]] * input[["m2"]]/5) * (1 - input[["ek"]]) * ((((1+input[["zi"]])^years)*input[["zi"]])/(((1+input[["zi"]])^years)-1))
     sedn_slpc %>%
       summarise(cy = (input[["m2"]]/5 * input[["lk"]]), zins = annu, gk = zins + cy)
       
  })
   
   #zinsen <- reactive({
  #  input[["invest"]] * input[["ek"]] * input[["zi"]] * input[["m2"]]/5
  #     
 #})
   

   
   
  output$m <- renderInfoBox({
    m2 <- m2()
    valueBox("Benötigte m² ", prettyNum(m2$m))
  })
  
  output$yieldm2 <- renderInfoBox({
      m2 <- m2()
      valueBox("Erzeugte kWh/m² p.a", prettyNum(m2$yieldm2))
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
  
  output$ge <- renderInfoBox({
    erlös <- erlös()
    valueBox("Erlös € p.a.",prettyNum(erlös$ge))
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

  
  
  
# Normale Outputbox, falls das normale nicht klappt
  #output$result <- renderValueBox({
 #   valueBox(
    #  "Zinsen",
    #  invest$zinsen,
    #  )
  #})

  
  
  
  
  output$radiation_chart <- renderPlot({
    data <- filtering()
    data %>%
      mutate(day = utc_timestamp %>% as.character() %>% substr(6,19)) %>%
      mutate(swm2 = solar_watt * input$efficency * input$m2) %>%
      mutate(kwhd = kwh / 4000 *input$kwhy) %>% 
      mutate(ec = swm2 - kwhd) %>%
      mutate(e1 = ifelse(swm2 < kwhd, swm2, ifelse(swm2 > kwhd, kwhd , 0))) %>%
      mutate(v1 = ifelse(swm2 > kwhd, swm2 - kwhd, 0)) %>%
      group_by(day) %>%
      summarize(avg = mean(solar_watt, na.rm = TRUE) * input$m2, e = mean(e1), v = mean(v1), stdv1 = sd(v1, na.rm = TRUE) , std = sd(ec, na.rm = TRUE) / sqrt(n()), slp = mean(kwhd)) %>%
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