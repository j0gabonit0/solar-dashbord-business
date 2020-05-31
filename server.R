# Solar Rendite Rechner
# Der Rendite Rechner errechnet die voraussichtlichen Gewinne einer PV-Anlage basierend auf dem Standort, dem Lastgang und der Leistung der PV-Anlage.
# Annahmen für die Berechnung - Diese sind konservativ gehalten, sodass voraussichtlich die untere Grenze der erwarteten Gewinne errechnet wird.

# Sicherheitsfaktor 95% der voraussichtlichen Leistung - Dies wird pauschal angesetzt.
sf = 1/0.96

# Leistungsabnahme des Moduls über 20 Jahre durchschnittlich 5% = 20 * 5% = 10%. Wenn die Abnahme über 20 Jahre 10 % ist, dann kann man den Startwert der Module * 95% rechnen. - Dies entspricht den Erfahrungswerten aus den Modulangaben der Hersteller
module_reduce = 0.90

# Quadratmeter für 1 kWp
m2kwp = 5

#Eigenverbrauch  = consum
#Netzeinspeisung = sale



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
        kwhkwp = sum(solar_watt) / years * input$efficency * sf * m2kwp * module_reduce,
        consum_proof = sum(consumw1, na.rm = TRUE) / years)
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
   

     # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
     # file$datapath -> gives the path of the file
     data <- reactive({
       file1 <- input$file
       if(is.null(file1)){return()} 
       read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
       data<-read.csv(input$file$datapath)
       data<- data %>% 
         mutate(date = seq(from = as.Date("2019-01-01"), to = as.Date("2019-12-31"), by = 'day')) %>% 
         mutate(date_full = seq(ymd_hm('2019-01-01 00:00'),ymd_hm('2019-12-31 23:45'), by = '15 mins')) %>% 
         mutate(date_full = as.POSIXct(date_full, format="%Y-%m-%d %H:%M:%S")) %>% 
         group_by(date = floor_date(date_full, unit = "hour")) %>%
         summarize( kwh = sum(kwh)/4) %>% 
         mutate(date = as.POSIXct(date, format="%Y-%m-%d %H:%M:%S")) %>% 
         mutate(day = date %>% as.character() %>% substr(5,19)) %>% 
         right_join(data1$sedn_slpc,by = "day") %>% 
         select(-day,-date) %>% 
         rename(consumw1 = kwh) %>% 
         select(-country,-temperature,-global_radiation) 
       
     })
     
     data1 <- reactive({
       sedn_t <- sedn_slpc %>%
         mutate(day = utc_timestamp %>% as.character() %>% substr(5,19))
       
     })
     
     
     
     # this reactive output contains the summary of the dataset and display the summary in table format
     output$filedf <- renderTable({
       if(is.null(data())){return ()}
       input$file
     })
     
     # this reactive output contains the summary of the dataset and display the summary in table format
     output$sum <- renderTable({
       if(is.null(data())){return ()}
       summary(data())
       
     })
     
     # This reactive output contains the dataset and display the dataset in table format
     output$table <- renderTable({
       if(is.null(data())){return ()}
       data()
     })
     
     # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
     output$tb <- renderUI({
       if(is.null(data()))
         h5("Powered by", tags$img(src='RStudio-Ball.png', heigth=200, width=200))
       else
         tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", tableOutput("table")),tabPanel("Summary", tableOutput("sum")))
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
   
   output$consum_proof <- renderInfoBox({
     kwhyield <- kwhyield()
     valueBox("Verbrauchte kWh p.a.",prettyNum(kwhyield$consum_proof))
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
  
  output$bar_chart <- renderPlot({
    data <- filtering()
    data %>%
      mutate(day = utc_timestamp %>% as.character() %>% substr(6,19)) %>%
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
      geom_line(aes(y = em, colour = "Eigenverbrauch in kWh"),stat = "identity") +
      geom_line(aes(y = vm, colour = "Netzeinspeisung in kWh"),stat = "identity") +
      geom_point(aes(y = em),stat = "identity") +
      geom_point(aes(y = vm),stat = "identity") +
      geom_text(mapping = aes(x = month, y = em, label = em)) +
      geom_text(mapping = aes(x = month, y = vm, label = vm)) +
      xlab("") +
      ylab("")
        
  })
  
  
  output$radiation_chart <- renderPlot({
    data <- filtering()
    data %>%
      mutate(day = utc_timestamp %>% as.character() %>% substr(6,19)) %>%
      mutate(swm2 = solar_watt * input$efficency * input$m2 * sf * module_reduce) %>%
      mutate(e1 = ifelse(swm2 < consumw1 , swm2, ifelse(swm2 > consumw1, consumw1 , 0))) %>%
      mutate(v1 = ifelse(swm2 > consumw1, swm2 - consumw1, 0)) %>%
      group_by(day) %>%
      summarize(avg = mean(solar_watt, na.rm = TRUE) * input$m2, e = mean(e1), v = mean(v1), stdv1 = sd(v1, na.rm = TRUE) , slp = mean(consumw1)) %>%
      mutate(date = as.POSIXct(paste0("2020-", day), format = c("%Y-%m-%d %H:%M:%S"))) %>%
      ggplot() + 
      aes(x = date) +
      geom_smooth(aes(y = slp, colour = "Standardlastrofil  p.a")) +
      geom_smooth(aes(y = e, colour = "Eigenverbrauch")) +
      geom_smooth(aes(y = v, colour = "Einspeisung ins Netz  p.a")) +
      xlab("") +
      ylab("")
  })



 output$dataTable <- renderDT({
    data <- filtering()
    data %>% 
      mutate(day = utc_timestamp %>% as.character() %>% substr(6,19)) %>%
      mutate(swm2 = solar_watt * input$efficency * input$m2 * sf * module_reduce) %>%
      mutate(consum = ifelse(swm2 < consumw1 , swm2, ifelse(swm2 > consumw1, consumw1 , 0))) %>%
      mutate(sale = ifelse(swm2 > consumw1, swm2 - consumw1, 0)) %>%
      group_by(day) %>% 
      summarise(consum_mean = mean(consum, na.rm = TRUE), sale_mean = mean(sale,na.rm = TRUE), swm2 = mean(swm2,na.rm = TRUE), consumw1 = mean(consumw1,na.rm = TRUE)) %>% 
      mutate(date = as.POSIXct(paste0("2020-", day), format = c("%Y-%m-%d %H:%M:%S"))) %>%
      mutate(month = month(date)) %>% 
      group_by(month) %>%
      summarize(consum_month = floor(sum(consum_mean, na.rm = TRUE)), sale_month = floor(sum(sale_mean, na.rm = TRUE)), Erzeugung_Anlage = floor(sum(swm2,na.rm = TRUE)), Strombedarf = floor(sum(consumw1,na.rm = TRUE))) %>% 
      mutate(consum_perc = 100 * (consum_month / (sum(consum_month,na.rm = TRUE) + sum(sale_month,na.rm = TRUE)))) %>% 
      mutate(sale_perc = 100 * (sale_month / (sum(sale_month,na.rm = TRUE) + sum(consum_month,na.rm = TRUE))))
      

  })
  
}

