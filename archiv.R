###Radiation Chart Geom SMOOTH Archiv


output$radiation_chart <- renderPlot({
  filtering() %>% 
    group_by(day) %>%
    summarise(
      avg = mean(solar_watt, na.rm = TRUE) * input$m2,
      consum_day = mean(consum),
      sale_day = mean(sale),
      stdv1 = sd(sale_day, na.rm = TRUE) ,
      slp = mean(consumw1)
    ) %>%
    mutate(date = as.POSIXct(paste0("2020-", day), format = c("%Y-%m-%d %H:%M:%S"))) %>%
    ggplot() +
    scale_y_continuous(breaks = seq(0, 100, 10),limits=c(0, 100), expand = expand_scale(0.1))+
    scale_x_datetime(date_labels = "%b",date_breaks = "1 month", expand = c(0,0)) +
    aes(x = date) +
    geom_smooth(aes(y = slp, colour = "Standardlastprofil")) +
    geom_smooth(aes(y = consum_day, colour = "Eigenverbrauch")) +
    geom_smooth(aes(y = sale_day, colour = "Netzeinspeisung")) +
    xlab("") +
    ylab("") +
    scale_colour_manual(name = "legend",
                        values = c("blue", "green", "yellow")) +
    theme(
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA),
      axis.text.x = element_text(angle=45, hjust = 1, colour = "white"),
      axis.text.y = element_text(colour = "white"),
      panel.border = element_rect(colour = "white", fill=NA, size=1)
    )
}, bg = "transparent")
