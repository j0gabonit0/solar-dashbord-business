library(shinydashboard)
titlePanel("SolarCalculator Volti")
sidebar <- dashboardSidebar(
 sidebarMenu(
      selectizeInput(inputId = "selected_country", label = "1.Select city",options = list(maxOptions = 10000), choices = sort(unique(nuts_123_germany$place))),
      numericInput(inputId = "m2", label = "3.Freie Dachflaeche", value = "500"),
      numericInput(inputId = "efficency", label = "4.Effizienz Module", value = "0.2", min = 0, max = 1, step = 0.05),
      numericInput(inputId = "cost", label = "5.Kosten pro kWh", value = "0.16"),
      numericInput(inputId = "price", label = "6. Einspeiseverguetung", value = "0.075"),
      numericInput(inputId = "invest", label = "7. Investition/kWp", value = "860"),
      numericInput(inputId = "lk", label = "8. Laufende Kosten pro Jahr/kWp", value = "10"),
      numericInput(inputId = "zi", label = "9. Zinsen", value = "0.02"),
      numericInput(inputId = "ek", label = "10. Anteil Eigenkapital", value = "0"),
      numericInput(inputId = "latitude", label = "11. Breitengrad", value = "52.51"),
      numericInput(inputId = "longitude", label = "12. Längengrad", value = "13.41"),
      numericInput(inputId = "tilt_angle_modul", label = "13. Neigung des Moduls", value = "10"),
      numericInput(inputId = "azimuth_angle_modul", label = "15.Ausrichtung Modul Azimuth", value = "180"),
      numericInput(inputId = "albedo", label = "14.Albedo", value = "0.20"),
      fileInput("file","Upload the file"),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma = ',',Semicolon = ';',Tab = '\t', Space = ''), selected = ','),
    sidebarMenuOutput("sbmenu")
  )
)


body <- dashboardBody(
       h3("Photovoltaik Rendite Rechner", align = "center"),
              box(
              plotOutput("bar_chart"), 
              width = 6, 
              title = "Verhältnis Eigenverbrauch/Netzeinspeisung", 
              background = "green"),
              box(
              plotOutput("radiation_chart"), 
              width = 6,
              title = "Stromnetznutzung", 
              background = "yellow"),
       hr(),
       fluidPage(fluidRow(column(12, div(dataTableOutput("dataTable"))))),
       fluidPage(fluidRow(column(10, div(dataTableOutput("dtcsv"))))),
       hr(),
               fluidRow(
               valueBoxOutput("kwhm2", width = 4),
               valueBoxOutput("consum_proof", width = 4),
               valueBoxOutput("kwhkwp", width = 4)),
        hr(),
             fluidRow(
               valueBoxOutput("ekwh", width = 4),
               valueBoxOutput("vkwh", width = 4),
               valueBoxOutput("ms", width = 4)),
        hr(),
             fluidRow(
               valueBoxOutput("ekwh_percent", width = 6),
               valueBoxOutput("vkwh_percent", width = 6)),
        hr(),
             fluidRow(
               valueBoxOutput("ev", width = 4),
               valueBoxOutput("es", width = 4),
               valueBoxOutput("ge", width = 4)),
        hr(),
             fluidRow(
               valueBoxOutput("cy", width = 4),
               valueBoxOutput("result", width = 4),
               valueBoxOutput("gk", width = 4)),
       tableOutput("files"),
       fluidRow(
         valueBoxOutput("jo", width = 4)),
       fluidRow(
         plotlyOutput("pie_rent", height = 400, width = 400)
       )
       )
  

 

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Solar-Calc"),
  sidebar,
  body
)
