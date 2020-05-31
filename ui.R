library(shinydashboard)
titlePanel("Solar Deutschland")


sidebar <- dashboardSidebar(
  menuItem("PV-Anlage", tabName = "pv-anlage"),
  selectInput(inputId = "selected_country", label = "1.Select city", choices = sort(unique(sedn_slpc$Stadt))),
  dateRangeInput(inputId = "date", label = "2.Select years", format = "yyyy", startview = "decade", start = "1990-01-01", end = "2010-12-31"),
  numericInput(inputId = "m2", label = "3.Freie Dachfläche", value = "500"),
  numericInput(inputId = "efficency", label = "4.Effizienz Module", value = "0.2", min = 0, max = 1, step = 0.05),
  numericInput(inputId = "cost", label = "5.Kosten pro kWh", value = "0.16"),
  numericInput(inputId = "price", label = "6. Einspeisevergütung", value = "0.075"),
  numericInput(inputId = "invest", label = "7. Investition/kWp", value = "860"),
  numericInput(inputId = "lk", label = "8. Laufende Kosten pro Jahr/kWp", value = "10"),
  numericInput(inputId = "zi", label = "9. Zinsen", value = "0.02"),
  numericInput(inputId = "ek", label = "10. Anteil Eigenkapital", value = "0"),
  menuItem("Lastganganalyse", tabName = "lastgang"),
  fileInput("file","Upload the file"),
  helpText("Default max. file size is 5MB"),
  tags$hr(),
  h5(helpText("Select the read.table parameters below")),
  checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
  checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
  br(),
  radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
  )



# Investition Kosten
# Jährliche Kosten
# kWp

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "lastgang",
  h3("Photovoltaik Rendite Rechner", align = "center"),
  plotOutput("bar_chart"),
  hr( colour = "red"),
  fluidPage(fluidRow(column(12, div(dataTableOutput("dataTable"))))),
  hr( colour = "red")),
  uiOutput("tb"),
  tabItem(tabName = "pv-anlage",
  plotOutput("radiation_chart"),
  hr(),
  fluidRow(
      # Dynamic valueBoxes
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
    valueBoxOutput("ekwh_percent", width = 4),
    valueBoxOutput("vkwh_percent", width = 4)),
  hr(),
  fluidRow(
      valueBoxOutput("ev", width = 4),
      valueBoxOutput("es", width = 4),
      valueBoxOutput("ge", width = 4)),
  hr(),
  fluidRow(
    valueBoxOutput("cy", width = 4),
    valueBoxOutput("result", width = 4),
    valueBoxOutput("gk", width = 4))
  )
  )
  )

 

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Solar-Calc"),
  sidebar,
  body
)
