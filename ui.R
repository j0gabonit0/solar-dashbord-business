library(shinydashboard)
titlePanel("Solar Deutschland")

sidebar <- dashboardSidebar(
  selectInput(inputId = "selected_country", label = "1.Select city", choices = sort(unique(sedn_slpc$Stadt))),
  dateRangeInput(inputId = "date", label = "2.Select years", format = "yyyy", startview = "decade", start = "1990-01-01", end = "2010-12-31"),
  numericInput(inputId = "kwhy", label = "3.kWh Jahresverbrauch", value = "440000"),
  numericInput(inputId = "m2", label = "4.Freie Dachfläche", value = "500"),
  numericInput(inputId = "efficency", label = "5.Effizienz Module", value = "0.2", min = 0, max = 1, step = 0.05),
  numericInput(inputId = "cost", label = "6.Kosten pro kWh", value = "0.16"),
  numericInput(inputId = "price", label = "7. Einspeisevergütung", value = "0.075"),
  numericInput(inputId = "invest", label = "8. Investition/kWp", value = "860"),
  numericInput(inputId = "lk", label = "9. Laufende Kosten pro Jahr/kWp", value = "8"),
  numericInput(inputId = "zi", label = "10. Zinsen", value = "0.015"),
  numericInput(inputId = "ek", label = "11. Anteil Eigenkapital", value = "0.7")
  
)

# Investition Kosten
# Jährliche Kosten
# kWp

body <- dashboardBody(
  h3("Photovoltaik Rendite Rechner", align = "center"),
  plotOutput("radiation_chart"),
  hr( colour = "red"),
  fluidRow(
      # Dynamic valueBoxes
      valueBoxOutput("yieldm2", width = 4),
      valueBoxOutput("m", width = 4),
      valueBoxOutput("ms", width = 4)),
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
 

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Solar Germany"),
  sidebar,
  body
)
