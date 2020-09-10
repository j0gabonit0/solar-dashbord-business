fig <- plot_ly(
  domain = sedn_slpc %>% 
    mutate(swm2 = radiation_direct_horizontal) %>%
    mutate(e1 = ifelse(swm2 <= consumw1, swm2, consumw1)) %>%
    mutate(v1 = ifelse(swm2 >= consumw1, swm2 - consumw1, 0)) %>%
    summarise(
      ekwh = (sum(e1, na.rm = TRUE) / years),
      vkwh = (sum(v1, na.rm = TRUE) / years),
      z = 1) %>% 
  domain = list(x = c(0, 20), y = c(0, 1001000)),
  value = 2700,
  title = list(text = "Speed"),
  type = "indicator",
  mode = "gauge+number") 
fig <- fig %>%
  layout(margin = list(l=20,r=30))


fig <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = 90,
  title = list(text = "Speed"),
  type = "indicator",
  mode = "gauge+number") 
fig <- fig %>%
  layout(margin = list(l=20,r=30))

domain2 = sedn_slpc %>% 
  mutate(swm2 = radiation_direct_horizontal) %>%
  mutate(e1 = ifelse(swm2 <= consumw1, swm2, consumw1)) %>%
  mutate(v1 = ifelse(swm2 >= consumw1, swm2 - consumw1, 0)) %>%
  summarise(
    ekwh = (sum(e1, na.rm = TRUE) / 100 / years),
    vkwh = (sum(v1, na.rm = TRUE) / 100 /years),
    z = 1)


  
  fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = (20 / (5+20)) * 100,
    title = list(text = "Eigenverbrauch in %"),
    type = "indicator",
    mode = "gauge+number+delta",
    delta = list(reference = 500),
    gauge = list(
      bar = list(color = "darkblue"),
      axis =list(range = list(NULL, 100)),
      steps = list(
        list(range = c(0, 10), color = "darkred"),
        list(range = c(10, 25), color = "yellow"),
        list(range = c(25, 100), color = "green")),
      threshold = list(
        line = list(color = "blue", width = 3),
        thickness = 0.8,
        value = 10)),
        margin = list(l=20,r=30))
  fig
  
  
  
fig

fig <- plot_ly(
  domain = sedn_slpc %>% 
    mutate(swm2 = radiation_direct_horizontal) %>%
    mutate(e1 = ifelse(swm2 <= consumw1, swm2, consumw1)) %>%
    mutate(v1 = ifelse(swm2 >= consumw1, swm2 - consumw1, 0)) %>%
    summarise(
      ekwh = (sum(e1, na.rm = TRUE) / years),
      vkwh = (sum(v1, na.rm = TRUE) / years),
      z = 1,) %>% 
  value = 450,
  title = list(text = "Speed"),
  type = "indicator",
  mode = "gauge+number+delta",
  delta = list(reference = 380),
  gauge = list(
    axis =list(range = list(NULL, 500)),
    steps = list(
      list(range = c(0, 100), color = "lightgray"),
      list(range = c(250, 400), color = "gray")),
    threshold = list(
      line = list(color = "yellow", width = 4),
      thickness = 0.05,
      value = 490))) 
fig

domain






 fig <-  sedn_slpc %>% 
    mutate(swm2 = radiation_direct_horizontal) %>%
    mutate(e1 = ifelse(swm2 <= consumw1, swm2, consumw1)) %>%
    mutate(v1 = ifelse(swm2 >= consumw1, swm2 - consumw1, 0)) %>%
    summarise(
      ekwh = (sum(e1, na.rm = TRUE) / years),
      vkwh = (sum(v1, na.rm = TRUE) / years),
      z = 1
    ) %>% 
    pivot_longer(-z, names_to = "name", values_to = "values") %>% 
    plot_ly(labels = ~name, values = ~values) %>%
    add_pie(hole = 0.95) %>%
    layout(title = "Eigenverbrauch/Netzeinspeisung",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
           paper_bgcolor='transparent')
fig 
