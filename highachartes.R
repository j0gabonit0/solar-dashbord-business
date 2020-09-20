
q <- sedn_slpc_t %>%
  mutate(day = utc_timestamp %>% as.character() %>% substr(6, 19)) %>%  #mehrere Jahre auf ein JAhr reduzieren
  mutate(day = day %>% as.character() %>% substr(1, 5)) %>% # 24 Stunden auf einen Tag reduzieren 
  group_by(day) %>% 
  summarise(consum_day = sum(consumw1), 
            production_day = sum(radiation_direct_horizontal)) %>% 
  mutate(date = as.POSIXct(paste0("2020-", day, "-01"), format = c("%Y-%m-%d"))) %>%
  pivot_longer(cols=c('consum_day', 'production_day'), names_to='variable1', 
               values_to="value") %>% 
  mutate(date = as.POSIXct(paste0("2020-", day), format = c("%Y-%m-%d"))) %>% 
  select(-day) %>% 
  drop_na() %>% 
  mutate(month = month(date)) %>% 
  filter(variable1 == "production_day") %>% 
  ggplot( aes(x=date, y = value)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(month ~ ., scales = "free_x",ncol=4)
q

str(q)


w <- sedn_slpc %>% 
  filter(country_nuts2 == "DE11") %>% 
  mutate(day = utc_timestamp %>% as.character() %>% substr(6, 19)) %>%  #mehrere Jahre auf ein JAhr reduzieren
  group_by(day) %>% 
  summarise(consumw1 = sum(consumw1)) %>% 
  arrange(desc(consumw1)) %>%
  slice(1:10) %>% 
  mutate(consumw1 = consumw1 / years) %>%
  #mutate(date = as.POSIXct(paste0("2020-", day), format = c("%Y-%m-%d %H:%M:%S"))) %>%
  mutate(date = day %>% as.character()) %>% 
  rowid_to_column("ID")  %>% 
  ggplot( aes(x = date, y = consumw1, label = ID)) +
  geom_bar(stat='identity', position='dodge') +
  geom_label(aes(label = consumw1, hjust = 0.5), size = 2)+
  geom_text(aes(label = day), vjust = 0) + 
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    axis.text.x = element_text(angle=45, hjust = 1),
    panel.border = element_rect(colour = "white", fill=NA, size=1)
  )
w  

