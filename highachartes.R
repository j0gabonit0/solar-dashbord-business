
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
