library(tidyverse)
library(broom)

dd <- get(load("measurements/all_data.Rsave"))

slopes <- dd %>% 
  group_by(zone, strain, treatment, replicate, transfer) %>% 
  mutate(day_per_transfer=row_number()) %>% 
  filter(day_per_transfer %in% 2:6) %>% 
  nest() %>% 
  mutate(
    fit = map(data, ~ lm(log(Sum) ~ day_per_transfer, data = .)), results = map(fit, tidy)) %>% 
  unnest(results) %>% 
  filter(term=="day_per_transfer")
  
save(slopes, file="measurements/slopes.Rsave")
write_csv(slopes, path="measurements/slopes.csv")
