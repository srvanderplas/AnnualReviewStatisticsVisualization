# tb plot script
library(tidyverse)

tb <- read_csv("data/TB_notifications_2018-03-18.csv") %>% 
  select(country, iso3, year, new_sp_m04:new_sp_fu) %>%
  gather(stuff, count, new_sp_m04:new_sp_fu) %>%
  separate(stuff, c("stuff1", "stuff2", "genderage")) %>%
  select(-stuff1, -stuff2) %>%
  mutate(gender=substr(genderage, 1, 1), 
         age=substr(genderage, 2, length(genderage))) %>%
  select(-genderage)

tb_au <- tb %>% 
  filter(country == "Australia") %>%
  filter(!(age %in% c("04", "014", "514", "u"))) %>%
  filter(year > 1996, year < 2013)

tb_au_12 <- tb %>% 
  filter(country == "Australia") %>%
  filter(!(age %in% c("04", "014", "514", "u"))) %>%
  filter(year == 2012)

ggplot(tb_au_12, aes(x = gender, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  facet_grid(~ age) +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.position = "none")

ggplot(tb_au_12, aes(x = age, y = count, fill = age)) +
  geom_bar(stat = "identity") +
  facet_grid(~ gender) +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.position = "none")
