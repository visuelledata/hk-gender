library(tidyverse)
library(forcats)
library(SciencesPo)
library(scales)

pop <- read_csv("D:/rproj/hk-gender/preclean-excel.csv", 
                                  na = '-',
                                  col_types = cols(Australian = col_integer(), 
                                                   Indonesian = col_integer(), 
                                                   Japanese = col_integer(), 
                                                   Nepalese = col_integer(), 
                                                   Pakistani = col_integer(), 
                                                   Thai = col_integer()))

# To Do: clean graphs, population distribution HK, distribution by nationality, ...


pop$age_range <- factor(pop$age_range, levels = unique(pop$age_range))

pop <- pop %>% 
  gather(chinese_hk_resident:Others, key = 'nationality', value = 'population') %>% 
  mutate(prop = population / sum(population, na.rm = TRUE))


pop %>%  
  filter(age_range != '0 - 4' & age_range != '80 - 84' & age_range != '85+') %>%
  group_by(age_range, sex, nationality) %>%
  summarize(population = sum(population),
            prop = sum(prop)) %>% 
  ggplot() + 
  geom_col(aes(x = age_range, y = prop, fill = sex), position = 'dodge') + 
  facet_wrap(~nationality, scale = 'free_y') #add this as an interactive graph
  
  
pop %>%  
  filter(age_range != '0 - 4' & age_range != '80 - 84' & age_range != '85+') %>%
  group_by(age_range, sex) %>% 
  summarize(population = sum(population)) %>% 
  mutate(prop = population / sum(population)) %>% 
  ggplot() + 
  geom_col(aes(x = age_range, y = prop, fill = sex, width = .85), position = 'dodge') + 
  geom_text(aes(x = age_range, y = prop, fill = sex, label = percent(prop)), position = position_dodge(width = .9), vjust = .2, hjust = 1.1,size = 3, color = 'Grey70') +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) + 
  scale_fill_manual(values = c('Red', 'Blue')) +
  geom_hline(yintercept = .504, color = 'Grey', alpha = .7) + 
  theme_pub() +
  no_gridlines() + 
  no_x_axis() +
  no_x_line() +
  no_legend() +
  no_ticks() +
  coord_flip()
  

pop %>%  
  filter(age_range != '0 - 4' & age_range != '80 - 84' & age_range != '85+') %>%
  group_by(age_range, sex) %>% 
  summarize(population = sum(population)) %>% 
  mutate(prop = population / sum(population)) %>% 
  ggplot() + 
  geom_col(aes(x = age_range, y = prop, group = sex, fill = prop >= .504 & sex == 'female' & age_range != '75 - 79', width = .85), position = 'dodge') + 
  scale_fill_manual(values = c('Grey', 'Grey', 'Blue', 'Red')) +
  geom_text(aes(x = age_range, y = prop, fill = sex, label = percent(prop)), position = position_dodge(width = .9), vjust = .2, hjust = 1.1,size = 3, color = 'White') +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) + 
  geom_hline(yintercept = .504, color = 'Grey', alpha = .7) + coord_flip()
  theme_pub() +
  no_gridlines() + 
  no_x_axis() +
  no_x_line() +
  no_ticks() +
  coord_flip()
