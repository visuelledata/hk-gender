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
  

sex.scale <- c("female" = "red", "male" = "blue")

pop %>%  
  filter(age_range != '0 - 4' & age_range != '80 - 84' & age_range != '85+') %>%
  group_by(age_range, sex) %>% 
  summarize(population = sum(population)) %>% 
  mutate(prop = population / sum(population)) %>% 
  mutate(condition = prop >= 0.504 & sex == "female" & age_range != '75 - 79') %>%
  mutate(condition = any(condition)) %>% 
  ggplot(aes(x = age_range, y = prop, color = sex, group = sex)) + 
  geom_col(position = "dodge", fill = "grey60") +
  geom_col(aes(fill = sex, alpha = condition), position = "dodge") +
  scale_color_manual(values = sex.scale) +
  scale_fill_manual(values = sex.scale, guide = F) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) + 
  geom_text(aes(x = age_range, y = prop, fill = sex, label = percent(prop)), position = position_dodge(width = .9), vjust = .358, hjust = 1.1,size = 4, color = 'White') +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) + 
  geom_hline(yintercept = .504, color = 'White') + 
  coord_flip()+
  theme_pub() +
  no_gridlines() + 
  no_x_axis() +
  no_x_line() +
  no_ticks() +
  coord_flip() + 
  no_legend() + 
  labs(x = '', y = '')


