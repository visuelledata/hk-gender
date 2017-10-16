library(tidyverse)
library(forcats)
library(SciencesPo)
library(scales)
library(stringr)

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
  

sex.scale <- c("female" = "#E24645", "male" = "#428DB6")

pop %>%  
  group_by(age_range, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  mutate(condition = prop >= 0.504 & sex == "female" & age_range != '75 - 79' & age_range != '80 - 84' & age_range != '85+' & age_range != '60 - 64') %>%
  mutate(condition = any(condition)) %>% 
  ggplot(aes(x = age_range, y = prop, color = sex, group = sex)) + 
  geom_hline(yintercept = .504, color = 'grey20', linetype = 'dashed', alpha = .7) + 
  geom_col(position = "dodge", fill = "grey73") +
  geom_col(aes(fill = sex, alpha = condition), position = "dodge") +
  scale_color_manual(values = sex.scale) +
  scale_fill_manual(values = sex.scale, guide = F) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) + 
  geom_text(aes(x = age_range, y = prop, fill = sex, label = percent(prop)), position = position_dodge(width = .9), vjust = .358, hjust = 1.1,size = 4, color = 'White') +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) + 
  coord_flip()+
  theme_pub(axis_line = TRUE) +
  no_gridlines() + 
  no_x_axis() +
  no_x_line() +
  annotate_color(x = .6, y = .57, size = 3,
                 labels = '- Global % of Women', 
                 colors = c('grey20', 'grey20', 'grey20', 'grey20', '#E24645')) +
  no_ticks() +
  theme(axis.text.y=element_text(color = "grey55"),
        plot.title = element_text(color = 'grey45', hjust = -.3, vjust = 5), 
        plot.margin = unit(c(1,.5, .5, .5), 'cm')) + 
  coord_flip() + 
  no_legend() + 
  labs(x = '', y = '', title = 'Women significantly outnumber men from 20 to 59 in Hong Kong')


####look into
#pop %>%  
  group_by(age_range, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  ggplot() + 
  geom_col(aes(x = age_range, y = prop, fill = sex)) +
  annotate_color(x = 5, y = .06, fontface = 'bold', size = 7,
                 labels = 'I go to school today and have', 
                 colors = c('blue', 'red', 'green', 'pink', 'grey', 'orange', 'pink')) +
  coord_polar(theta = "y") + 
  geom_text(aes(x = age_range, y = prop, label = age_range)) + 
  coord_flip()


  