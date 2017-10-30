library(tidyverse)
library(forcats)
library(SciencesPo)
library(scales)
library(stringr)
library(grid)
library(gridExtra)
library(RColorBrewer)

pop <- read_csv("D:/rproj/hk-gender/preclean-excel.csv", 
                                  na = '-',
                                  col_types = cols(Australian = col_integer(), 
                                                   Indonesian = col_integer(), 
                                                   Japanese = col_integer(), 
                                                   Nepalese = col_integer(), 
                                                   Pakistani = col_integer(), 
                                                   Thai = col_integer()))

source("annotate_color.R")

# To Do: clean graphs, population distribution HK, distribution by nationality, ...


pop$age_range <- factor(pop$age_range, levels = unique(pop$age_range))
pop$sex <- factor(pop$sex, levels = unique(pop$sex))

pop <- pop %>% 
  gather(`Chinese (HK Resident)`:Others, key = 'nationality', value = 'population') %>% 
  mutate(prop = population / sum(population, na.rm = TRUE))




women.scale <- brewer.pal(9, "Reds")
men.scale <- brewer.pal(9, "Blues")
sex.scale <- c("female" = "#FB8072", "male" = "#80B1D3")












# Exploratory data analysis------------------------------------------------------------------------------------------


pop %>%  
  filter(age_range != '0 - 4' & age_range != '80 - 84' & age_range != '85+') %>%
  group_by(age_range, sex, nationality) %>%
  summarize(population = sum(population),
            prop = sum(prop)) %>% 
  ggplot() + 
  geom_col(aes(x = age_range, y = prop, fill = sex), position = 'dodge') + 
  facet_wrap(~nationality, scale = 'free_y') #add this as an interactive graph
  





# Start---------------------------------------------------------------------------------------------------------------
pop %>%  #Try as lollipop chart 
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



pop %>%  
  group_by(age_range, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  mutate(condition = prop >= 0.504 & sex == "female" & age_range != '75 - 79' & age_range != '80 - 84' & age_range != '85+' & age_range != '60 - 64') %>%
  mutate(condition = any(condition)) %>% 
  ggplot(aes(x = age_range, y = prop, color = sex, group = sex)) +
  geom_linerange(aes(ymin = 0, ymax = prop), 
                 position = position_dodge(width = .7), color = 'grey60') + 
  geom_point(size = 5, position = position_dodge(width = .7)) + 
  scale_color_manual(values = c(men.scale[5], women.scale[5])) 



pop %>%  
  group_by(age_range, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  mutate(condition = sex == "female" & (age_range == '0 - 4' | age_range == '5 - 9' | age_range == '75 - 79' | 
                                        age_range == '80 - 84'| age_range == '85+')) %>%
  mutate(condition = any(condition)) %>% 
  ggplot(aes(x = age_range, y = prop, color = sex, group = sex)) +
  geom_linerange(aes(ymin = 0, ymax = prop), 
                 position = position_dodge(width = .5), color = 'grey80') + 
  geom_point(aes(), 
             size = 5, position = position_dodge(width = .5), color = 'grey80') + 
  geom_point(aes(alpha = condition), 
             size = 5, position = position_dodge(width = .5)) + 
  scale_color_manual(values = c(men.scale[6], women.scale[6])) 


pop %>%  
  group_by(age_range, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  mutate(condition = prop >= 0.504 & sex == "female" & age_range != '75 - 79' & age_range != '80 - 84' & age_range != '85+' & age_range != '60 - 64') %>%
  mutate(condition = any(condition)) %>% 
  filter(condition == TRUE) %>% 
  ggplot(aes(x = age_range, y = prop, color = sex, group = sex)) +
  geom_hline(yintercept =  .5) +
  geom_linerange(aes(ymin = 0, ymax = prop), 
                 position = position_dodge(width = .7), color = 'grey60') + 
  geom_point(aes(), 
             size = 5, position = position_dodge(width = .7)) + 
  scale_color_manual(values = c(men.scale[5], women.scale[5])) 



# End---------------------------------------------------------------------------------------------------------------------





# Population by Nationality--------------------------------------------------------------------------------------------
pop %>% 
  filter(nationality != 'Chinese (HK Resident)') %>% 
  group_by(nationality) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(condition = nationality == 'Filipino' | nationality == 'Indonesian' |
                     nationality == 'Chinese (Temp Resident)' | nationality == 'British' | nationality == 'Indian') %>% 
  ggplot(aes(fct_reorder(nationality, population), population)) + 
  geom_col(fill = 'grey 89') + 
  geom_col(aes(alpha = condition), fill = '#807DBA') +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) + 
  theme_pub() + 
  no_legend() + 
  theme(axis.text = element_text(color = 'Grey 48', angle = 30, hjust = 1.1, vjust = -16))


#There are 7,336,585 people in Hong Kong
sum(pop$population, na.rm = TRUE) 
#There are 6,646,415 Chinese Residents
sum(filter(pop, nationality == 'Chinese (HK Resident)')$population, na.rm = TRUE)
# 90.59% of people are Chinese residents, 9.41% are minorities
sum(filter(pop, nationality == 'Chinese (HK Resident)')$population, na.rm = TRUE) / sum(pop$population, na.rm = TRUE)
1 - sum(filter(pop, nationality == 'Chinese (HK Resident)')$population, na.rm = TRUE) / sum(pop$population, na.rm = TRUE)
# 54% Female and 46% male
pop %>% 
  group_by(sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population))
# With only Chinese (HK), 48.1% male, 51.9% female
pop %>% 
  filter(nationality == 'Chinese (HK Resident)') %>% 
  group_by(sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population))
#Without Chinese (HK), 25.5% male, 74.5% female
pop %>% 
  filter(nationality != 'Chinese (HK Resident)') %>% 
  group_by(sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population))
# These groups make up 77% of foreigners
minority <- pop %>% 
  filter(nationality != 'Chinese (HK Resident)') %>% 
  group_by(nationality) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  arrange(desc(prop))
minority$prop[1] + minority$prop[2] + minority$prop[3] + minority$prop[5] + minority$prop[6]  

#End- Population by nationality-------------------------------------------------------------------------------------------


# Bar chart nationality------------------------------------------------------------------------------------------------------

grouped <- pop %>% 
  group_by(nationality, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population))

pop_by_nation <- pop %>% 
  group_by(nationality, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  mutate(condition = nationality == 'Filipino' | nationality == 'Indonesian' |
                     nationality == 'Chinese (Temp Resident)' | nationality == 'British' | nationality == 'Indian') %>% 
  ggplot(aes(x = fct_reorder2(nationality, sex, -prop), y = prop, fill = sex)) + 
  geom_col(width = .8, fill = 'grey 90') +
  geom_col(aes(alpha = condition, fill = sex), width = .8) +
  scale_fill_manual(values = c(men.scale[3], women.scale[5]), expand = c(0,0)) + 
  theme_pub(axis_line = TRUE) + 
  no_gridlines() + 
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(labels = c("British", "Pakistani", "American", "Australian", "Nepalese", "Other", "Japanese", 
                              "Chinese HK", "Indian", "Other Chinese ", "Thai", 'Filipino', "Indonesian")) + 
  no_y_line() + 
  no_y_text() + 
  no_y_ticks() +
  geom_text(data = subset(grouped, sex == 'female'), aes(x = nationality, y = prop, 
                                                         fill = sex, label = percent(prop)), 
            vjust = 1.3, color = 'Grey95', size = 3.5) + 
  no_legend() + 
  labs(x = '\nNationality', y = 'Percent of women in group\n') + 
  theme(plot.margin = unit(c(.9, 1, 1, 1.2), "cm"), #  top, right, bottom, left
        plot.title = element_text(hjust = -.11, vjust = 5.5, color = 'Grey 23'),
        axis.title = element_text(color = 'Grey40', size = 14),
        axis.title.y = element_text(hjust = .94),
        axis.line.x = element_line(color = 'Grey 55'),
        axis.text = element_text(color = 'Grey 48', angle = 30, hjust = 1.1, vjust = -16),
        axis.ticks = element_line(color = 'Grey 60'),
        axis.ticks.length = unit(0, "cm")) + 
  annotate('text', x = 13.33, y = 1.01, label = '---------100%', color = 'grey 50', size = 4)


grobs <- grobTree(
  gp = gpar(fontsize = 14, fontface = 'bold'), 
  textGrob(label = "4 of the 5 largest nationalities are dominated by ", name = "title1",
           x = unit(2.33, "lines"), y = unit(-.5, "lines"), 
           hjust = 0, vjust = 0, gp = gpar(col = 'Grey 23')),
  textGrob(label = "Women", name = "title2",
           x = grobWidth("title1") + unit(2.24, "lines"), y = unit(-.5, "lines"),
           hjust = 0, vjust = 0, gp = gpar(col = women.scale[5]))
)


gb <- ggplot_build(pop_by_nation)
gt <- ggplot_gtable(gb)
gt$layout$clip[gt$layout$name=="panel"] <- "off"
gg <- arrangeGrob(gt, top=grobs, padding = unit(2.6, "line"))
grid.newpage()
grid.draw(gg)

ggsave('bar_nation.jpeg', gg)
# End- Bar chart nationality-----------------------------------------------------------------------------------------


# Population Pyramids-----------------------------------------------------------------------------------------------------
pop_pyr_format <- function(data, filter_cat = NA){ 
  if (!is.na(filter_cat)){
  women <- pop %>% 
    filter(nationality == filter_cat) %>%
    mutate(population = (population * -1))
  men <- pop %>% 
    filter(nationality == filter_cat)
  pop_pyr_data <- bind_rows(subset(women, sex == 'female'), subset(men, sex == 'male')) 
  return(pop_pyr_data)
  } else {
    women <- pop %>% 
      mutate(population = (population * -1))
    men <- pop
    pop_pyr_data <- bind_rows(subset(women, sex == 'female'), subset(men, sex == 'male'))
    return(pop_pyr_data)
  }
}

pyramid_nationality <- pop_pyr_format(pop) %>% 
  filter(nationality != 'Chinese (HK Resident)') %>% 
  ggplot(aes(x = age_range, y = population, fill = sex, frame = nationality)) +   # Fill column
  geom_col(width = .6, position = 'identity') +
  coord_flip(ylim = c(-50000, 50000), expand = c(0,0)) +  # Flip axes
  #labs(title="Population by age ") +
  scale_y_continuous(breaks = seq(-50000, 50000, 10000), 
                     labels = paste0(as.character(c(seq(50, 0, -10), seq(10, 50, 10))), "k")) +
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values = c(men.scale[5], women.scale[5])) + 
  no_legend()

gganimate(pyramid_nationality, 'pyramid.gif')


bar_nationality <- pop %>% 
  group_by(nationality, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  filter(nationality != 'Chinese (HK Resident)') %>% 
  ggplot(aes(x = sex, y = prop, fill = sex, frame = nationality)) + 
  geom_col(position = 'identity') + 
  geom_text(aes(label = percent(prop)), vjust = -.8, size = 4) + 
  scale_fill_manual(values = c(men.scale[5], women.scale[5])) + 
  theme_pub() +
  ylim(0,1) + 
  no_y_axis() + 
  no_legend() 

gganimate(bar_nationality, 'bars.gif')


ggdraw(a) + draw_plot(temp1, x = -0.25, y = -0.25, scale = .5)


#overall pop dist
pop_pyr_format(pop) %>% 
  ggplot(aes(x = age_range, y = population, fill = sex)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  coord_flip(ylim = c(-350000, 350000)) +  # Flip axes
  labs(title="Email Campaign Funnel") +
  scale_y_continuous(breaks = seq(-50000, 50000, 10000), 
                     labels = paste0(as.character(c(seq(50, 0, -10), seq(5, 50, 10))), "k")) +
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_manual(values = c(men.scale[5], women.scale[5]))











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

  
  
grouped <- pop %>%  
  group_by(age_range, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population))
  
pop %>%  
  group_by(age_range, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  ggplot() + 
  geom_col(aes(x = age_range, y = prop, fill = sex), width = .97) +
  annotate_color(x = 5.4, y = .06, size = 5.5, default_color = 'black',
                   labels = 'In Hong Kong,                                      ', 
                   colors = c('Grey30')) + 
    annotate_color(x = 3, y = .06, size = 4.5, default_color = 'grey40',
                   labels = 'Women outnumber    Men in most age groups', 
                   colors = c('black', '', '', '', '', 'black')) +
    coord_polar(theta = "y") + 
    theme_pub() +
    no_legend() + 
    no_y_axis() +
    no_x_axis() + 
    scale_y_continuous(limits = c(0, 1), expand = c(0,0)) + 
    geom_text(data = subset(grouped, sex == 'male'), 
              aes(x = age_range, y = prop, label = age_range), 
              angle = 16, color = 'grey20', size = 3.2,
              hjust = -.1, position = position_dodge(width=.5)) +
    geom_hline(yintercept = .5, linetype = 'dashed', color = 'grey35') +
    theme(plot.margin = unit(c(0,0, 0, 0), 'cm'))
  ggsave('circleplot.jpg', plot = last_plot())
  