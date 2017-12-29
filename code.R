library(tidyverse)
library(forcats)
library(SciencesPo)
library(scales)
library(stringr)
library(grid)
library(gridExtra)

# Setup--------------------------------------------------------------------------------------------------------------------------
pop <- read_csv("D:/rproj/hk-gender/preclean-excel.csv", #this data is by nationality
                                  na = '-',
                                  col_types = cols(Australian = col_integer(), 
                                                   Indonesian = col_integer(), 
                                                   Japanese = col_integer(), 
                                                   Nepalese = col_integer(), 
                                                   Pakistani = col_integer(), 
                                                   Thai = col_integer()))     #upload data and assign column types


pop$age_range <- factor(pop$age_range, levels = unique(pop$age_range))
pop$sex <- factor(pop$sex, levels = unique(pop$sex))

pop <- pop %>% 
  gather(`Chinese (HK Resident)`:Others, key = 'nationality', value = 'population') %>% 
  mutate(prop = population / sum(population, na.rm = TRUE))


#Color Palettes
women.scale <- RColorBrewer::brewer.pal(9, "Reds")
men.scale <- RColorBrewer::brewer.pal(9, "Blues")
sex.scale <- c("female" = "#FB8072", "male" = "#80B1D3")


# To Do: clean graphs


# Functions-----------------------------------------------------------------------------------------------------------------
source('annotate_color.R') #load annotate_color()

source('title_no_clip.R')


pop_pyr_format <- function(data, filter_cat = NA){ 
  if (!is.na(filter_cat)){
    women <- pop %>% 
      filter(nationality == filter_cat) %>%
      mutate(population = (population * -1),
             prop = (prop * -1))
    men <- pop %>% 
      filter(nationality == filter_cat)
    pop_pyr_data <- bind_rows(subset(women, sex == 'female'), subset(men, sex == 'male')) 
    return(pop_pyr_data)
  } else {
    women <- pop %>% 
      mutate(population = (population * -1),
             prop = (prop * -1))
    men <- pop
    pop_pyr_data <- bind_rows(subset(women, sex == 'female'), subset(men, sex == 'male'))
    return(pop_pyr_data)
  }
}

# Exploratory data analysis------------------------------------------------------------------------------------------

pop %>%  
  filter(age_range != '0 - 4' & age_range != '80 - 84' & age_range != '85+') %>%
  group_by(age_range, sex, nationality) %>%
  summarize(population = sum(population),
            prop = sum(prop)) %>% 
  ggplot() + 
  geom_col(aes(x = age_range, y = prop, fill = sex), position = 'dodge') + 
  facet_wrap(~nationality, scale = 'free_y') #add this as an interactive graph
  




# Percent of women in age_group---------------------------------------------------------------------------------------------------------------

grouped <- pop %>%  
  group_by(age_range, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population))

# population by age and gender
grouped %>% 
  ggplot(aes(x = age_range, y = prop, color = sex, group = sex)) +
  geom_hline(yintercept = .5, linetype = 'dotted', color = 'grey20') + 
  geom_linerange(aes(ymin = 0, ymax = prop), 
                 position = position_dodge(width = .5), color = 'grey60') + 
  geom_point(size = 6, position = position_dodge(width = .5)) + 
  labs(x = '\nAge range', y = 'Percent within age group\n') + 
  scale_color_manual(values = c(men.scale[5], women.scale[5])) + 
  scale_y_continuous(limits = c(0, .7),expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 80, vjust = .2, hjust = .2)) + 
  no_legend()
  

title_default <- function(plot = last_plot(), title_segments, colors){
  title_align_no_clip(
    plot = plot,
    title_segments = title_segments,
    colors = colors,
    axis.title.color = 'Grey40', 
    axis.text.color =  'Grey40',
    axis.x.line.color = 'Grey35',
    axis.y.line.color = 'Grey35', 
    tick.color = 'Grey35')
}

title_default(title_segments = c('Percent of ', 'men', ' and ', 'women ', 'within each age group'),
                    colors = c('Grey25', men.scale[4], 'Grey25', women.scale[4], 'Grey25'))
                   


#highlight ends
grouped %>% 
  mutate(condition = sex == "female" & (age_range == '0 - 4' | age_range == '5 - 9' | age_range == '75 - 79' | 
                                        age_range == '80 - 84'| age_range == '85+')) %>%
  mutate(condition = any(condition)) %>% 
  ggplot(aes(x = age_range, y = prop, color = sex, group = sex)) +
  geom_linerange(aes(ymin = 0, ymax = prop), 
                 position = position_dodge(width = .5), color = 'grey95') + 
  geom_linerange(aes(ymin = 0, ymax = prop, alpha = condition), 
                 position = position_dodge(width = .5), color = 'grey50') + 
  geom_point(size = 6, position = position_dodge(width = .5), color = 'grey95') + 
  geom_point(aes(alpha = condition), 
             size = 6, position = position_dodge(width = .5)) + 
  labs(x = '\nAge range', y = 'Percent within age group\n') + 
  scale_color_manual(values = c(men.scale[5], women.scale[5])) +
  scale_y_continuous(limits = c(0, .7),expand = c(0,0)) + 
  scale_x_discrete(labels = c('0 - 4', '5 - 9', rep(' ', 13), '75 - 79', '80 - 84', '85+')) +
  theme(axis.text.x = element_text(angle = 80, vjust = .2, hjust = .2)) + 
  no_legend() + 
  annotate(geom = 'text', x = .8, y = .59, hjust = 0, 
           color = 'Grey50', size = 3.8,
           label = c('More boys are born \nthan girls globally')) + 
  annotate(geom = 'text', x = 14.1, y = .61, hjust = 0, 
           color = 'Grey50', size = 3.8,
           label = c('Women have longer \nlife expectancies than \nmen'))

title_default(title_segments = c('These data points are caused by', 'birth ', 'and ', 'life expectancy ', 'phenomena.'),
                    colors = c('Grey40', 'Grey20', 'Grey40', 'Grey20', 'Grey40'))

#highlight target
highlight_target <- grouped %>% 
  mutate(condition = prop >= 0.504 & sex == "female" & age_range != '75 - 79' & age_range != '80 - 84' & age_range != '85+' & age_range != '60 - 64') %>%
  mutate(condition = any(condition)) %>% 
  ggplot(aes(x = age_range, y = prop, color = sex, group = sex)) +
  geom_linerange(aes(ymin = 0, ymax = prop), 
                 position = position_dodge(width = .6), color = 'grey95') + 
  geom_linerange(aes(ymin = 0, ymax = prop, alpha = condition), 
                 position = position_dodge(width = .6), color = 'grey50') + 
  geom_point(size = 4, position = position_dodge(width = .6), color = 'grey80') + 
  geom_point(aes(alpha = condition), 
             size = 8, position = position_dodge(width = .6)) + 
  geom_text(data = subset(filter(grouped, sex == 'female'), age_range == '20 - 24' | age_range == '25 - 29'|
                                                            age_range == '30 - 34' | age_range == '35 - 39'|
                                                            age_range == '40 - 44' | age_range == '45 - 49'|
                                                            age_range == '50 - 54' | age_range == '55 - 59'), 
            aes(x = age_range, y = prop, fill = sex, label = percent(round(prop, digits = 2))), 
            color = 'white', size = 2.6, hjust = .2, vjust = 0) +
  geom_text(data = subset(filter(grouped, sex == 'male'), age_range == '20 - 24' | age_range == '25 - 29'|
                                                          age_range == '30 - 34' | age_range == '35 - 39'|
                                                          age_range == '40 - 44' | age_range == '45 - 49'|
                                                          age_range == '50 - 54' | age_range == '55 - 59'), 
            aes(x = age_range, y = prop, fill = sex, label = percent(round(prop, digits = 2))), 
            color = 'white', size = 2.6, hjust = .75, vjust = .3) +
  labs(x = '\nAge range', y = 'Percent within age group\n') + 
  scale_color_manual(values = c(men.scale[5], women.scale[5])) +
  scale_y_continuous(limits = c(0, .7),expand = c(0,0)) +
  scale_x_discrete(labels = c(rep(' ', 4), '20 - 24', '25 - 29', '30 - 34', 
                                           '35 - 39', '40 - 44', '45 - 49', 
                                           '50 - 54', '55 - 59', rep(' ', 6))) + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_text(color = 'Grey40', size = 14),
        axis.title.y = element_text(hjust = .94),
        axis.line.x = element_line(color = 'grey35'),
        axis.line.y = element_line(color = 'grey35'),
        axis.text = element_text(color = 'grey40'),
        axis.ticks = element_line(color = 'grey35')) + 
  no_legend() 



subplot <- pop %>%
  filter(age_range == '20 - 24' | age_range == '25 - 29'|
         age_range == '30 - 34' | age_range == '35 - 39'|
         age_range == '40 - 44' | age_range == '45 - 49'|
         age_range == '50 - 54' | age_range == '55 - 59') %>% 
  group_by(sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  ggplot(aes(x = sex, y = prop)) + 
  geom_col(aes(fill = sex), width = .9) + 
  geom_text(aes(label = percent(prop)), 
            color = 'white', vjust = 1.5) + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = c(men.scale[3], women.scale[3])) +
  no_legend() + 
  no_axes() + 
  theme(plot.title = element_text(color = 'grey60', size = 12.5, hjust = 0)) + 
  ggtitle('  Percent of women and \n  men in highlighted data')
#Remove some axis text from the base plot 

title_default(plot = highlight_target,
              title_segments = c('From now on, ', 'this data ', 'will be our ', 'focus'),
              colors = c('Grey40', 'Grey27', 'Grey40', 'Grey27')) +
  draw_plot(subplot, x = .3, y = -.144, scale = 0.25)



# Population by Nationality--------------------------------------------------------------------------------------------
pop %>% 
  filter(nationality != 'Chinese (HK Resident)') %>% 
  group_by(nationality) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(condition = nationality == 'Filipino' | nationality == 'Indonesian' |
                     nationality == 'Chinese (Other)') %>% 
  ggplot(aes(fct_reorder(nationality, population), population)) + 
  geom_col(fill = 'grey 89') + 
  geom_col(aes(alpha = condition), fill = '#807DBA') +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) + 
  theme_pub(axis_line = TRUE) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1.1)) + 
  labs(x = '\nNationality', y = 'Population\n') + 
  no_legend()

pop_by_nationality <- title_default(title_segments = c('The biggest nationalities have significantly more women than men', ''),
              colors = c('grey27', 'grey27'))
#There are 7,336,585 people in Hong Kong
sum(pop$population, na.rm = TRUE) 
#There are 6,646,415 Chinese Residents
sum(filter(pop, nationality == 'Chinese (HK Resident)')$population, na.rm = TRUE)
# 90.59% of people are Chinese residents, 9.41% have foreign nationalities
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


# Bar chart nationality------------------------------------------------------------------------------------------------------

grouped <- pop %>% 
  group_by(nationality, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population))

pop %>% 
  group_by(nationality, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  mutate(condition = nationality == 'Filipino' | 
                     nationality == 'Indonesian' |
                     nationality == 'Chinese (Other)') %>% 
  ggplot(aes(x = fct_reorder2(nationality, sex, -prop), y = prop, fill = sex)) + 
  geom_col(width = .8, fill = 'grey 90') +
  geom_col(aes(alpha = condition, fill = sex), width = .8) +
  scale_fill_manual(values = c(men.scale[3], women.scale[5]), expand = c(0,0)) + 
  theme_pub(axis_line = TRUE) + 
  no_gridlines() + 
  scale_y_discrete(expand = c(0,0)) +
  no_y_line() + 
  no_y_text() + 
  no_y_ticks() +
  geom_text(data = subset(grouped, sex == 'female'), 
            aes(x = nationality, y = prop, 
                fill = sex, label = percent(prop)), 
            vjust = 1.3, color = 'Grey95', size = 3.5) + 
  no_legend() + 
  labs(x = '\nNationality', y = 'Percent of women in group\n') + 
  theme(axis.text = element_text(angle = 30, hjust = 1.1, vjust = -16),
        axis.ticks = element_line(color = 'Grey 60'),
        axis.ticks.length = unit(0, "cm")) + 
  annotate('text', x = 13.33, y = 1.01, label = '---------100%', color = 'grey 50', size = 4)


title_default(title_segments = c('What percent of these nationalities are comprised of women?', ''),
              colors = c('Grey40', 'grey40')) 






# Population Pyramids-----------------------------------------------------------------------------------------------------
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

gganimate(bar_nationality)


ggdraw(a) + draw_plot(temp1, x = -0.25, y = -0.25, scale = .5)

ggplot(pop) + geom_point(aes(x = age_range, y = population)) + theme_base()

#overall pop dist-----------------------------------------------------------------------------------
overall_pop_dist <- pop_pyr_format(pop) %>% 
  ggplot(aes(x = age_range, y = prop, fill = sex)) +  
  geom_bar(stat = 'identity', width = .9) +   
  geom_text(aes(x = age_range, y = 0, label = age_range), 
            color = 'white', size = 4.5, nudge_x = .05) + 
  coord_flip(ylim = c(-.1, .1)) +
  scale_y_continuous(breaks = seq(-.1, .1, .02), 
                     labels = paste0(as.character(c(seq(10, 0, -2), seq(2, 10, 2))), '%')) +
  scale_fill_manual(values = c(men.scale[5], women.scale[5])) + 
  theme_pub(axis_line = TRUE) + 
  labs(x = 'Age Range\n', y = '\nPopulation') +
  theme(panel.grid.major = element_line(size = .75),
        axis.text = element_text(size = 8.35)) +
  no_major_y_gridlines() +
  no_legend() + 
  no_y_text() + 
  no_y_line() + 
  no_y_ticks() +
  annotate(geom = 'text', x = 14.2, y = -.0725, 
           label = 'Female', color = women.scale[[4]], size = 3.5) +
  annotate(geom = 'text', x = 14.2, y = .065, 
           label = 'Male', color = men.scale[[4]], size = 3.5) 

title_default(title_segments = c('Hong Kong Population Pyramid'),
              colors = c('Grey25'))


#annotate to add mean ages of men and women?


#Diverging Bars-------------------------------------------------------------------------------------------------------
#how many women does each nationality contribute to this age_range?
temp1 <- pop %>%
  filter(age_range == '20 - 24' | age_range == '25 - 29'|
           age_range == '30 - 34' | age_range == '35 - 39'|
           age_range == '40 - 44' | age_range == '45 - 49'|
           age_range == '50 - 54' | age_range == '55 - 59') %>%
  group_by(nationality, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  filter(sex == 'male') 

temp2 <- pop %>% 
  filter(age_range == '20 - 24' | age_range == '25 - 29'|
           age_range == '30 - 34' | age_range == '35 - 39'|
           age_range == '40 - 44' | age_range == '45 - 49'|
           age_range == '50 - 54' | age_range == '55 - 59') %>%
  group_by(nationality, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  filter(sex == 'female')

temp2$population <- temp2$population - temp1$population


temp2 %>% 
  mutate(condition = population > 0) %>%
  mutate(condition2 = population > 20000 & population < 190000) %>% 
  ggplot(aes(x = fct_reorder(nationality, population), y = population, label = population)) + 
  geom_point(stat = 'identity', color = men.scale[6], size = 2)  +
  geom_segment(aes(y = 0, x = nationality, yend = population, xend = nationality), 
               color = men.scale[6]) +
  geom_segment(aes(alpha = condition, y = 0, x = nationality, yend = population, xend = nationality), 
               color = women.scale[3]) +
  geom_segment(aes(alpha = condition2, y = 0, x = nationality, yend = population, xend = nationality), 
               color = women.scale[6]) +
  geom_point(aes(alpha = condition), stat = 'identity', color = women.scale[4], size = 2)  +
  geom_point(aes(alpha = condition2), stat = 'identity', color = women.scale[7], size = 3)  +
  labs(x = 'Nationalities\n', y = '\nNumber of excess men or women') + 
  coord_flip() + 
  no_legend() 

temp2 <- title_default(title_segments = c('Numerical difference between men and women', ''),
              colors = c('Grey27', 'grey27')) 

plot_grid(temp2, pop_by_nationality, pop_by_nation, ncol = 1, nrow = 3)



# Only Helpers-------------------------------------------------------------------------------------------------------------------
without_helpers <- read_csv("withouthelpers-preclean.csv", #this data is by ethnicity
                            na = '-') %>% 
  gather(Chinese:Multiracial, key = 'ethnicity', value = 'population')

with_helpers <- read_csv("withhelpers-preclean.csv", 
                         na = '-') %>% 
  gather(Chinese:Multiracial, key = 'ethnicity', value = 'population')

only_helpers <- with_helpers %>% 
  mutate(population = with_helpers$population - without_helpers$population) %>% 
  filter(population != 0)

sum(only_helpers$population, na.rm = TRUE) #domestic helpers in Hong Kong
sum(only_helpers$population, na.rm = TRUE) / sum(pop$population, na.rm = TRUE) #percent of total pop
sum(filter(pop, age_range == '0 - 4' | age_range == '5 - 9')$population, na.rm = TRUE) / sum(only_helpers$population, na.rm = TRUE)
# 4.38% dom helpers, 9.41% immigrants

only_helpers %>% 
  group_by(sex, ethnicity) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>%
  #mutate(prop = population / sum(population)) %>% 
  ggplot(aes(x = sex, y = population)) +
  geom_col() + 
  geom_text(aes(label = population), vjust = -1) + 
  facet_wrap(~ethnicity, scale = 'free_y') 

only_helpers %>%  #male helpers
  group_by(sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>%
  mutate(prop = population / sum(population)) %>% 
  ggplot(aes(x = sex, y = prop)) +
  geom_col() + 
  geom_text(aes(label = prop), vjust = -1) 


only_helpers %>% #male helpers
  group_by(ethnicity) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  ggplot(aes(fct_reorder(ethnicity, prop), prop)) +
  geom_col() + 
  geom_text(aes(label = percent(prop)), vjust = -1)

sum(filter(pop, nationality == 'Chinese (HK Resident)' & sex == 'male' & (age_range == '20 - 24' | age_range == '25 - 29'| age_range == '30 - 34' | age_range == '35 - 39'| age_range == '40 - 44' | age_range == '45 - 49'| age_range == '50 - 54' | age_range == '55 - 59'))$population, na.rm = TRUE) / sum(filter(pop, nationality == 'Chinese (HK Resident)'& (age_range == '20 - 24' | age_range == '25 - 29'| age_range == '30 - 34' | age_range == '35 - 39'| age_range == '40 - 44' | age_range == '45 - 49'| age_range == '50 - 54' | age_range == '55 - 59'))$population, na.rm = TRUE)

pop %>% 
  filter(nationality == 'Chinese (HK Resident)' & (age_range == '20 - 24' | age_range == '25 - 29'| age_range == '30 - 34' | age_range == '35 - 39'| age_range == '40 - 44' | age_range == '45 - 49'| age_range == '50 - 54' | age_range == '55 - 59'))

#3.3% of the 56 to 44 number comes from domestic helpers
sum(filter(with_helpers, sex == 'male' & (age_range == '20 - 24' | age_range == '25 - 29'| age_range == '30 - 34' | age_range == '35 - 39'| age_range == '40 - 44' | age_range == '45 - 49'| age_range == '50 - 54' | age_range == '55 - 59'))$population, na.rm = TRUE) / sum(filter(with_helpers, (age_range == '20 - 24' | age_range == '25 - 29'| age_range == '30 - 34' | age_range == '35 - 39'| age_range == '40 - 44' | age_range == '45 - 49'| age_range == '50 - 54' | age_range == '55 - 59'))$population, na.rm = TRUE)



# Misc-------------------------------------------------------------------------------------------------------------

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
  
  
  