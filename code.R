library(tidyverse)
library(forcats)
library(SciencesPo)
library(scales)
library(stringr)
library(grid)
library(gridExtra)
library(cowplot)

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

title_align_no_clip <- function(plot = last_plot(), title_segments, colors, 
                                filename = NULL, save_width = NA, save_height = NA, 
                                axis_title_color = 'Grey40',
                                axis_x_line_color = 'Grey55',
                                axis_y_line_color = 'Grey55',
                                axis_text_color = 'Grey48',
                                tick_color = 'Grey55'){
  
  if (is.null(title_segments) || is.null(colors)){
    stop('Missing one of the arguments: labels, colors, x, or y')}
  
  # Preformat the graph for the title
  plot = plot + theme(plot.margin = unit(c(.9, 1, 1, 1.2), "cm"),   
                      axis.title = element_text(color = axis_title_color, size = 14),
                      axis.title.y = element_text(hjust = .94),
                      axis.line.x = element_line(color = axis_x_line_color),
                      axis.line.y = element_line(color = axis_y_line_color),
                      axis.text = element_text(color = axis_text_color),
                      axis.ticks = element_line(color = tick_color))
  
  # Create a set of grobs
  grobs <- grobTree(  
    gp = gpar(fontsize = 14, fontface = 'bold'),
    
    textGrob(label = title_segments[1], name = "title1",  
             x = unit(2.33, "lines"), y = unit(-.5, "lines"), 
             hjust = 0, vjust = 0, 
             gp = gpar(col = colors[1])),
    
    if(length(title_segments) > 1){
      textGrob(label = title_segments[2], name = "title2",
               x = grobWidth("title1") + unit(2.24, "lines"), 
               y = unit(-.5, "lines"),
               hjust = 0, vjust = 0, gp = gpar(col = colors[2]))
    },
    
    if(length(title_segments) > 2){
      textGrob(label = title_segments[3], name = "title3",
               x = grobWidth("title1") + grobWidth("title2") + unit(2.24, "lines"), 
               y = unit(-.5, "lines"),
               hjust = 0, vjust = 0, gp = gpar(col = colors[3]))
    },
    if(length(title_segments) > 3){
      textGrob(label = title_segments[4], name = "title4",
               x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") +  unit(2.24, "lines"), 
               y = unit(-.5, "lines"),
               hjust = 0, vjust = 0, gp = gpar(col = colors[4]))
    },
    if(length(title_segments) > 4){
      textGrob(label = title_segments[5], name = "title5",
               x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + grobWidth("title4") + unit(2.24, "lines"), 
               y = unit(-.5, "lines"),
               hjust = 0, vjust = 0, gp = gpar(col = colors[5]))
    }
  )
  
  # Turn off clipping and draw plot
  gb <- ggplot_build(plot) # Puts plot into a usable format
  gt <- ggplot_gtable(gb) # puts grobs into a table to allow the object to be manipulated
  gt$layout$clip[gt$layout$name=="panel"] <- "off" # removes clipping with graph borders 
  gg <- arrangeGrob(gt, top = grobs, padding = unit(2.6, "line")) # places above grobs into a gtable
  grid.newpage() # makes a blank graphing window for ggplot2
  grid.draw(gg) # draws the plot
  
  # Save the plot
  if(!is.null(filename)) ggsave(filename, plot = gg, width = save_width, height = save_height )
}


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

        #standardize all y axes to match and allow for easier reading

# population by age and gender
pop %>%   #consider putting next to other plots so 
  group_by(age_range, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
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
  
title_align_no_clip(title_segments = c('Percent of ', 'men', ' and ', 'women ', 'within each age group'),
                    colors = c('Grey25', men.scale[4], 'Grey25', women.scale[4], 'Grey25'),
                    axis_title_color = 'Grey40', 
                    axis_text_color =  'Grey40',
                    axis_x_line_color = 'Grey35',
                    axis_y_line_color = 'Grey35', 
                    tick_color = 'Grey35')



#highlight ends
pop %>%  
  group_by(age_range, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
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
  theme(axis.text.x = element_text(angle = 80, vjust = .2, hjust = .2)) + 
  no_legend()
#add annotations on plot

title_align_no_clip(title_segments = c('These data points are caused by ', 'birth', ' and ', 'life expectancy ', 'phenomena.'),
                    colors = c('Grey40', 'Grey20', 'Grey40', 'Grey20', 'Grey40'),
                    axis_title_color = 'Grey40', 
                    axis_text_color =  'Grey40',
                    axis_x_line_color = 'Grey35',
                    axis_y_line_color = 'Grey35', 
                    tick_color = 'Grey35')

#highlight target
grouped <- pop %>%  
  group_by(age_range, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population))

pop %>%  
  group_by(age_range, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
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
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = '\nAge range', y = 'Percent within age group\n') + 
  scale_color_manual(values = c(men.scale[5], women.scale[5])) +
  scale_y_continuous(limits = c(0, .7),expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 80, vjust = .2, hjust = .2)) + 
  no_legend()

title_align_no_clip(title_segments = c('From now on, ', 'this data ', 'will be our ', 'focus'),
                    colors = c('Grey40', 'Grey30', 'Grey40', 'Grey30'),
                    filename = 'highlight_target.jpeg',
                    axis_title_color = 'Grey40', 
                    axis_text_color =  'Grey40',
                    axis_x_line_color = 'Grey35',
                    axis_y_line_color = 'Grey35', 
                    tick_color = 'Grey35')


pop %>%
  filter(age_range == '20 - 24' | age_range == '25 - 29'|
         age_range == '30 - 34' | age_range == '35 - 39'|
         age_range == '40 - 44' | age_range == '45 - 49'|
         age_range == '50 - 54' | age_range == '55 - 59') %>% 
  group_by(sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  ggplot(aes(x = sex, y = prop)) + 
  geom_col() + 
  geom_text(aes(label = percent(prop)), 
            color = 'white', vjust = 1.5) 


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

pop_by_nation <- pop %>% 
  group_by(nationality, sex) %>% 
  summarize(population = sum(population, na.rm = TRUE)) %>% 
  mutate(prop = population / sum(population)) %>% 
  mutate(condition = nationality == 'Filipino' | 
                     nationality == 'Indonesian' |
                     nationality == 'Chinese (Temp Resident)' | 
                     nationality == 'British' | 
                     nationality == 'Indian') %>% 
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
  theme(axis.text = element_text(angle = 30, hjust = 1.1, vjust = -16),
        axis.ticks = element_line(color = 'Grey 60'),
        axis.ticks.length = unit(0, "cm")) + 
  annotate('text', x = 13.33, y = 1.01, label = '---------100%', color = 'grey 50', size = 4)

title_align_no_clip(plot = pop_by_nation, 
                    title_segments = c('4 of the 5 largest nationalities are dominated by ', 'Women'),
                    colors = c('Grey25', women.scale[5]),
                    filename = 'bar_nation.jpeg')



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

gganimate(bar_nationality, 'bars.gif')


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

title_align_no_clip(plot = overall_pop_dist, 
                    title_segments = c('Hong Kong Population Pyramid'),
                    colors = c('Grey25'),
                    filename = 'overall_pop_dist.jpeg')


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
  ggplot(aes(x = fct_reorder(nationality, population), y = population, label = population)) + 
  geom_point(stat = 'identity', fill = "black", size = 1)  +
  geom_segment(aes(y = 0, x = nationality, yend = population, xend = nationality), 
               color = "black") +
  labs(title = "The number of women minus the number of men by nationality") + 
  coord_flip()

ggsave('women-men.jpeg', plot = last_plot())
#group negatives together? reorder from inc to dec


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
  
  
  