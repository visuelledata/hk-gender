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

pop$age_range <- factor(pop$age_range, levels = unique(pop$age_range))

pop <- pop %>% 
  gather(chinese_hk_resident:Others, key = 'nationality', value = 'population') %>% 
  mutate(prop = population / sum(population, na.rm = TRUE))



annotate('text', x = 3, y = .03, parse = T,label = '"I " * phantom("go to school today")', color = NA) + 
  annotate('text', x = 3, y = .03, parse = T,label = 'phantom("I ") * "go" * phantom("to school today")', color = 'Blue') +
  annotate('text', x = 3, y = .03, parse = T,label = 'phantom("I go ") * "to" * phantom("school today")', color = 'Green') +
  annotate('text', x = 3, y = .03, parse = T,label = 'phantom("I go to ") * "school" * phantom("today")', color = 'Purple') +
  annotate('text', x = 3, y = .03, parse = T,label = 'phantom("I go to school ") * "today"', color = 'Orange') +
  temp(labels = c('bob', 'builder'), colors = c('red', 'blue'))



pop %>%  
  filter(age_range != '0 - 4' & age_range != '80 - 84' & age_range != '85+') %>%
  group_by(age_range, sex, nationality) %>%
  summarize(population = sum(population),
            prop = sum(prop)) %>% 
  ggplot() + 
  geom_col(aes(x = age_range, y = prop, fill = sex), position = 'dodge') +
  annotate_color(x = 3, y = .05,
                 labels = 'I go to school today and have fun', 
                 colors = c('blue', 'red', 'green', 'pink', 'grey', 'orange', 'pink', 'grey'), size = 6, angle = 20)


#label_maker created with help of https://stackoverflow.com/users/3521006/docendo-discimus
annotate_color <- function(geom = 'text', x = NULL, y = NULL, xmin = NULL, xmax = NULL,  
                           ymin = NULL, ymax = NULL, xend = NULL, yend = NULL, ...,
                           labels = NULL, colors = NULL, default_color = 'black'){
  labels <- strsplit(labels, " ")[[1]]
  n <- length(labels)
  
  if (length(colors) < length(labels)){
    colors <- map_chr(seq_len(length(labels)), function(i){
      if (is.na(colors[i])){
        colors[i] <- default_color
      } else {colors[i] <- colors [i]}
    })}
  if (length(colors) > length(labels)){
    colors = colors[1:length(labels)]
    warning('The length of the colors arg is longer than the number of words in the labels arg. Extra colors will be ignored.')
  }
  
   #labelmaker
    labels <- map_chr(seq_len(n), function(i) {
      start0 <- labels[seq_along(labels) < i]
      mid0 <- labels[i]
      end0 <- labels[seq_along(labels) > i]
      start <- paste0('phantom("', paste(start0, collapse = " "), ' ")')
      end <- paste0('phantom("', paste(end0, collapse = " "), '")')
      if(length(start0) > 0 && length(end0) > 0) {
        paste(start, paste0('"', paste(mid0, collapse = " "), '"'), end, sep = ' * ')
      } else if (length(end0) > 0) {
        paste(paste0('"', paste(mid0, collapse = " "), '"'), end, sep = ' * ')
      } else if (length(start0) > 0) {
        paste(start, paste0('"', paste(mid0, collapse = " "), '"'), sep = ' * ')
      } else {
        stop("couldn't finish ...")
      }
    })   
  
  annos <- list()
  #annotation_maker
  annos <- map2(labels, colors, function(annolabel, annotext){
    annos[seq_along(annolabel)] <- list(annotate(geom, x, y, xmin, xmax, ymin, ymax, xend, yend, ...,
                                         parse = T, label = annolabel, color = annotext))
  })
  return(annos) #returns a list of annotation functions
}

