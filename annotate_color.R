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


pop %>%  
  filter(age_range != '0 - 4' & age_range != '80 - 84' & age_range != '85+') %>%
  group_by(age_range, sex, nationality) %>%
  summarize(population = sum(population),
            prop = sum(prop)) %>% 
  ggplot() + 
  geom_col(aes(x = age_range, y = prop, fill = sex), position = 'dodge') +
  annotate_color('I go to school today and have fun', c('blue', 'red', 'green', 'pink', 'grey', 'orange', 'purple', 'red'))


annotate('text', x = 3, y = .03, parse = T,label = '"I " * phantom("go to school today")', color = NA) + 
  annotate('text', x = 3, y = .03, parse = T,label = 'phantom("I ") * "go" * phantom("to school today")', color = 'Blue') +
  annotate('text', x = 3, y = .03, parse = T,label = 'phantom("I go ") * "to" * phantom("school today")', color = 'Green') +
  annotate('text', x = 3, y = .03, parse = T,label = 'phantom("I go to ") * "school" * phantom("today")', color = 'Purple') +
  annotate('text', x = 3, y = .03, parse = T,label = 'phantom("I go to school ") * "today"', color = 'Orange') +
  temp(labels = c('bob', 'builder'), colors = c('red', 'blue'))



#add more fields
#font size, font face?
annotate_color <- function(labels = NULL, colors = NULL){
  label_maker <- function(labels){ #label_maker created with help of https://stackoverflow.com/users/3521006/docendo-discimus
    x <- strsplit(labels, " ")[[1]]
    n <- length(x)
    
    map_chr(seq_len(n), function(i) {
      start0 <- x[seq_along(x) < i]
      mid0 <- x[i]
      end0 <- x[seq_along(x) > i]
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
  }
  labels <- label_maker(labels)
  annos <- list()
  for (i in seq_along(labels)){
    annos[i] <- list(annotate('text', x = 3, y = .03, parse = T, label = labels[i], color = colors[i]))
  }
  return(annos)
}
