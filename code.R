library(tidyverse)


pop <- read_csv("D:/rproj/hk-gender/preclean-excel.csv", 
                                  na = '-',
                                  col_types = cols(Australian = col_integer(), 
                                                   Indonesian = col_integer(), 
                                                   Japanese = col_integer(), 
                                                   Nepalese = col_integer(), 
                                                   Pakistani = col_integer(), 
                                                   Thai = col_integer()))
