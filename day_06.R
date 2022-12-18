library(tidyverse)
library(magrittr)


source('AoC functions.R')
source('.Rprofile')

download_advent(2022,
                6,
                funct = readr::read_lines)


input %<>% 
  tibble(string = .) %>% 
  mutate(c1 = str_sub(string, 1,1),
         c2 = str_sub(string, 2,2),
         c3 = str_sub(string, 3,3),
         c4 = str_sub(string, 4,4),
         
         flag = ifelse(n_distinct(c_across(c1:c4)) == 4,
                       1,
                       0),
         count = 1)


while(input$flag[1] == 0){
  input %<>% 
    mutate(string = str_sub(string,
                            2,
                            -1),
           c1 = str_sub(string, 1,1),
           c2 = str_sub(string, 2,2),
           c3 = str_sub(string, 3,3),
           c4 = str_sub(string, 4,4),
           
           flag = ifelse(n_distinct(c_across(c1:c4)) == 4,
                         1,
                         0),
           count = count + 1)
}

input$count + 3



# Part 2 ------------------------------------------------------------------
num = 14

download_advent(2022,
                6,
                funct = readr::read_lines)

input %<>% 
  tibble(string = .) %>% 
  mutate(c = str_split(str_sub(string,
                               1,
                               num),
                       '')) %>% 
  unnest_wider(c, names_sep = '_') %>% 
  mutate(flag = ifelse(n_distinct(c_across(c_1:c_14)) == num,
                       1,
                       0),
         count = 1)

while(input$flag[1] == 0){
  input %<>% 
    select(-c(c_1:c_14)) %>% 
    mutate(count = count + 1,
           string = str_sub(string,
                            2,
                            -1),
           
           c = str_split(str_sub(string,
                                 1,
                                 num),
                         '')) %>% 
    unnest_wider(c,
                 names_sep = '_') %>% 
    mutate( flag = ifelse(n_distinct(c_across(c_1:c_14)) == num,
                          1,
                          0))
}


input$count + 13
